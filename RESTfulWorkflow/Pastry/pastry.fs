module Pastry

// Created by Jakob Lautrup Nysom 2015-04-24
open System
open System.Net
open System.Threading

open PastryTypes
open PastryUtils

// ====================== CONFIG ======================

let SEPARATOR = " SEPARATOR " // This is silly, but safe...

// ====================================================

// ============ TYPES FOR THE INTERFACE ===============

// The main state of pastry
type PastryState<'a> = {
    node: Node; // Because nodes change during routing
    data: 'a;   // The data of the application
}

// Updated state, response, status code
type ResourceResponse<'a> = {
    state: PastryState<'a>
    message: string;    // Http response string
    status: int;        // Http status code
}

// A function for the resource request func to send requests through
// partial_resource_url, method, data, state -> response
type SendFunc<'a> = string -> string -> string -> PastryState<'a> -> ResourceResponse<'a>

// A function to handle resource requests
// url, method, data, send_func, state -> response
type ResourceRequestFunc<'a> = string -> string -> string -> SendFunc<'a> -> PastryState<'a> -> ResourceResponse<'a>

// A function to serialize the state passed through pastry
type SerializeFunc<'a> = 'a -> string

// A record used for passing the current application environment easily through
// functions
type PastryEnv<'a> = {
    send: SendFunc<'a>;
    handle: ResourceRequestFunc<'a>;
    serialize: SerializeFunc<'a>;
    state: PastryState<'a>;
}

// I need this for circular references /o/
type RouteFunc<'a> = PastryEnv<'a> -> MessageType -> string -> GUID -> ResourceResponse<'a>

// ====================================================

// A simple constructor for resource responses
let resource_response<'a> (state: PastryState<'a>) (message: string)
        (status: int) : ResourceResponse<'a> =
    { state = state; message = message; status = status; }

// Returns whether the given resource belongs on this pastry node or another
let belongs_on_other (self_guid: string) (resource: string) (other_guid: string) : bool =
    let (self, other) =
        match deserialize_guid self_guid, deserialize_guid other_guid with
        | Some(a), Some(b) -> a, b
        | _ -> failwith (sprintf "Invalid guids given: %s %s" self_guid other_guid)
    let path = split resource '/'
    match get_destination path with
    | Ok(guid) ->
        distance self guid < distance other guid
    | Error(_) ->
        failwith "Bad resource url: '%s'" resource

// Sends a backup state to the nodes that are watching this node
let send_backup_state (node: Node) (state_msg: string) =
    let closer_than k acc =
        distance k node.guid < distance acc node.guid

    let folder k _ acc =
        if (valid_min_leaf node k) && (closer_than k acc) then k
        else acc

    let neighbor = Map.foldBack folder node.leaves node.minleaf
    if neighbor = node.guid then // No smaller leaves
        // Send it to the highest leaf
        printfn "BACKUP: No other leaves, backup delayed..."
    else
        let address =
            match Map.tryFind neighbor node.leaves with
            | Some(addr) ->
                addr
            | None ->
                failwith "ASSERTION FAILED: Smallest key not found in leaves... WTF!"
        match send_message address Backup state_msg neighbor <| Some(100) with
        | HttpResult.Ok(msg) ->
            printfn "BACKUP: Backed up succesfully! (%s)" msg
        | HttpResult.ConnectionError(msg) ->
            printfn "BACKUP: Connection Error!"
        | HttpResult.Error(msg, status) ->
            printfn "BACKUP: No reply for backup: Handle node failure? (%d %s)" status msg

// Handles a pastry join request (this node getting the things needed to join)
let handle_join (node: Node) (message: string): Node =
    let string_states = message.Split([|SEPARATOR|], StringSplitOptions.RemoveEmptyEntries)
    let states = Array.foldBack (fun str acc -> (deserialize str)::acc) string_states []

    let rec last = function
        | head::[] -> head
        | head::tail -> last tail
        | _ -> failwith "Unreachable scenario found /o/"

    let A = List.head states    // Physically closest to this node
    let Z = last states         // GUID-wise  closest to this node

    // Get this node's neighbors (the nodes physically closest to this one)
    let nfolder k v (acc: Map<GUID, NetworkLocation>) =
        if acc.Count >= MAX_LEAVES then acc else Map.add k v acc
    let neighbors = Map.foldBack nfolder A.neighbors (Map.ofList [(A.guid, A.address)])

    // Get this node's leaves (the nodes with MAX_LEAVES/2 smaller and bigger GUIDs)
    let leaves =
        let inner_leaves =
            if Z.leaves.Count < MAX_LEAVES then
                Z.leaves
            else
                // The minimum leaf is farther from this node than the maximum
                if (distance node.guid Z.minleaf) > (distance node.guid Z.maxleaf) then
                    Map.remove Z.minleaf Z.leaves
                else
                    Map.remove Z.maxleaf Z.leaves
        Map.add Z.guid Z.address inner_leaves

    let lfolder k _ (acc: GUID * GUID) =
        let (minleaf, maxleaf) = acc
        if k < minleaf then
            (k, maxleaf)
        else if k > maxleaf then
            (minleaf, k)
        else acc
    let (minleaf, maxleaf) = Map.foldBack lfolder leaves (node.guid, node.guid)

    printfn "Neighbors of %A: %A" node.guid neighbors
    printfn "Leaves    of %A: %A" node.guid leaves
    let updated_node =
        { node with
            neighbors = Map.empty;//neighbors; // UNUSED
            leaves = leaves;
            minleaf = minleaf;
            maxleaf = maxleaf;
        }
    let notify guid address =
        match send_message address Update (serialize updated_node) guid None with
        | HttpResult.Ok(_) ->
            ()
        | HttpResult.ConnectionError(msg) ->
            printfn "JOIN: Could not connect to %s when notifying of join: %s" (string guid) msg
        | HttpResult.Error(msg, status) ->
            printfn "JOIN: Failed to notify %s at '%s' of update: %d %s" (string guid) address status msg
    Map.iter notify updated_node.leaves
    //Map.iter notify updated_node.neighbors // We don't remove these properly, so...
    List.iter (fun map_level -> Map.iter notify map_level) updated_node.routing_table
    printfn "JOIN: State after joining: %A" updated_node
    updated_node

// Handles that the node closest to this on the 'bigger' side has died
let migrate_dead_leaf<'a> (env: PastryEnv<'a>) (neighbor: GUID): PastryState<'a> =
    let response = env.handle "migrate" "PUT" env.state.node.backup env.send env.state
    response.state

// Handles that a node in the leaf set has failed
let handle_dead_leaf<'a> (env: PastryEnv<'a>) (leaf: GUID) (route_func: RouteFunc<'a>)
        : PastryState<'a> =
    printfn "DEAD: Removing dead leaf %s from leaf set" (serialize_guid leaf)
    let node_with_leaves = remove_leaf env.state.node leaf
    // Find out what to do with the death
    let watched = find_watched_neighbor env.state.node
    //printfn "%s" <| String.replicate 50 "="
    //printfn "Watched neighbor of %s: %s" (string node.guid) (string watched)
    //printfn "min, max, leaves: %s | %s | %A" (string node.minleaf) (string node.maxleaf) node.leaves
    //printfn "%s" <| String.replicate 50 "="

    let new_env =
        // This node is supposed to handle it
        let new_env = { env with state = { env.state with node = node_with_leaves;}}
        if leaf = watched then // IMPORTANT original node
            { new_env with state = migrate_dead_leaf new_env leaf; }
        else // Send it elsewhere. Update the node in case other leaves have failed
            let response = route_func new_env DeadNode (serialize_guid leaf) leaf
            { new_env with state = response.state; }

    // Find the node that might have a replacement leaf
    let get_folder predicate =
        let folder k _ acc =
            if (predicate k) && (distance new_env.state.node.guid k > distance new_env.state.node.guid acc) then k
            else acc
        folder

    let predicate = // Checks if it would be a leaf of the old state
        if valid_min_leaf new_env.state.node leaf then valid_min_leaf new_env.state.node
        else valid_max_leaf new_env.state.node

    let end_leaf = Map.foldBack (get_folder predicate) new_env.state.node.leaves new_env.state.node.guid
    if end_leaf = new_env.state.node.guid then
        if valid_min_leaf env.state.node leaf then
            // No other min leaves, therefore min is max
            let new_node: Node = { new_env.state.node with minleaf = new_env.state.node.maxleaf; }
            { new_env.state with node = new_node; }
        else
            // No other max leaves, therefore max is min
            let new_node = { new_env.state.node with maxleaf = new_env.state.node.minleaf; }
            { new_env.state with node = new_node; }
    else
        // Ask it for its state
        let response = route_func new_env GetState "" end_leaf
        let final_env = { new_env with state = response.state; }
        let state = deserialize response.message

        // Get the replacement leaves if any
        let update_folder guid addr new_node =
            if not (guid = leaf) then try_add_leaf new_node guid addr
            else new_node
        let new_node = Map.foldBack update_folder state.leaves final_env.state.node
        { final_env.state with node = new_node; }

// Updates this node based on a new node that is joining the network
let update_node<'a> (env: PastryEnv<'a>) (new_node: Node) : PastryState<'a> =
    let node_with_leaves = try_add_leaf env.state.node new_node.guid new_node.address
    //try_add_neighbor updated_leaves new_node.guid new_node.address

    // Find out whether this node is a close leaf (1 smaller or larger)
    let is_left_leaf = (new_node.guid = find_left_leaf node_with_leaves)
    let is_right_leaf = (new_node.guid = find_right_leaf node_with_leaves)
    if is_left_leaf || is_right_leaf then
        let own_id = serialize_guid env.state.node.guid
        let other_id = serialize_guid new_node.guid
        printfn "MIGRATE: The new node %s is my neighbor!" other_id
        let path = sprintf "migrate/%s/%s" own_id other_id
        let new_env = { env with state = { env.state with node = node_with_leaves;}}
        let response = new_env.handle path "PUT" "" new_env.send new_env.state
        response.state
    else
        { env.state with node = node_with_leaves; }

// Handles a message intended for this node
let handle_message<'a> (env: PastryEnv<'a>) (typ: MessageType) (message: string)
        (key: GUID) (route_func: RouteFunc<'a>) : ResourceResponse<'a> =
    if not (typ = Backup) then
        printfn "HANDLE: | Handling '%A' | '%s'" typ message
    //printfn "HANDLE: | '%s'" message
    match typ with
    | Join -> // A node is requesting to join, and this is the one with the nearest GUID
        let firstsep = message.IndexOf(SEPARATOR)
        let address = message.[..firstsep-1]
        let states = message.[firstsep + SEPARATOR.Length..]
        match send_message address JoinState states key None with
        | HttpResult.Ok(message) ->
            printfn "Node %s sent of sates succesfully" (serialize_guid key)
        | HttpResult.Error(message, status) ->
            printfn "Error for %s when sending join state batch: %d | %s" (serialize_guid key) status message
        | HttpResult.ConnectionError(msg) ->
            printfn "Connection error for the newly joined node %s: %s" (string key) msg

        resource_response env.state "" 200

    | JoinState -> // This node receives the states needed to initialize itself
        let updated_node = handle_join env.state.node message
        resource_response { env.state with node = updated_node;} "Joined" 200

    | Update -> // The node is notified of a new node joining
        let updated_state = update_node env <| deserialize message
        //printfn "PASTRY: State after update: %A" updated_node
        resource_response updated_state "Updated" 200

    | Resource(path, meth) -> // Request for a resource here
        //printfn "PASTRY: Requesting resource at '%s' using '%s'" path meth
        env.handle path meth message env.send env.state

    | DeadNode -> // Something has died
        printfn "DEAD: Notified that %s has died" (serialize_guid key)
        let dead =
            match deserialize_guid message with
            | Some(guid) ->
                guid
            | None ->
                failwith "Could not deserialize dead node GUID %s" message

        let watched = find_watched_neighbor env.state.node
        //printfn "%s" <| String.replicate 50 "="
        //printfn "Watched neighbor of %s: %s" (string node.guid) (string watched)
        //printfn "min, max, leaves: %s | %s | %A" (string node.minleaf) (string node.maxleaf) node.leaves
        //printfn "%s" <| String.replicate 50 "="

        if dead = watched then
            printfn "DEAD: It's the one I'm watching!"
            match Map.tryFind dead env.state.node.leaves with
            | Some(_) ->
                let updated_state = handle_dead_leaf env dead route_func
                resource_response updated_state "Handled!" 200
            | None ->
                printfn "DEAD: Already handled..."
                resource_response env.state "Already handled" 200
        else
            // Route it a little to the left
            let closer dst a b =
                (distance a dst) < (distance b dst)

            let folder leaf_guid _ current_closest =
                let minleaf = valid_min_leaf env.state.node leaf_guid
                let notdead = not (leaf_guid = dead)
                let closer_to_node = closer env.state.node.guid leaf_guid current_closest
                if minleaf && notdead && closer_to_node then
                    leaf_guid
                else
                    current_closest
            let dst = Map.foldBack folder env.state.node.leaves env.state.node.minleaf
            printfn "DEAD: Forwarding 'dead' call to %s" (serialize_guid dst)
            route_func env DeadNode message dst
    | Backup ->
        printfn "BACKUP: Backup received!"
        let new_node = { env.state.node with backup = message; }
        resource_response { env.state with node = new_node;} "Backed up!" 200

    | GetState | Ping ->
        failwith "GetState and Ping messages are handled elsewhere!"

// Routes something using the leaf set of the node
let rec route_leaf (env: PastryEnv<'a>) (typ: MessageType) (msg: string) (key: GUID)
        (route_func: RouteFunc<'a>) : ResourceResponse<'a> =
    let distance_check = fun leafkey _ acc ->
        if distance leafkey key < distance acc key then leafkey else acc
    let closest = Map.foldBack distance_check env.state.node.leaves env.state.node.guid
    if closest = env.state.node.guid then
        handle_message env typ msg key route_func
    else
        let address = Map.find closest env.state.node.leaves
        match send_message address typ msg key None with // No timeout here :u
        | HttpResult.Ok(message) ->
            resource_response env.state message 200
        | HttpResult.Error(message, status) ->
            resource_response env.state message status
        | HttpResult.ConnectionError(msg) -> // HERE BE POSSIBLE DEADLOCKS (yay)
            printfn "ROUTE LEAF: Could not send a message to %s: %s" (string key) msg
            // Fix the leaf
            let new_state = handle_dead_leaf env closest route_func
            let new_env = { env with state = new_state; }
            // Retry
            route_leaf new_env typ msg key route_func

// Routes a message somewhere
let rec route<'a> (env: PastryEnv<'a>) (typ: MessageType) (msg: string) (key: GUID)
        : ResourceResponse<'a> =
    printfn "ROUTE: | Routing '%A'" typ
    printfn "ROUTE: | towards '%A'" key
    let (new_state, message) =
        match typ with // This is actually okay since the nodes contain no strings
        | Join ->
            // NOTE NOTE NOTE ------- remove later!
            // Simulate some latency on localhost

            // If there is an old version in the leaf set
            let new_state =
                if Map.containsKey key env.state.node.leaves then
                    // Make sure it's dead before continuing!
                    printfn "DEAD: Removing old reference before forwarding"
                    handle_dead_leaf env key route
                else
                    env.state

            printfn "Sleeping before forwarding join..."
            Thread.Sleep(1000)
            printfn "Done! Continuing..."
            let msg = sprintf "%s%s%s" msg SEPARATOR (serialize new_state.node)
            new_state, msg
        | _ ->
            env.state, msg

    let new_env = { env with state = new_state; }
    let valid_leaf = (valid_min_leaf env.state.node key) || (valid_max_leaf env.state.node key)
    // Within leaf set
    if valid_leaf || Map.isEmpty env.state.node.leaves || (key = env.state.node.guid) then
        //printfn "PASTRY: Found target within leaf set!"
        route_leaf env typ message key route
    else
        //printfn "PASTRY: Checking routing table..."
        //printfn "PASTRY: Actually just using the leaf set ATM..."
        // Same check as above... for laziness
        route_leaf env typ message key route

// Attempts to forward a message to a pastry node using the given url parts
let try_forward_pastry<'a> (env: PastryEnv<'a>) (cmd_str: string) (dst_str: string)
        (body: string) (response: HttpListenerResponse)
        : ResourceResponse<'a> =
    match deserialize_guid dst_str with
    | Some(guid) ->
        match cmd_str with
        | "join" ->
            reply response "Forward attempted!" 200 "Ok"
            route env Join body guid
        | "update" ->
            reply response "Forward attempted!" 200 "Ok"
            route env Update body guid
        | "joinstate" ->
            reply response "Forward attempted!" 200 "Ok"
            handle_message env JoinState body guid route
        | "backup" ->
            reply response "Forward attempted!" 200 "Ok"
            handle_message env Backup body guid route
        | "ping" ->
            let msg = sprintf "PONG @ %s" (serialize_guid env.state.node.guid)
            reply response msg 200 "Ok"
            resource_response env.state "Ok" 200
        | "getstate" ->
            reply response (serialize env.state.node) 200 "Ok"
            resource_response env.state "Ok" 200
        | "deadnode" ->
            reply response "Handling of death attempted" 200 "Ok"
            handle_message env DeadNode body guid route
        | "debug" ->
            let a = sprintf "==== Pastry state ====\n%A" env.state.node
            let msg = sprintf "%s\n==== Application state ====\n%A" a env.state.data
            reply response msg 200 "Ok"
            resource_response env.state "Ok" 200
        | _ ->
            let error_message = sprintf "Bad pastry command: '%s'" cmd_str
            reply response error_message 400 "Not Found"
            resource_response env.state error_message 400
    | None ->
        let error_message = sprintf "Invalid GUID received: '%s'" dst_str
        reply response error_message 400 "Not Found"
        resource_response env.state error_message 400

// Creates a send func !
let create_send_func<'a> (env: PastryEnv<'a>): SendFunc<'a> =
    let rec send_func (resource_path: string) (meth: string) (data: string)
            (state: PastryState<'a>): ResourceResponse<'a> =
        match get_destination (split resource_path '/') with
        | Ok(guid) ->
            let new_env = { env with send = send_func; state = state; }
            route new_env (Resource(resource_path, meth)) data guid

        | Error(resp, status, reason) ->
            printfn "SEND FUNC: Could not get a destination for '%s': %d %s" resource_path status reason
            resource_response state resp status
    send_func

// Attempts to forward some given url parts
let try_forward_resource<'a> (env: PastryEnv<'a>) (split_res_path: string list)
        (meth: string) (data: string) (response: HttpListenerResponse)
        : ResourceResponse<'a> =
    match get_destination split_res_path with
    | Ok(guid) ->
        // Construct the message function used by the repo to route messages out
        let resource_url = String.concat "/" split_res_path
        let route_response = route env (Resource(resource_url, meth)) data guid
        reply response route_response.message route_response.status "Ok"
        route_response
    | Error(msg, status, reason) ->
        reply response msg status reason
        resource_response env.state msg status

// Pings the closest bigger neighbor of a node to see if it's still alive
let ping_neighbor<'a> (env: PastryEnv<'a>) : PastryState<'a> =
    let neighbor = find_watched_neighbor env.state.node
    printfn "PING: -> %s" (serialize_guid neighbor)
    if neighbor = env.state.node.guid then // No smaller leaves
        // Send it to the highest leaf
        printfn "PING: No other leaves, ping delayed..."
        env.state
    else
        let address =
            match Map.tryFind neighbor env.state.node.leaves with
            | Some(addr) ->
                addr
            | None ->
                failwith "ASSERTION FAILED: Smallest key not found in leaves... WTF!"
        match send_message address Ping "" neighbor None with
        | HttpResult.Ok(_) ->
            env.state
        | HttpResult.Error(msg, status) ->
            printfn "PING: why did it return an error? %d | %s" status msg
            env.state
        | HttpResult.ConnectionError(message) ->
            printfn "PING: Neighbor is dead, do something!"
            handle_dead_leaf env neighbor route

// Makes the given pastry node start listening...
let start_listening<'a when 'a: equality> (env: PastryEnv<'a>) =
    printfn "Initializing..."
    use listener = new System.Net.HttpListener ()
    let basepath = sprintf "http://%s/" env.state.node.address
    printfn "Listening at '%s'..." basepath
    listener.Prefixes.Add basepath
    listener.Start ()

    // Listen for a a message
    let rec listen (old_env: PastryEnv<'a>) =
        printf "> " // Show that pastry is listening and not dead

        // Update the send function!
        let env = { old_env with send = create_send_func old_env; }

        let cxt      = listener.GetContext()
        let request  = cxt.Request
        let response = cxt.Response
        let meth     = request.HttpMethod
        let args     = request.QueryString
        let path     = request.Url.AbsolutePath
        let rip      = request.RemoteEndPoint.Address
        let rport    = request.RemoteEndPoint.Port
        let body =
            use is = new System.IO.StreamReader(request.InputStream, request.ContentEncoding)
            is.ReadToEnd()
        let parts = split (path.[1..]) '/' // That annoying first slash...
        // Now interpret what was received
        let new_state =
            match parts with
            | "pastry"::cmd_string::dst_string::[] ->
                let res_resp = try_forward_pastry env cmd_string dst_string body response
                res_resp.state

            | "pastry"::stuff -> // only handles '/pastry'
                let error_message = sprintf "Unrecognized pastry request url: '%s'" path
                error error_message
                reply response error_message 404 "Not found"
                env.state

            | "resource"::resource_path_parts -> // Someone requests a resource
                // Get the data from a GET request
                let data =
                    if meth = "GET" then
                        let d = args.Get "data"
                        if d = null then ""
                        else d
                    else
                        body

                let res = try_forward_resource env resource_path_parts meth data response
                if not (res.state.data = env.state.data) then // WOW THIS IS UGLY PLEASE MAKE IT STOP!
                    send_backup_state res.state.node <| env.serialize res.state.data
                res.state
            | _ ->
                let error_message = sprintf "Not related to this: %s" path
                error error_message
                reply response error_message 404 "Not found"
                env.state

        listen { env with state = new_state; }

    // Start listening
    ignore <| listen env

// "Actually" starts the server. Used by the interface functions
let start_server_fixed_guid<'a when 'a: equality> (address: NetworkLocation) (peer: NetworkLocation option)
        (handler: ResourceRequestFunc<'a>) (serializer: SerializeFunc<'a>)
        (guid: GUID) (data: 'a) =
    printfn "? Pastry GUID: %s" (serialize_guid guid)
    let node: Node = { // Create a new node
        guid = guid;
        address = address;
        leaves = Map.empty;
        minleaf = guid;
        maxleaf = guid;
        neighbors = Map.empty;
        routing_table = [];
        backup = "";
    }
    let dummy_sender env path meth data =
        failwith "ASSERTION FAILED: Dummy send func not replaced before use!"

    let env: PastryEnv<'a> = { // Create the inter parts
        send = dummy_sender;
        handle = handler;
        serialize = serializer;
        state = { node = node; data = data; };
    }
    match peer with
    | None ->
        start_listening<'a> env
    | Some(peer) ->
        match send_message peer Join address guid None with
        | HttpResult.ConnectionError(msg) ->
            error <| sprintf "Could not establish a connection with peer at '%s': %s" peer msg
        | HttpResult.Error(msg, status) ->
            error <| sprintf "Got an error from peer at '%s': %d | %s" peer status msg
        | HttpResult.Ok(_) ->
            start_listening<'a> env

// Starts a server with a fixed guid in string from
let test_server<'a when 'a: equality> (address: NetworkLocation) (peer: NetworkLocation option)
        (handler: ResourceRequestFunc<'a>) (serializer: SerializeFunc<'a>)
        (guid_str: string) (data: 'a) =
    let guid =
        match deserialize_guid guid_str with
        | Some(id) -> id
        | None -> failwith "Invalid GUID given!"
    start_server_fixed_guid address peer handler serializer guid data

// Creates a local node and makes it join the Pastry network
let start_server<'a when 'a: equality> (address: NetworkLocation) (peer: NetworkLocation option)
        (handler: ResourceRequestFunc<'a>) (serializer: SerializeFunc<'a>)
        (data: 'a) =
    let guid = hash address
    start_server_fixed_guid address peer handler serializer guid data

