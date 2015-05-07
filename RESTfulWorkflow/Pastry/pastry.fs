module Pastry

// Created by Jakob Lautrup Nysom 2015-04-24
open System
open System.Net
open System.Threading

open PastryTypes
open PastryUtils

// ====================== CONFIG ======================

let DIGIT_POWER = 4 // 2^b-1 per row in the routing table
let MAX_LEAVES = 16 // or 32
let SEPARATOR = " SEPARATOR " // This is silly, but safe...

// ====================================================

// ============ TYPES FOR THE INTERFACE ===============

// Updated state, response, status code
type ResourceResponse<'a> = 'a * string * int

// A function for the resource request func to send requests through
// partial_resource_url, method, data, state -> response
type SendFunc<'a> = string -> string -> string -> 'a -> ResourceResponse<'a>

// A function to handle resource requests
// url, method, data, send_func, state -> response
type ResourceRequestFunc<'a> = string -> string -> string -> SendFunc<'a> -> 'a -> ResourceResponse<'a>

// A function to serialize the state passed through pastry
type SerializeFunc<'a> = 'a -> string

// A record containing the types needed to inter properly with the pastry
// network
type PastryInterface<'a> = {
    send: SendFunc<'a> option;
    handle: ResourceRequestFunc<'a>;
    serialize: SerializeFunc<'a>;
    state: 'a;
}

// I need this for circular references /o/
type RouteFunc<'a> = Node -> MessageType -> string -> GUID -> PastryInterface<'a> -> Node * 'a * (string * int)

// ====================================================

// Sends a pastry message to a node at an address, that a type of message must
// be forwarded towards a pastry node, carrying some data
let send_message (address: NetworkLocation) (typ: MessageType) (message: string) (destination: GUID) : (string * int) option =
    try
        match typ with
        | Resource(path, meth) ->
            let url = sprintf "http://%s/resource/%s" address path
            printfn "SEND: %s %s => %s" meth url message
            use w = new System.Net.WebClient ()
            match meth with
            | "GET" ->
                Some((w.DownloadString(url + "?data="+message)), 200)// TODO make safer
            | _ ->
                Some((w.UploadString(url, meth, message)), 200)
        | _ ->
            let cmd =
                match typ with
                | Join          -> "join"
                | JoinState     -> "joinstate"
                | Update        -> "update"
                | Backup        -> "backup"
                | Ping          -> "ping"
                | GetState      -> "getstate"
                | DeadNode      -> "deadnode"
                | Resource(_)   -> failwith "Got past a match on 'Resource'"
            let url = sprintf "http://%s/pastry/%s/%s" address cmd (serialize_guid destination)
            printfn "SEND: %s => %s" url message
            use w = new System.Net.WebClient ()
            Some((w.UploadString(url, "POST", message), 200))
    with
        | ex -> None

// Replies to a http request
let reply (response: HttpListenerResponse) (answer: string) (status: int) (reason: string) =
    printfn " --> %d %s [%s]" status reason answer
    // Set HTTP statuscode and reason (e.g., 200 OK)
    response.StatusCode <- status
    response.StatusDescription <- reason
    // Encode and write body.
    let buffer = System.Text.Encoding.UTF8.GetBytes(answer : string)
    response.ContentLength64 <- int64 buffer.Length;
    response.OutputStream.Write(buffer,0,buffer.Length);
    response.OutputStream.Close();

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
        printfn "PASTRY: No other leaves, backup delayed..."
    else
        let address =
            match Map.tryFind neighbor node.leaves with
            | Some(addr) ->
                addr
            | None ->
                failwith "ASSERTION FAILED: Smallest key not found in leaves... WTF!"
        match send_message address Backup state_msg neighbor with
        | None ->
            printfn "PASTRY: No reply for backup: Handle node failure?"
        | Some(resp, status) ->
            printfn "PASTRY: Backed up succesfully!"

// Adds the given node as a neighbor if it makes sense (I'm not using this ATM)
let try_add_neighbor (node: Node) (guid: GUID) (address: NetworkLocation) : Node =
    if node.neighbors.Count < MAX_LEAVES then
        { node with neighbors = Map.add guid address node.neighbors; }
    else
        node

// Finds the biggest leaf blah blah
let find_max_leaf (node: Node) (leaves: Map<GUID, NetworkLocation>): GUID =
    let folder k _ acc =
        if valid_max_leaf node k then
            if distance k node.guid > distance acc node.guid then k
            else acc
        else acc
    let maxleaf = Map.foldBack folder leaves node.guid
    if maxleaf = node.guid then node.minleaf
    else maxleaf

// Finds the smallest leaf blah blah
let find_min_leaf (node: Node) (leaves: Map<GUID, NetworkLocation>): GUID =
    let folder k _ acc =
        if valid_min_leaf node k then
            if distance k node.guid > distance acc node.guid then k
            else acc
        else acc
    let minleaf = Map.foldBack folder leaves node.guid
    if minleaf = node.guid then node.maxleaf
    else minleaf

// Tries to add the given guid/address pair to the node as a leaf
let try_add_leaf (node: Node) (guid: GUID) (address: NetworkLocation) : Node =
    // Check whether there is an empty spot in the leaf set for this leaf
    if node.leaves.Count < MAX_LEAVES then // Room for more
        let leaves = Map.add guid address node.leaves
        // Find the index of the sorted list of keys that is bigger than the
        // node's guid
        let sorted_leaves = Map.toList leaves |> List.map (fun (k,_) -> k) |> List.sort
        let folder k acc =
            if k < node.guid then acc + 1
            else acc
        let bigger_index = List.foldBack folder sorted_leaves 0
        let min_leaf_index = (bigger_index + (leaves.Count / 2)) % leaves.Count
        let max_leaf_index =
            if min_leaf_index = 0 then leaves.Count - 1
            else min_leaf_index - 1
        let minleaf = List.nth sorted_leaves min_leaf_index
        let maxleaf = List.nth sorted_leaves max_leaf_index
        { node with leaves = leaves; minleaf = minleaf; maxleaf = maxleaf; }
    else
        if (valid_max_leaf node guid) && (not (guid = node.maxleaf)) then
            let leaves = Map.add guid address (Map.remove node.maxleaf node.leaves)
            let maxleaf = find_max_leaf node leaves
            { node with leaves = leaves; maxleaf = maxleaf; }
        else if (valid_min_leaf node guid) && (not (guid = node.minleaf)) then
            let leaves = Map.add guid address (Map.remove node.minleaf node.leaves)
            let minleaf = find_min_leaf node leaves
            { node with leaves = leaves; minleaf = minleaf; }
        else
            node

// Safely removes a leaf and updates the min/max parts
let remove_leaf (node: Node) (leaf: GUID) : Node =
    let leaves = Map.remove leaf node.leaves
    if leaves.Count < MAX_LEAVES then // Room for more
        // Find the index of the sorted list of keys that is bigger than the
        // node's guid
        let sorted_leaves = Map.toList leaves |> List.map (fun (k,_) -> k) |> List.sort
        let folder k acc =
            if k < node.guid then acc + 1
            else acc
        let bigger_index = List.foldBack folder sorted_leaves 0
        let min_leaf_index = (bigger_index + (leaves.Count / 2)) % leaves.Count
        let max_leaf_index =
            if min_leaf_index = 0 then leaves.Count - 1
            else min_leaf_index - 1
        let minleaf = List.nth sorted_leaves min_leaf_index
        let maxleaf = List.nth sorted_leaves max_leaf_index
        { node with leaves = leaves; minleaf = minleaf; maxleaf = maxleaf; }
    else if leaf = node.maxleaf then
        let maxleaf = find_max_leaf node leaves
        { node with leaves = leaves; maxleaf = maxleaf; }
    else if leaf = node.maxleaf then
        let minleaf = find_min_leaf node leaves
        { node with leaves = leaves; minleaf = minleaf; }
    else
        { node with leaves = leaves; }

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
        match send_message address Update (serialize updated_node) guid with
        | Some(_) -> ()
        | None -> printfn "PASTRY: Failed to notify %A at '%s' of update..." guid address
    Map.iter notify updated_node.leaves
    //Map.iter notify updated_node.neighbors // We don't remove these properly, so...
    List.iter (fun map_level -> Map.iter notify map_level) updated_node.routing_table
    printfn "PASTRY: State after joining: %A" updated_node
    updated_node

// Handles that the node closest to this on the 'bigger' side has died
let migrate_dead_leaf<'a> (node: Node) (neighbor: GUID)
        (inter: PastryInterface<'a>) =
    printfn "%s" <| String.replicate 50 "="
    printfn "NOT IMPLEMENTED: Migrating dead neighbor's state..."
    printfn "%s" <| String.replicate 50 "="

// Finds the GUID of the neighbor that this node watches over
let find_watched_neighbor (node: Node) =
    let closer_than k acc =
        distance k node.guid < distance acc node.guid
    let folder k _ acc =
        if (valid_max_leaf node k) && (closer_than k acc) then k
        else acc
    Map.foldBack folder node.leaves node.maxleaf

// Handles that a node in the leaf set has failed
let handle_dead_leaf<'a> (node: Node) (leaf: GUID) (route_func: RouteFunc<'a>)
        (inter: PastryInterface<'a>): Node =
    printfn "PASTRY: Removing dead leaf %s from leaf set" (serialize_guid leaf)
    let leaves = Map.remove leaf node.leaves
    // Find out what to do with the death
    let watched = find_watched_neighbor node
    printfn "%s" <| String.replicate 50 "="
    printfn "Watched neighbor of %s: %s" (string node.guid) (string watched)
    printfn "min, max, leaves: %s | %s | %A" (string node.minleaf) (string node.maxleaf) node.leaves
    printfn "%s" <| String.replicate 50 "="

    let leaf_node = { node with leaves = leaves; }
    let updated_node =
        // This node is supposed to handle it
        if leaf = watched then // IMPORTANT original node
            migrate_dead_leaf leaf_node leaf inter
            leaf_node
        else // Send it elsewhere. Update the node in case other leaves have failed
            let (node', _, _) = route_func leaf_node DeadNode (serialize_guid leaf) leaf inter
            node'

    // Find the node that might have a replacement leaf
    let get_folder predicate =
        let folder k _ acc =
            if (predicate k) && (distance node.guid k > distance node.guid acc) then k
            else acc
        folder

    let predicate = if valid_min_leaf node leaf then valid_min_leaf node else valid_max_leaf node
    let end_leaf = Map.foldBack (get_folder predicate) leaves node.guid
    if end_leaf = node.guid then
        if valid_min_leaf node leaf then
            // No other min leaves, therefore min is max
            { updated_node with minleaf = node.maxleaf; }
        else
            // No other max leaves, therefore max is min
            { updated_node with maxleaf = node.minleaf; }
    else
        // Ask it for its state
        let (_, _, (resp, status)) = route_func node GetState "" end_leaf inter
        let state = deserialize resp

        // Get the replacement leaves if any
        let update_folder guid addr node' =
            if not (guid = leaf) then try_add_leaf node' guid addr
            else node'
        Map.foldBack update_folder state.leaves updated_node

// Updates this node based on a new node that is joining the network
let update_node (node: Node) (new_node: Node) : Node =
    let updated_leaves = try_add_leaf node new_node.guid new_node.address
    try_add_neighbor updated_leaves new_node.guid new_node.address

// Handles a message intended for this node
let handle_message<'a> (node: Node) (typ: MessageType) (message: string) (key: GUID)
        (route_func: RouteFunc<'a>) (inter: PastryInterface<'a>)
        : Node * 'a * (string * int) =
    printfn "PASTRY: Handling '%A' message '%s'..." typ message
    match typ with
    | Join -> // A node is requesting to join, and this is the one with the nearest GUID
        let firstsep = message.IndexOf(SEPARATOR)
        let address = message.[..firstsep-1]
        let states = message.[firstsep + SEPARATOR.Length..]
        // Failure is unimportant here, no?
        ignore <| send_message address JoinState states key
        node, inter.state, ("", 200)

    | JoinState -> // This node receives the states needed to initialize itself
        let updated_node = handle_join node message
        updated_node, inter.state, ("Joined", 200)

    | Update -> // The node is notified of a new node joining
        let updated_node = update_node node <| deserialize message
        printfn "PASTRY: State after update: %A" updated_node
        updated_node, inter.state, ("Updated", 200)

    | Resource(path, meth) -> // Request for a resource here
        printfn "PASTRY: Requesting resource at '%s' using '%s'" path meth
        let send_func =
            match inter.send with
            | Some(func) -> func
            | None -> failwith "ASSERTION FAILED: No send func in interface at handle_message"
        let (state', resp, status) = inter.handle path meth message send_func inter.state
        node, state', (resp, status)

    | DeadNode -> // Something has died
        printfn "PASTRY: Notified that %s has died" (serialize_guid key)
        let dead =
            match deserialize_guid message with
            | Some(g) -> g
            | None -> failwith "Could not deserialize dead node GUID %s" message

        let watched = find_watched_neighbor node
        printfn "%s" <| String.replicate 50 "="
        printfn "Watched neighbor of %s: %s" (string node.guid) (string watched)
        printfn "min, max, leaves: %s | %s | %A" (string node.minleaf) (string node.maxleaf) node.leaves
        printfn "%s" <| String.replicate 50 "="

        if dead = watched then
            printfn "DEAD: It's the one I'm watching!"
            match Map.tryFind dead node.leaves with
            | Some(_) ->
                let node' = handle_dead_leaf node dead route_func inter
                node', inter.state, ("Handled!", 200)
            | None ->
                printfn "PASTRY: Already handled..."
                node, inter.state, ("Already handled", 200)
        else
            // Route it a little to the left
            let closer dst a b =
                (distance a dst) < (distance b dst)
            let folder k v acc =
                if (valid_min_leaf node k) && (not (k = dead)) && (closer node.guid k acc) then
                    k
                else
                    acc
            let dst = Map.foldBack folder node.leaves node.minleaf
            printfn "PASTRY: Forwarding 'dead' call to %s" (serialize_guid dst)
            route_func node DeadNode message dst inter
    | Backup ->
        printfn "PASTRY: Backup received!"
        { node with backup = message; }, inter.state, ("Backed up!", 200)

    | GetState | Ping ->
        failwith "GetState and Ping messages are handled elsewhere!"

// Routes something using the leaf set of the node
let rec route_leaf (node: Node) (typ: MessageType) (msg: string) (key: GUID)
        (route_func: RouteFunc<'a>) (inter: PastryInterface<'a>)
        : Node * 'a * (string * int) =
    let distance_check = fun leafkey _ acc ->
        if distance leafkey key < distance acc key then leafkey else acc
    let closest = Map.foldBack distance_check node.leaves node.guid
    if closest = node.guid then
        let (node', state', resp) = handle_message node typ msg key route_func inter
        node', state', resp
    else
        let address = Map.find closest node.leaves
        match send_message address typ msg key with
        | Some(result) ->
            node, inter.state, result
        | None -> // HERE BE POSSIBLE DEADLOCKS (yay)
            // Fix the leaf
            let updated_node = handle_dead_leaf node closest route_func inter
            // Retry
            route_leaf updated_node typ msg key route_func inter

// Messages
// Join: join address
// Routes a message somewhere
let rec route<'a> (node: Node) (typ: MessageType) (msg: string) (key: GUID)
        (inter: PastryInterface<'a>) : Node * 'a * (string * int) =
    printfn "PASTRY: Routing '%A' message towards '%A'" typ key
    let (node', message) =
        match typ with // This is actually okay since the nodes contain no strings
        | Join ->
            // NOTE NOTE NOTE ------- remove later!
            // Simulate some latency on localhost

            // If there is an old version in the leaf set
            let node' =
                if Map.containsKey key node.leaves then
                    // Make sure it's dead before continuing!
                    printfn "DEAD: Removing old reference before forwarding"
                    handle_dead_leaf node key route inter
                else
                    node

            printfn "Sleeping before forwarding join..."
            Thread.Sleep(1000)
            printfn "Done! Continuing..."
            node', sprintf "%s%s%s" msg SEPARATOR (serialize node')
        | _ ->
            node, msg

    let valid_leaf = (valid_min_leaf node' key) || (valid_max_leaf node' key)
    // Within leaf set
    if valid_leaf || Map.isEmpty node'.leaves || (key = node'.guid) then
        printfn "PASTRY: Found target within leaf set!"
        route_leaf node' typ message key route inter
    else
        printfn "PASTRY: Checking routing table..."
        printfn "PASTRY: Actually just using the leaf set ATM..."
        // Same check as above... for laziness
        route_leaf node' typ message key route inter

// Attempts to forward a message to a pastry node using the given url parts
let try_forward_pastry<'a> (node: Node) (cmd_str: string) (dst_str: string)
        (body: string) (response: HttpListenerResponse) (inter: PastryInterface<'a>)
        : InterpretResult<'a> =
    match deserialize_guid dst_str with
    | Some(guid) ->
        printfn "%A" route
        match cmd_str with
        | "join" ->
            reply response "Forward attempted!" 200 "Ok"
            let (node', state', _) = route node Join body guid inter
            Valid(node', state')
        | "update" ->
            reply response "Forward attempted!" 200 "Ok"
            let (node', state', _) = route node Update body guid inter
            Valid(node', state')
        | "joinstate" ->
            reply response "Forward attempted!" 200 "Ok"
            let (node', state', _) = handle_message node JoinState body guid route inter
            Valid(node', state')
        | "backup" ->
            reply response "Forward attempted!" 200 "Ok"
            let (node', state', _) = handle_message node Backup body guid route inter
            Valid(node', state')
        | "ping" ->
            reply response (sprintf "PONG @ %s" (serialize_guid node.guid)) 200 "Ok"
            Valid(node, inter.state)
        | "getstate" ->
            reply response (serialize node) 200 "Ok"
            Valid(node, inter.state)
        | "deadnode" ->
            reply response "Handling of death attempted" 200 "Ok"
            let (node', state', _) = handle_message node DeadNode body guid route inter
            Valid(node', state')
        | _ ->
            let error_message = sprintf "Bad pastry command: '%s'" cmd_str
            Invalid(error_message, 400, "Not Found")
    | None ->
        let error_message = sprintf "Invalid GUID received: '%s'" dst_str
        Invalid(error_message, 404, "Not found")

// Attempts to forward some given url parts
let try_forward_resource<'a> (node: Node) (split_res_path: string list)
        (meth: string) (data: string) (response: HttpListenerResponse)
        (inter: PastryInterface<'a>) : InterpretResult<'a> =
    match get_destination split_res_path with
    | Ok(guid) ->
        // Construct the message function used by the repo to route messages out
        let rec send_func (resource_path: string) (meth: string) (data: string) (state: 'a): ResourceResponse<'a> =
            match get_destination (split resource_path '/') with
            | Ok(guid) ->
                let (node', state', (resp, status)) =
                    route node (Resource(resource_path, meth)) data guid { inter with send = Some(send_func); }
                (state', resp, status)

            | Error(resp, status, reason) ->
                error <| sprintf "Could not send '%s' message from repo to '%s'" meth resource_path
                (state, resp, status)

        let resource_url = String.concat "/" split_res_path
        let (node', state', (message, status)) = route node (Resource(resource_url, meth)) data guid { inter with send = Some(send_func); }
        reply response message status "Ok"
        Valid(node', state')
    | Error(msg, status, reason) ->
        Invalid(msg, status, reason)

// Pings the closest bigger neighbor of a node to see if it's still alive
let ping_neighbor<'a> (node: Node) (inter: PastryInterface<'a>) : Node =
    let neighbor = find_watched_neighbor node
    printfn "PING: -> %s" (serialize_guid neighbor)
    if neighbor = node.guid then // No smaller leaves
        // Send it to the highest leaf
        printfn "PASTRY: No other leaves, ping delayed..."
        node
    else
        let address =
            match Map.tryFind neighbor node.leaves with
            | Some(addr) ->
                addr
            | None ->
                failwith "ASSERTION FAILED: Smallest key not found in leaves... WTF!"
        match send_message address Ping "" neighbor with
        | None ->
            printfn "PASTRY: Neighbor is dead, do something!"
            handle_dead_leaf node neighbor route inter
        | Some(resp, status) ->
            node

// Makes the given pastry node start listening...
let start_listening<'a when 'a: equality> (node: Node) (inter_arg: PastryInterface<'a>) =
    printfn "Initializing..."
    use listener = new System.Net.HttpListener ()
    let basepath = sprintf "http://%s/" node.address
    printfn "Listening at '%s'..." basepath
    listener.Prefixes.Add basepath
    listener.Start ()

    // Listen for a a message
    let rec listen (node: Node) (inter: PastryInterface<'a>) =
        printfn "> Waiting..."
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
        printfn "Path: %s" path
        let parts = split (path.[1..]) '/' // That annoying first slash...
        printfn "Parts: %A" parts
        // Now interpret what was received
        let result =
            match parts with
            | "pastry"::cmd_string::dst_string::[] ->
                try_forward_pastry node cmd_string dst_string body response inter

            | "pastry"::stuff -> // only handles '/pastry'
                let error_message = sprintf "Unrecognized pastry request url: '%s'" path
                Invalid(error_message, 404, "Illegal action")

            | "resource"::resource_path_parts -> // Someone requests a resource
                let res = try_forward_resource node resource_path_parts meth body response inter
                // Handle sending of backups
                match res with
                | Valid(node', state') ->
                    // Find out whether the request was handled on this server
                    // NOTE: without this check the program will deadlock
                    // when there is just two nodes and a resource request passes
                    // through both of them
                    if not (state' = inter.state) then // WOW THIS IS UGLY PLEASE MAKE IT STOP!
                        // Serialize the updated state
                        let state_msg = inter.serialize state'

                        // Send it to the node closest to itself
                        send_backup_state node state_msg

                    res
                | _ -> res

            | _ ->
                let error_message = sprintf "Not related to this: %s" path
                Invalid(error_message, 200, "Not found")

        // NOTE: Only invalid requests are automatically replied
        match result with
        | Valid(updated_node, updated_state) ->
            //let updated_node' = ping_neighbor updated_node inter
            let updated_node' = updated_node
            listen updated_node' { inter with state = updated_state; }
        | Invalid(error_message, status, reason) ->
            error error_message
            reply response error_message status reason
            //let updated_node = ping_neighbor node inter
            let updated_node = node
            listen updated_node inter

    // Start listening
    ignore <| listen node inter_arg


// "Actually" starts the server. Used by the interface functions
let start_server_fixed_guid<'a when 'a: equality> (address: NetworkLocation) (peer: NetworkLocation option)
        (handler: ResourceRequestFunc<'a>) (serializer: SerializeFunc<'a>)
        (guid: GUID) (state: 'a) =
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
    let inter: PastryInterface<'a> = { // Create the inter parts
        send = None;
        handle = handler;
        serialize = serializer;
        state = state;
    }
    match peer with
    | None ->
        start_listening<'a> node inter
    | Some(peer) ->
        match send_message peer Join address guid with
        | None ->
            error <| sprintf "Could not establish a connection with peer at '%s'" peer
        | Some(_) ->
            start_listening<'a> node inter

// Starts a server with a fixed guid in string from
let test_server<'a when 'a: equality> (address: NetworkLocation) (peer: NetworkLocation option)
        (handler: ResourceRequestFunc<'a>) (serializer: SerializeFunc<'a>)
        (guid_str: string) (state: 'a) =
    let guid =
        match deserialize_guid guid_str with
        | Some(id) -> id
        | None -> failwith "Invalid GUID given!"
    start_server_fixed_guid address peer handler serializer guid state

// Creates a local node and makes it join the Pastry network
let start_server<'a when 'a: equality> (address: NetworkLocation) (peer: NetworkLocation option)
        (handler: ResourceRequestFunc<'a>) (serializer: SerializeFunc<'a>)
        (state: 'a) =
    let guid = hash address
    start_server_fixed_guid address peer handler serializer guid state

