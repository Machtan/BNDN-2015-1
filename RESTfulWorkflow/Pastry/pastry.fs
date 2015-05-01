module pastry

// Created by Jakob Lautrup Nysom 2015-04-24
open System
open System.Net
open System.Threading

open pastry_types
open pastry_utils

type NetworkLocation = string

// A function for the resource request func to send requests through
// partial_resource_url, method, data, state -> state, response
type SendFunc<'a> = string -> string -> string -> 'a -> 'a * string

// A function to handle resource requests
// url, method, send_func, state -> state, response
type ResourceRequestFunc<'a> = string -> string -> SendFunc<'a> -> 'a -> 'a * string

// ====================== CONFIG ======================

let DIGIT_POWER = 4 // 2^b-1 per row in the routing table
let MAX_LEAVES = 16 // or 32
let SEPARATOR = " SEPARATOR " // This is silly, but safe...

// ====================================================

// Sends a pastry message to a node at an address, that a type of message must
// be forwarded towards a pastry node, carrying some data
let send_message (address: NetworkLocation) (typ: MessageType) (message: string) (destination: U128) : string option =
    try
        match typ with
        | Resource(path, meth) ->
            let url = sprintf "http://%s/resource/%s" address path
            printfn "SEND: %s %s => %s" meth url message
            use w = new System.Net.WebClient ()
            match meth with
            | "GET" ->
                Some(w.DownloadString(url + "?data="+message)) // TODO make safer
            | _ ->
                Some(w.UploadString(url, meth, message))
        | _ ->
            let cmd =
                match typ with
                | Join          -> "join"
                | JoinState     -> "joinstate"
                | Update        -> "update"
                | Resource(_)   -> failwith "Got past a match on 'Resource'"
            let {a = p1; b = p2} = destination
            let url = sprintf "http://%s/pastry/%s/%020d%020d" address cmd p1 p2
            printfn "SEND: %s => %s" url message
            use w = new System.Net.WebClient ()
            Some(w.UploadString(url, "POST", message))
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

// Adds the given node as a neighbor if it makes sense (I'm not using this ATM)
let try_add_neighbor (node: Node) (guid: GUID) (address: NetworkLocation) : Node =
    if node.neighbors.Count < MAX_LEAVES then
        { node with neighbors = Map.add guid address node.neighbors; }
    else
        node

// Tries to add the given guid/address pair to the node as a leaf
let try_add_leaf (node: Node) (guid: GUID) (address: NetworkLocation) : Node =
    if node.leaves.Count < MAX_LEAVES then // Room for more
        let minleaf = if guid < node.minleaf then guid else node.minleaf
        let maxleaf = if guid > node.maxleaf then guid else node.maxleaf
        let leaves = Map.add guid address node.leaves
        { node with leaves = leaves; minleaf = minleaf; maxleaf = maxleaf; }
    else
        if guid > node.minleaf && guid < node.maxleaf then // Within replacement range
            if guid >= node.guid then
                let leaves = Map.add guid address (Map.remove node.maxleaf node.leaves)
                let maxleaf = Map.foldBack (fun k _ acc -> if k > acc then k else acc) leaves guid
                { node with leaves = leaves; maxleaf = maxleaf; }
            else
                let leaves = Map.add guid address (Map.remove node.minleaf node.leaves)
                let minleaf = Map.foldBack (fun k _ acc -> if k < acc then k else acc) leaves guid
                { node with leaves = leaves; minleaf = minleaf; }
        else
            node

// Handles a pastry join request
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
            neighbors = neighbors;
            leaves = leaves;
            minleaf = minleaf;
            maxleaf = maxleaf;
        }
    let notify guid address =
        match send_message address Update (serialize updated_node) guid with
        | Some(_) -> ()
        | None -> printfn "PASTRY: Failed to notify %A at '%s' of update..." guid address
    Map.iter notify updated_node.leaves
    Map.iter notify updated_node.neighbors
    List.iter (fun map_level -> Map.iter notify map_level) updated_node.routing_table
    printfn "PASTRY: State after joining: %A" updated_node
    updated_node

// Handles a message intended for this node
let handle_message<'a> (node: Node) (typ: MessageType) (message: string) (key: GUID)
        (handler: ResourceRequestFunc<'a>) (send_func: SendFunc<'a>) (state: 'a)
        : Node * 'a * (string option) =
    printfn "PASTRY: Handling '%A' message '%s'..." typ message
    match typ with
    | Join -> // A node is requesting to join, and this is the one with the nearest GUID
        let firstsep = message.IndexOf(SEPARATOR)
        let address = message.[..firstsep-1]
        let states = message.[firstsep + SEPARATOR.Length..]
        // Failure is unimportant here, no?
        ignore <| send_message address JoinState states key
        node, state, None

    | JoinState -> // This node receives the states needed to initialize itself
        let updated_node = handle_join node message
        updated_node, state, None

    | Update -> // The node is notified of a new node joining
        let node_state = deserialize message
        let updated_leaves = try_add_leaf node node_state.guid node_state.address
        let updated_node = try_add_neighbor updated_leaves node_state.guid node_state.address
        printfn "PASTRY: State after update: %A" updated_node
        updated_node, state, None

    | Resource(path, meth) -> // Request for a resource here
        printfn "PASTRY: Requesting resource at '%s' using '%s'" path meth
        let (state', resp) = handler path meth send_func state
        node, state', Some(resp)

// Messages
// Join: join address
// Routes a message somewhere
let route<'a> (node: Node) (typ: MessageType) (msg: string) (key: GUID)
        (handler: ResourceRequestFunc<'a>) (send_func: SendFunc<'a>) (state: 'a)
        : Node * 'a * (string option) =
    printfn "PASTRY: Routing '%A' message towards '%A'" typ key
    let message =
        match typ with // This is actually okay since the nodes contain no strings
        | Join ->
            // NOTE NOTE NOTE ------- remove later!
            // Simulate some latency on localhost
            printfn "Sleeping before forwarding join..."
            Thread.Sleep(1000)
            printfn "Done! Continuing..."
            sprintf "%s%s%s" msg SEPARATOR (serialize node)
        | _ -> msg

    // Within leaf set
    if (node.minleaf <= key && key <= node.maxleaf) || Map.isEmpty node.leaves then
        printfn "PASTRY: Found target within leaf set!"
        let distance_check = fun leafkey _ acc ->
            if distance leafkey key < distance acc key then leafkey else acc
        let closest = Map.foldBack distance_check node.leaves node.guid
        if closest = node.guid then
            let (node', state', resp) = handle_message node typ message key handler send_func state
            node', state', resp
        else
            let address = Map.find closest node.leaves
            node, state, send_message address typ message key
    else
        printfn "PASTRY: Checking routing table..."
        printfn "PASTRY: Actually just using the leaf set ATM..."
        // Same check as above... for laziness
        let distance_check = fun leafkey _ acc ->
            if distance leafkey key < distance acc key then leafkey else acc
        let closest = Map.foldBack distance_check node.leaves node.guid
        if closest = node.guid then
            let (node', state', resp) = handle_message node typ message key handler send_func state
            node', state', resp
        else
            let address = Map.find closest node.leaves
            node, state, send_message address typ message key

// Attempts to forward a message to a pastry node using the given url parts
let try_forward_pastry<'a> (node: Node) (cmd_str: string) (dst_str: string)
        (body: string) (response: HttpListenerResponse) (state: 'a)
        : InterpretResult<'a> =
    match deserialize_guid dst_str with
    | Some(guid) ->
        printfn "%A" route
        match cmd_str with
        | "join" ->
            reply response "Send attempted!" 200 "Ok"
            let (node', state', _) = route node Join body guid DUMMY_HANDLER DUMMY_SEND_FUNC state
            Valid(node', state')
        | "update" ->
            reply response "Send attempted!" 200 "Ok"
            let (node', state', _) = route node Update body guid DUMMY_HANDLER DUMMY_SEND_FUNC state
            Valid(node', state')
        | "joinstate" ->
            reply response "Send attempted!" 200 "Ok"
            let (node', state', _) = handle_message node JoinState body guid DUMMY_HANDLER DUMMY_SEND_FUNC state
            Valid(node', state')
        | _ ->
            let error_message = sprintf "Bad pastry command: '%s'" cmd_str
            Invalid(error_message, 400, "Not Found")
    | None ->
        let error_message = sprintf "Invalid GUID received: '%s'" dst_str
        Invalid(error_message, 404, "Not found")

// Attempts to forward some given url parts
let try_forward_resource<'a> (node: Node) (split_res_path: string list) (meth: string)
        (data: string) (response: HttpListenerResponse) (handler: ResourceRequestFunc<'a>)
        (outer_state: 'a) : InterpretResult<'a> =
    match get_destination split_res_path with
    | Ok(guid) ->
        // Construct the message function used by the repo to route messages out
        let rec send_func (resource_path: string) (meth: string) (data: string) (state: 'a): 'a * string =
            match get_destination (split resource_path '/') with
            | Ok(guid) ->
                let (node', state', resp_msg) = route node (Resource(resource_path, meth)) data guid handler (send_func) state
                let resp =
                    match resp_msg with
                    | Some(str) -> str
                    | None -> failwith "ASSERT FAILED: The resource request did not return a string"
                (state', resp)
            | Error(resp, status, reason) ->
                error <| sprintf "Could not send '%s' message from repo to '%s'" meth resource_path
                (state, resp)

        let resource_url = String.concat "/" split_res_path
        let (node', state', resp_msg) = route node (Resource(resource_url, meth)) data guid handler (send_func) outer_state
        match resp_msg with // If there was some response from the routing just now:
        | Some(str) ->
            reply response str 200 "Ok"
        | None ->
            failwith <| sprintf "PASTRY: No response gotten when routing resource towards '%s' at '%s'" resource_url (serialize_guid guid)
        Valid(node', state')
    | Error(msg, status, reason) ->
        Invalid(msg, status, reason)

// Makes the given pastry node start listening...
let start_listening<'a> (node: Node) (handler_arg: ResourceRequestFunc<'a>) (state_arg: 'a) =
    printfn "Initializing..."
    use listener = new System.Net.HttpListener ()
    let basepath = sprintf "http://%s/" node.address
    printfn "Listening at '%s'..." basepath
    listener.Prefixes.Add basepath
    listener.Start ()

    // A function to handle resource requests
    // context (for replying to this call!), split_path (without /resources), send_func, state -> state
    //type ResourceRequestFunc<'a> = HttpListenerContext -> string list -> SendFunc -> 'a -> 'a

    let rec listen (node: Node) (handler: ResourceRequestFunc<'a>) (state: 'a) =
        // Listen for a a message

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
                try_forward_pastry node cmd_string dst_string body response state

            | "pastry"::stuff -> // only handles '/pastry'
                let error_message = sprintf "Unrecognized pastry request url: '%s'" path
                Invalid(error_message, 404, "Illegal action")

            | "resource"::resource_path_parts -> // Someone requests a resource
                try_forward_resource node resource_path_parts meth body response handler state

            | _ ->
                let error_message = sprintf "Not related to this: %s" path
                Invalid(error_message, 200, "Not found")

        // NOTE: Only invalid requests are automatically replied
        match result with
        | Valid(updated_node, updated_state) ->
            listen updated_node handler updated_state
        | Invalid(error_message, status, reason) ->
            error error_message
            reply response error_message status reason
            listen node handler state

    ignore <| listen node handler_arg state_arg// Start listening

// Creates a local node and makes it join the Pastry network
let start_server<'a> (address: NetworkLocation) (peer: NetworkLocation option) (handler: ResourceRequestFunc<'a>) (state: 'a) =
    let guid = hash address
    printfn "? Pastry GUID: %020d%020d" guid.a guid.b
    let node: Node = {
        guid = guid;
        address = address;
        leaves = Map.empty;
        minleaf = guid;
        maxleaf = guid;
        neighbors = Map.empty;
        routing_table = [];
    }
    match peer with
    | None ->
        start_listening<'a> node handler state
    | Some(peer) ->
        match send_message peer Join address guid with
        | None ->
            error <| sprintf "Could not establish a connection with peer at '%s'" peer
        | Some(_) ->
            start_listening<'a> node handler state
