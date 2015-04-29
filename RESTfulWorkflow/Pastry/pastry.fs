// Created by Jakob Lautrup Nysom 2015-04-24
open System
open System.Security.Cryptography
open Newtonsoft.Json
open System.Net
open System.IO
open System.Threading

// Type definitions
type U128 = uint64 * uint64
type NetworkLocation = string // an url... Maybe something better later
type GUID = U128 // u128... Maybe something better later
type Address = NetworkLocation

//type DeliverFunc = string -> GUID -> unit
//type ForwardFunc = string -> GUID -> GUID -> string * GUID
//type NewLeafFunc = GUID list -> unit

// ====================== CONFIG ======================

let b = 4 // 2^b-1 per row in the routing table
let len_L = 16 // or 32

// ====================================================

// UPDATE: send_messsage and start_listening.listen
type MessageType =
| Join      // A new node is joining and requesting states
| Update    // A new node has been added sucessfully: update to include it
| JoinState // The state data for a new node
| Route     // Send a message somewhere
// Add more persistency commands here

// The state record of a Pastry node
type Node = {
    guid: GUID;
    address: NetworkLocation;
    // I assume that there are never more than 65k rows in the table
    routing_table: Map<GUID, Address> list;
    minleaf: U128;
    maxleaf: U128;
    neighbors: Map<GUID, Address>;  // Physical location proximity (IP address)
    leaves: Map<GUID, Address>;     // GUID-numerically closest nodes
}

// Just a little formatting
let error str =
    printfn "ERROR: %s" str

// Splits a string at the given char and returns a list of substrings
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Sends a pastry message to a node at an address, that a type of message must
// be forwarded towards a pastry node, carrying some data
let send_message (address: NetworkLocation) (typ: MessageType) (message: string) (destination: U128) =
    try
        let cmd =
            match typ with
            | Join      -> "join"
            | JoinState -> "joinstate"
            | Update    -> "update"
            | Route     -> "route"
        let (p1, p2) = destination
        let url = sprintf "http://%s/pastry/%s/%020d%020d" address cmd p1 p2
        printfn "SEND: %s => %s" url message
        use w = new System.Net.WebClient ()
        Some(w.UploadString(url, "POST", message))
    with
        | ex -> None

// Replies to a http request
let reply (response: HttpListenerResponse) answer status reason =
    printfn " --> %d %s [%s]" status reason answer
    // Set HTTP statuscode and reason (e.g., 200 OK)
    response.StatusCode <- status
    response.StatusDescription <- reason
    // Encode and write body.
    let buffer = System.Text.Encoding.UTF8.GetBytes(answer : string)
    response.ContentLength64 <- int64 buffer.Length;
    response.OutputStream.Write(buffer,0,buffer.Length);
    response.OutputStream.Close();

// Calculates the absolute distance between two GUIDs
let distance (a: GUID) (b: GUID) : U128 =
    let (a1, a2) = a
    let (b1, b2) = b // below: Unsigned shenanigans
    let n1 = if a1 > b1 then a1 - b1 else b1 - a1
    let n2 = if a2 > b2 then a2 - b2 else b2 - a2
    (n1, n2)

// Serializes the state of the given node
let serialize (node: Node) : string =
    JsonConvert.SerializeObject node

let SEPARATOR = " SEPARATOR " // This is silly...
// Handles a message intended for this node
let handle_message (node: Node) (typ: MessageType) (msg: string) (key: GUID) =
    printfn "PASTRY: Handling '%A' message '%s'..." typ msg
    match typ with
    | Join ->
        let firstsep = msg.IndexOf(SEPARATOR)
        let address = msg.[..firstsep-1]
        let states = msg.[firstsep + SEPARATOR.Length..]
        // Failure is unimportant here, no?
        ignore <| send_message address JoinState states key
        node
    | _ ->
        node

// Messages
// Join: join address
// Routes a message somewhere
let route (node: Node) (typ: MessageType) (msg: string) (key: GUID) =
    printfn "PASTRY: Routing '%A' message towards '%A'" typ key
    let message =
        match typ with // This is actually okay since the nodes contain no strings
        | Join ->
            // NOTE NOTE NOTE ------- remove later!
            // Simulate some latency on localhost
            printfn "Sleeping before forwarding join..."
            Thread.Sleep(10000)
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
            handle_message node typ message key
        else
            let address = Map.find closest node.leaves
            match send_message address typ message key with
            | None -> error <| sprintf "Failed to send message to %s" address
            | Some(_) -> ()
            node
    else
        printfn "PASTRY: Checking routing table..."
        node

// Convert a byte array to an u64. Please don't use an array that is more than
// 8 bytes long... (tip: You get undefined behavior)
let to_u64 (byte_arr: byte[]) = // It's this or mapi + foldback ...
    let rec inner arr pos acc =
        match arr with
        | [] -> acc
        | x::xs -> // Use bit shifts to add the correct value
            inner xs (pos - 1) (acc + ((uint64 x) <<< (8 * pos)))
    let res: uint64 = 0UL
    inner (Array.toList byte_arr) (byte_arr.Length - 1) res

// Converts an IP-address (or any other string) to an u128 (sorta)
let hash (ip_address: NetworkLocation): U128 =
    let content: byte[] = System.Text.Encoding.ASCII.GetBytes(ip_address)
    let bytes: byte[] = (content |> HashAlgorithm.Create("SHA1").ComputeHash).[..15] // 16 first bytes
    let a = to_u64 bytes.[..7]
    let b = to_u64 bytes.[8..15]
    printfn "Hash : a / b : %d / %d" a b
    (a, b)

// Attempts to convert the given string to a GUID
let deserialize_guid (str: string) : GUID option =
    if not (str.Length = 40) then
        None
    else
        let a = str.[..19]
        let b = str.[20..]
        try
            let ua = System.Convert.ToUInt64(a)
            let ub = System.Convert.ToUInt64(b)
            Some((ua, ub)) //
        with
            | ex -> None

// A result type to simplify the interpretation a little
type InterpretResult =
| Valid of Node
| Invalid of string * int * string

// Makes the given pastry node start listening...
let start_listening (node: Node) =
    printfn "Initializing..."
    use listener = new System.Net.HttpListener ()
    let basepath = sprintf "http://%s/" node.address
    printfn "Listening at '%s'..." basepath
    listener.Prefixes.Add basepath
    listener.Start ()

    let rec listen (node: Node) =
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
                match deserialize_guid dst_string with
                | Some(guid) ->
                    printfn "%A" route
                    match cmd_string with
                    | "join" ->
                        reply response "Send attempted!" 200 "Ok"
                        Valid(route node Join body guid)
                    | "update" ->
                        reply response "Send attempted!" 200 "Ok"
                        Valid(route node Update body guid)
                    | "route" ->
                        reply response "Send attempted!" 200 "Ok"
                        Valid(route node Route body guid)
                    | "joinstate" ->
                        reply response "Received!" 200 "Ok"
                        Valid(handle_message node JoinState body guid)
                    | _ ->
                        let error_message = sprintf "Bad pastry command: '%s'" cmd_string
                        Invalid(error_message, 400, "Not Found")
                | None ->
                    let error_message = sprintf "Invalid GUID received: '%s'" dst_string
                    Invalid(error_message, 404, "Not found")

            | "pastry"::stuff -> // only handles '/pastry'
                let error_message = sprintf "Unrecognized pastry request url: '%s'" path
                Invalid(error_message, 400, "Illegal action")
            | _ ->
                let error_message = sprintf "Not related to pastry: %s" path
                Invalid(error_message, 404, "Not found")

        match result with
        | Valid(updated_node) ->
            listen updated_node // Listen again
        | Invalid(error_message, status, reason) ->
            error error_message
            reply response error_message status reason

    ignore <| listen node // Start listening

// Creates a local node and makes it join the Pastry network
let join_network (address: NetworkLocation) (peer: NetworkLocation option) =
    let guid = hash address
    let (a, b) = guid
    printfn "? Pastry GUID: %020d%020d" a b
    let node = {
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
        start_listening node
    | Some(peer) ->
        match send_message peer Join address guid with
        | None ->
            error <| sprintf "Could not establish a connection with peer at '%s'" peer
        | Some(_) ->
            start_listening node

// Adapted from https://stackoverflow.com/questions/1069103/how-to-get-my-own-ip-address-in-c
let get_public_ip () =
    let request: WebRequest = WebRequest.Create("http://checkip.dyndns.org/")
    use response: WebResponse = request.GetResponse()
    use stream: StreamReader = new StreamReader(response.GetResponseStream())
    let direction = stream.ReadToEnd()

    //Search for the ip in the html
    let first = direction.IndexOf("Address: ") + 9
    let last = direction.LastIndexOf("</body>")

    direction.Substring(first, last - first)

// Small-scale testing entry point
[<EntryPoint>]
let main args =
    //let m = Map.ofList [("bob", 20); ("alice", 31); ("ben", 10);]
    //let json = JsonConvert.SerializeObject m;
    //printfn "Serialized: %s" json
    //let data = JsonConvert.DeserializeObject<Map<string, int>>json
    //printfn "Deserialized: %A" data
    //listen()
    //join_network "localhost:8080" None
    //join_network "localhost:80" (Some("localhost:8080"))
    printfn "Danish Pastry!"
    match args with
    | [|addr; port; peer|] ->
        let address = sprintf "%s:%s" (if addr = "" then get_public_ip() else addr) port
        printfn "? Joining pastry network at '%s'..." address
        let known_peer = if peer = "" then None else Some(peer)
        let node = join_network address known_peer
        ()
        // Start listening...
    | _ ->
        printfn "Usage: Pastry.exe <address> <port> <peer_address_with_port>"
    0