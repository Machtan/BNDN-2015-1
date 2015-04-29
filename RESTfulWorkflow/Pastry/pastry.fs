// Created by Jakob Lautrup Nysom 2015-04-24
open System
open System.Security.Cryptography
open Newtonsoft.Json
open System.Net
open System.IO

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

type MessageType =
| Join      // A new node is joining and requesting states
| Update    // A new node has been added sucessfully: update to include it
| Route     // Send a message somewhere
// Add more persistency commands here

// The state record of a Pastry node
type Node = {
    id: GUID;
    address: NetworkLocation;
    // I assume that there are never more than 65k rows in the table
    routing_table: Map<GUID, Address> list;
    minleaf: U128;
    maxleaf: U128;
    neighbors: Map<GUID, Address>;  // Physical location proximity (IP address)
    leaves: Map<GUID, Address>;     // GUID-numerically closest nodes
}

// Splits a string at the given char and returns a list of substrings
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Sends a pastry message somewhere
let send_message (address: NetworkLocation) (destination: U128) (typ: MessageType) (data: string) =
    try
        let cmd =
            match typ with
            | Join      -> "join"
            | Update    -> "update"
            | Route     -> "route"
        let (p1, p2) = destination
        let url = sprintf "http://%s/pastry/%s/%d%d/" address cmd p1 p2
        printfn "SEND: %s => %s" url data
        use w = new System.Net.WebClient ()
        Some(w.UploadString(url, "POST", data))
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

// Messages
// Join: join address
// Routes a message somewhere
let route (node: Node) (typ: MessageType) (msg: string) (key: GUID) =
    // I'll just write this one... god knows where it should all go
    //if
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
    (a, b)

// Just a little formatting
let error str =
    printfn "ERROR: %s" str

// Makes the given pastry node start listening...
let start_listening (node: Node) =
    printfn "Initializing..."
    use listener = new System.Net.HttpListener ()
    listener.Prefixes.Add "http://localhost:8080/"
    listener.Start ()

    printfn "Listening..."

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
        let (updated_node, answer, status, reason) =
            match parts with
            | "pastry"::cmd::dst ->
                printfn "%A" route
                match cmd with
                | "join"    ->
                    node, "Unsupported", 404, "Not Found" //route node Join cmd dst body
                | "update"  ->
                    node, "Unsupported", 404, "Not Found" //route node Update cmd dst body
                | "route"   ->
                    node, "Unsupported", 404, "Not Found" //route node Route cmd dst body
                | _         ->
                    error <| sprintf "Received bad pastry command: '%s'" cmd
                    node, (sprintf "Bad pastry command: '%s'" cmd), 400, "Not Found"

            | "pastry"::stuff -> // only handles '/pastry'
                error <| sprintf "Bad pastry request url gotten: %s" path
                node, (sprintf "Unrecognized pastry request url: '%s'" path), 400, "Illegal action"
            | _ ->
                error <| sprintf "Got something not related to pastry: %s" path
                node, (sprintf "Not related to pastry: %s" path), 404, "Not found"
        reply response answer status reason
        listen updated_node // Listen again

    listen node // Start listening

// Creates a local node and makes it join the Pastry network
let join_network (address: NetworkLocation) (peer: NetworkLocation option): Node =
    let guid = hash address
    let node = {
        id = guid;
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
        match send_message peer guid Join address with
        | None ->
            failwith (sprintf "Could not establish a connection with peer at '%s'" peer)
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
        printfn "Joining pastry network at '%s'..." address
        let known_peer = if peer = "" then None else Some(peer)
        let node = join_network address known_peer
        ()
        // Start listening...
    | _ ->
        printfn "Usage: Pastry.exe <address> <port> <peer_address_with_port>"
    0