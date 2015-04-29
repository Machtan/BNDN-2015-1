// Created by Jakob Lautrup Nysom 2015-04-24
open System
open System.Security.Cryptography
open Newtonsoft.Json

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

// Messages
// Join: join address
// Routes a message somewhere
let route (node: Node) (msg: string) (key: GUID) =
    // I'll just write this one... god knows where it should all go
    //if
    0

// Send messages to the other pastry nodes that this has been created
let finalize_initialisation (node: Node) =
    0

// Sends a join message to find the key closest to the GUID of the new node,
// appending the state tables of all nodes on the way before returning it
let send_join_message (new_node: GUID, address: Address, data: string) =
    0

let listen_until_initialized (node: Node) =
    0

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

let listen () =
    use listener = new System.Net.HttpListener ()
    listener.Prefixes.Add "http://localhost:8080/"
    listener.Start ()
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
        node
    | Some(peer) ->
        match send_message peer guid Join address with
        | None -> failwith (sprintf "Could not establish a connection with peer at '%s'" peer)
        | Some(_) ->
            node

let send (node: Node) (msg: string) (address: Address) =
    0

let forward (node: Node) (msg: string) (dst: GUID) =
    0



// Small-scale testing entry point
[<EntryPoint>]
let main args =
    printfn "Hello world"
    let m = Map.ofList [("bob", 20); ("alice", 31); ("ben", 10);]
    let json = JsonConvert.SerializeObject m;
    printfn "Serialized: %s" json
    let data = JsonConvert.DeserializeObject<Map<string, int>>json
    printfn "Deserialized: %A" data
    //listen()
    join_network "localhost:8080" None
    join_network "localhost:80" (Some("localhost:8080"))
    0