module pastry_utils

open System
open System.Security.Cryptography
open System.IO
open System.Net
open Newtonsoft.Json

open pastry_types

// Just a little formatting
let error str =
    printfn "ERROR: %s" str

// Splits a string at the given char and returns a list of substrings
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Calculates the absolute distance between two GUIDs
let distance (one: GUID) (two: GUID) : U128 =
    // Unsigned shenanigans
    let a = if one.a > two.a then one.a - two.a else two.a - one.a
    let b = if one.b > two.b then one.b - two.b else two.b - one.b
    {a = a; b = b}

// Converts a GUID to a string
let serialize_guid (guid: GUID) : string =
    sprintf "%020d%020d" guid.a guid.b

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
            Some({a = ua; b = ub}) //
        with
            | ex -> None

// Serializes the state of the given node
let serialize (node: Node) : string =
    let serialize_guid_map m =
        Map.foldBack (fun k v acc -> Map.add (serialize_guid k) v acc) m Map.empty
    let leaves = serialize_guid_map node.leaves
    let neighbors = serialize_guid_map node.neighbors
    let minleaf = serialize_guid node.minleaf
    let maxleaf = serialize_guid node.maxleaf
    let guid = serialize_guid node.guid
    let routing_table = List.map serialize_guid_map node.routing_table
    let new_node: SerializableNode =
        {
            guid = guid;
            address = node.address;
            neighbors = neighbors;
            leaves = leaves;
            minleaf = minleaf;
            maxleaf = maxleaf;
            routing_table = routing_table;
        }
    JsonConvert.SerializeObject new_node

// Deserializes the state of the given node
let deserialize (json: string) : Node =
    let node = JsonConvert.DeserializeObject<SerializableNode> json
    let deserialize_safe_guid guid =
        match deserialize_guid guid with
        | Some(g) -> g
        | None -> failwith (sprintf "Attempted to deserialize invalid GUID: %s" guid)
    let deserialize_guid_map m =
        Map.foldBack (fun k v acc -> Map.add (deserialize_safe_guid k) v acc) m Map.empty
    let leaves = deserialize_guid_map node.leaves
    let neighbors = deserialize_guid_map node.neighbors
    let minleaf = deserialize_safe_guid node.minleaf
    let maxleaf = deserialize_safe_guid node.maxleaf
    let guid = deserialize_safe_guid node.guid
    let routing_table = List.map deserialize_guid_map node.routing_table
    let new_node: Node =
        {
            guid = guid;
            address = node.address;
            neighbors = neighbors;
            leaves = leaves;
            minleaf = minleaf;
            maxleaf = maxleaf;
            routing_table = routing_table;
        }
    new_node

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

// Get a list of the digits of a GUID as u64s
// NOTE: (could be u16 with the default value of 'b' (DIGIT_POWER))
let get_digits (guid: GUID) : uint64 list =
    let get_bytes (x: uint64): byte[] =
        let barr =
            if BitConverter.IsLittleEndian then
                Array.rev (BitConverter.GetBytes(x))
            else
                BitConverter.GetBytes(x)
        let len = Array.length barr
        if len < 8 then // Pad with zeroes if needed
            Array.append (Array.zeroCreate (8-len)) barr
        else
            barr

    let a_bytes = get_bytes guid.a
    let b_bytes = get_bytes guid.b

    let rec map_bytes acc = function
    | [] -> acc
    | byte1::byte2::tail -> map_bytes ((to_u64 [|byte1;byte2|])::acc) tail
    | _ -> failwith "This shouldn't happen!"

    List.rev (map_bytes [] (List.ofArray (Array.append a_bytes b_bytes)))

// Gets the length of the shared digits between two GUIDs
let shared_prefix_length (a: GUID) (b: GUID) =
    let rec inner a_list b_list len =
        match a_list, b_list with
        | [], [] -> len
        | ax::axs, bx::bxs ->
            if ax = bx then
                inner axs bxs (len + 1)
            else
                len
        | _ -> failwith "This should not happen: The list have different sizes"
    inner (get_digits a) (get_digits b) 0

// Converts an IP-address (or any other string) to an u128 (sorta)
// NOTE: I'm not sure whether this is endian-safe
let hash (ip_address: NetworkLocation): U128 =
    let content: byte[] = System.Text.Encoding.ASCII.GetBytes(ip_address)
    let bytes: byte[] = (content |> HashAlgorithm.Create("SHA1").ComputeHash).[..15] // 16 first bytes
    let a = to_u64 bytes.[..7]
    let b = to_u64 bytes.[8..15]
    //printfn "Hash : a / b : %d / %d" a b
    {a = a; b = b}

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

// Returns the destination of a given split resource url (after the /resources/ part)
let get_destination (resource_url: string list): Destination =
    let path = String.concat "/" resource_url
    match resource_url with
    | "user"::name::[] ->
        let path = "user/name"
        Ok(hash (sprintf "user/%s" name))
    | "user"::name::unwanted_stuff ->
        let error_message = sprintf "Bad user path '%s'. It should be on the form 'user/<username>'" path
        Error(error_message, 400, "Invalid URL")
    | "workflow"::workflow::[] ->
        Ok(hash (sprintf "workflow/%s" workflow))
    | "workflow"::workflow::event::attribute::[] ->
        Ok(hash (sprintf "workflow/%s/%s" workflow event))
    | "workflow"::badly_formed_path ->
        let error_message = sprintf "Bad workflow path '%s'. It should be on the form '/resource/workflow/<name>[/<eventname>/<attribute>]'" path
        Error(error_message, 400, "Invalid URL")
    | unknown_resource::whatever ->
        let error_message = sprintf "Unknown resource type: '%s'" unknown_resource
        Error(error_message, 404, "Not found")
    | _ ->
        Error("Whatever that is, it isn't here", 404, "Not found")

// A dummy for testing
let DUMMY_SEND_FUNC resource_url meth data state : 'a * string=
    failwith "Dummy SendFunc called! (how did this happen?)"

// A dummy for testing
let DUMMY_HANDLER resource_url meth send_func state : 'a * string=
    failwith "Dummy ResourceRequestFunc called (how did this happen?)"