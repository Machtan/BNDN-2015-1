module PastryUtils

open System
open System.Security.Cryptography
open System.IO
open System.Net
open System.Numerics
open Newtonsoft.Json
open System.Text

open PastryTypes
open NewWebClient

// ============= CONFIG ================
let DIGIT_POWER = 4 // 2^b-1 per row in the routing table
let MAX_LEAVES = 16 // or 32

// Just a little formatting
let error str =
    printfn "PASTRY ERROR: %s" str

// Splits a string at the given char and returns a list of substrings
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Replies to a http request
let reply (response: HttpListenerResponse) (answer: string) (status: int) (reason: string) =
    printfn "REPLY: --> %d | %s | %s" status reason answer
    // Set HTTP statuscode and reason (e.g., 200 OK)
    response.StatusCode <- status
    response.StatusDescription <- reason
    // Encode and write body.
    let buffer = System.Text.Encoding.UTF8.GetBytes(answer : string)
    response.ContentLength64 <- int64 buffer.Length;
    response.OutputStream.Write(buffer,0,buffer.Length);
    response.OutputStream.Close();


// Checks whether the given GUID is within the valid min leaf range
let valid_min_leaf (node: Node) (check: GUID) =
    if node.minleaf > node.guid then
        check >= node.minleaf || check < node.guid
    else
        check >= node.minleaf && check < node.guid

// Counts the number of smaller leaves in a node
let min_leaf_count (node: Node) =
    let folder k _ acc =
        if valid_min_leaf node k then acc + 1
        else acc
    Map.foldBack folder node.leaves 0

// Checks whether the given GUID is within the valid max leaf range
let valid_max_leaf (node: Node) (check: GUID) =
    if node.maxleaf < node.guid then
        check <= node.maxleaf || check > node.guid
    else
        check <= node.maxleaf && check > node.guid


// Counts the number of larger leaves in a node
let max_leaf_count (node: Node) =
    let folder k _ acc =
        if valid_max_leaf node k then acc + 1
        else acc
    Map.foldBack folder node.leaves 0

// Calculates the absolute distance between two GUIDs
let distance (one: GUID) (two: GUID) : GUID =
    abs (two - one)

// Converts a GUID to a string
let serialize_guid (guid: GUID) : string =
    let serialized = string guid
    (String.replicate (40 - String.length serialized) "0") + serialized

// Attempts to convert the given string to a GUIDi/
let deserialize_guid (str: string) : GUID option =
    if not (str.Length = 40) then
        None
    else
        try
            Some(BigInteger.Parse(str)) //
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
            backup = node.backup;
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
            backup = node.backup;
        }
    new_node

// Convert a byte array to an u64. Please don't use an array that is more than
// 8 bytes long... (tip: You get undefined behavior)
let to_u64 (byte_arr: byte[]) : uint64 = // It's this or mapi + foldback ...
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
    let get_bytes (guid: GUID) : byte list =
        let barr =
            if BitConverter.IsLittleEndian then
                Array.rev (guid.ToByteArray())
            else
                guid.ToByteArray()
        let len = Array.length barr
        if len < 16 then // Pad with zeroes if needed
            List.ofArray <| Array.append (Array.zeroCreate (16-len)) barr
        else
            List.ofArray barr

    let rec map_bytes acc = function
    | [] -> acc
    | byte1::byte2::tail -> map_bytes ((to_u64 [|byte1;byte2|])::acc) tail
    | _ -> failwith "This shouldn't happen!"

    List.rev (map_bytes [] <| get_bytes guid)

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
let hash (ip_address: NetworkLocation): GUID =
    let content: byte[] = System.Text.Encoding.ASCII.GetBytes(ip_address)
    let bytes: byte[] = (content |> HashAlgorithm.Create("SHA1").ComputeHash).[..15] // 16 first bytes
    let a = to_u64 bytes.[..7]
    let b = to_u64 bytes.[8..15]
    //printfn "Hash : a / b : %d / %d" a b
    (bigint a) * (bigint UInt64.MaxValue) + (bigint b)

// Finds the leaf that is to the left of this node
let find_left_leaf (node: Node) : GUID =
    let closer_than k acc =
        distance k node.guid < distance acc node.guid
    let folder k _ acc =
        if (valid_min_leaf node k) && (closer_than k acc) then k
        else acc
    Map.foldBack folder node.leaves node.minleaf

// Finds the leaf that is to the right of this node
let find_right_leaf (node: Node) : GUID =
    let closer_than k acc =
        distance k node.guid < distance acc node.guid
    let folder k _ acc =
        if (valid_max_leaf node k) && (closer_than k acc) then k
        else acc
    Map.foldBack folder node.leaves node.maxleaf

// Finds the GUID of the neighbor that this node watches over
let find_watched_neighbor (node: Node) = find_right_leaf node

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
    if leaves.Count = 0 then
        { node with leaves = leaves; minleaf = node.guid; maxleaf = node.guid; }
    else if leaves.Count < MAX_LEAVES then // Room for more
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


let normal_client: WebClient = new WebClient ()
let timeout_client: WebClient = (new WebClientWithTimeout(100)) :> WebClient


// Sends a HTTP message using the given HTTP action argument type
let send_http (action: HttpAction) (use_timeout: bool): HttpResult =
    try
        let client: WebClient =
            if use_timeout then timeout_client
            else normal_client

        match action with
        | Download(url, data) ->
            let full_url = sprintf "%s?data=%s" url data
            HttpResult.Ok(client.DownloadString(full_url))
        | Upload(url, meth, data) ->
            HttpResult.Ok(client.UploadString(url, meth, data))
    with
    | :? WebException as error ->
        //printfn "Got exception: %A" error
        //printfn "Error status: %A" error.Status
        match error.Status with
        | WebExceptionStatus.Success | WebExceptionStatus.ProtocolError ->
            let response: HttpWebResponse = error.Response :?> HttpWebResponse
            let status: int = int response.StatusCode
            let message =
                let encoding = Encoding.UTF8
                use is = new System.IO.StreamReader(response.GetResponseStream(), encoding)
                is.ReadToEnd()
            //printfn "Web error: %d | %s" status message
            HttpResult.Error(message, status)
        | _ ->
            HttpResult.ConnectionError(sprintf "Connection error: %A" error)
    | error ->
        HttpResult.ConnectionError(sprintf "Connection error: %A" error)

// Sends a pastry message to a node at an address, that a type of message must
// be forwarded towards a pastry node, carrying some data
let send_message (address: NetworkLocation) (typ: MessageType) (message: string)
        (destination: GUID) (use_timeout: bool): HttpResult =
    // Sleep a little. This might prevent too fast local communication
    System.Threading.Thread.Sleep(10)
    match typ with
    | Resource(path, meth) ->
        let url = sprintf "http://%s/resource/%s" address path
        printfn "SEND: %s %s => %s" meth url message
        let action =
            match meth with
            | "GET" ->
                Download(url, message)
            | "PUT" | "POST" | "DELETE" ->
                Upload(url, meth, message)
            | _ ->
                failwith "ASSERTION FAILED: Got unknown HTTP method in pastry send_message!"
        send_http action use_timeout

    | Request(location, port, url, meth, data) ->
        let url = sprintf "http://%s/pastry/request/%s/%s/%s" address location port url
        let action =
            match meth with
            | "GET" ->
                Download(url, data)
            | "PUT" | "POST" | "DELETE" ->
                Upload(url, meth, data)
            | _ ->
                failwith "Unknown request type gotten: %s" meth
        send_http action use_timeout

    | Collect(url) ->
        let return_url = sprintf "http://%s/pastry/collect/%s" address url
        let action = Upload(return_url, "PUT", message)
        send_http action use_timeout

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
            | _             -> failwith "Got past a match on '%A'" typ
        let url = sprintf "http://%s/pastry/%s/%s" address cmd (serialize_guid destination)
        if not (typ = Backup) then
            printfn "SEND: %s => %s" url message
        send_http <| Upload(url, "PUT", message) <| use_timeout

// Returns the destination of a given split resource url (after the /resources/ part)
let get_destination (resource_url: string list): Destination =
    let path = String.concat "/" resource_url
    match resource_url with
    | "user"::name::[] ->
        Ok(hash <| sprintf "user/%s" name)
    | "user"::name::"roles"::workflow::event::[] ->
        Ok(hash <| sprintf "user/%s" name)
    | "user"::name::"roles"::workflow::[] ->
        Ok(hash <| sprintf "user/%s" name)
    | "user"::name::unwanted_stuff ->
        let a = "It should be on the form 'user/<username>'"
        let error_message = sprintf "UTILS: Bad user path '%s'. %s" path a
        Error(error_message, 400, "Invalid URL")
    | "workflow"::workflow::[] -> // create/get workflow
        Ok(hash (sprintf "workflow/%s" workflow))
    | "workflow"::workflow::event::[] -> // create event
        Ok(hash (sprintf "workflow/%s/%s" workflow event))
    | "workflow"::workflow::event::attribute::[] ->
        Ok(hash (sprintf "workflow/%s/%s" workflow event))
    | "workflow"::workflow::event::relation::dst::[] ->
        Ok(hash (sprintf "workflow/%s/%s" workflow event))
    | "workflow"::badly_formed_path ->
        let a = "It should be on the form '/resource/workflow/<name>[/<eventname>/<attribute>]'"
        let error_message = sprintf "UTILS: Bad workflow path '%s'.\n%s" path a
        Error(error_message, 400, "Invalid URL")
    | "log"::workflow::[] ->
        Ok(hash (sprintf "workflow/%s" workflow))
    | "log"::workflow::event::[] ->
        Ok(hash (sprintf "workflow/%s" workflow))
    | "debug"::workflow::[] ->
        Ok(hash (sprintf "workflow/%s" workflow))
    | unknown_resource::whatever ->
        let error_message = sprintf "UTILS: Unknown resource type: '%s'" unknown_resource
        Error(error_message, 404, "Not found")
    | _ ->
        Error("Whatever that is, it isn't here", 404, "Not found")
