// --debois, Mar '15
module Server

// REST key-value store. 

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

type Relation =             // The internal string is a http address
    | Condition of string   // What is enabled once a node is executed
    | Exclusion of string   // What becomes excluded once this node is executed
    | Response of string    // What becomes pending once a node is executed

type Node = {
    executed: bool;
    included: bool;
    pending: bool;
    relations: Relation list;
    dependencies: Map<string, bool>; // Whether each condition is fulfilled
    // Ex: {"register": false}
}

// Add a relation to the node
let addRelation node relation =
    failwith "Not implemented"

// Add a dependency to the node
let addDependency node dependency =
    failwith "Not Implemented"

// Notify the node that a dependency has been executed
let setExecuted node dependency =
    failwith  "Not Implemented"

// Notify the node that it has become pending
let setPending node =
    failwith  "Not Implemented"

// Handles when the node is executed
let tryExecute node =
    let rec notify remainingRelations =
        failwith  "Not Implemented"
    failwith  "Not Implemented"

// Returns the status of the node (Should this be here?)
// Note: This should not return the node
let getStatus node =
    failwith  "Not Implemented"

// Ezcludes the node
let exclude node =
    failwith  "Not Implemented"

[<EntryPoint>]
let main args = 
    let name = 
        match args with
        | [||] -> failwith "No name argument given for the node"
        | [|arg|] -> arg
        | _ -> failwith "Too many arguments"
        
    printfn "Starting node '%s'!" name
    let port = 8080;
    use hl = new System.Net.HttpListener ()
    hl.Prefixes.Add <| sprintf "http://+:%d/" port
    // Host '+' means 'accept any path on this path.
    // https://msdn.microsoft.com/en-us/library/system.net.httplistenerprefixcollection.add(v=vs.110).aspx
    hl.Start ()
    printfn "Listener up @ 0.0.0.0:%d" port
    
    let mut state: Node = {
        executed = false;
        included = true;
        pending = false;
        relations = [];
        dependencies = Map.empty;
    }
   
    let rec loop (store : Map<string, string>) = 
        let cxt      = hl.GetContext()
        let request  = cxt.Request
        let response = cxt.Response
        let meth     = request.HttpMethod
        let path     = request.RawUrl
        let rip      = request.RemoteEndPoint.Address
        let rport    = request.RemoteEndPoint.Port
        let body = 
            use is = new System.IO.StreamReader(request.InputStream, request.ContentEncoding)
            is.ReadToEnd()

        printfn "%s %s from %s %d [%s]" meth path (rip.ToString()) rport body

        let reply answer status reason store' = 
            printfn " --> %d %s [%s]" status reason answer
            // Set HTTP statuscode and reason (e.g., 200 OK)
            response.StatusCode <- status           
            response.StatusDescription <- reason   
            // Encode and write body. 
            let buffer = System.Text.Encoding.UTF8.GetBytes(answer : string)
            response.ContentLength64 <- int64 buffer.Length;
            response.OutputStream.Write(buffer,0,buffer.Length);
            response.OutputStream.Close();
            loop store'
 
        match meth, path with 
        | "GET", Prefix "/resource/" r -> 
            match Map.tryFind r store with
            | None -> reply "No such resource" 404 "Not Found" store
            | Some v -> reply v 200 "OK" store

        | "PUT", Prefix "/resource/" r -> 
            match Map.tryFind r store with
            | None -> reply "No such resource" 404 "Not Found" store
            | Some _ -> reply "Updated" 200 "OK" (Map.add r body store)
        
        | "DELETE", Prefix "/resource/" r -> 
            match Map.tryFind r store with
            | None -> reply "No such resource" 404 "Not Found" store
            | Some _ -> reply "Deleted" 200 "OK" (Map.remove r store)
                      
        | "CREATE", Prefix "/resource/" r -> 
            match Map.tryFind r store with
            | Some _ -> reply "Already exists." 409 "Conflict" store
            | None -> reply "Created" 200 "OK" (Map.add r body store)

        | _ -> reply "Not found (path)." 404 "Not found" store
    
    loop Map.empty
    
    // Dead code.
    0

