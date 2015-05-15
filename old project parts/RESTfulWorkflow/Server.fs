// --debois, Mar '15
module Server

// REST key-value store. 

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

[<EntryPoint>]
let main args = 
    let port = 8080;
    use hl = new System.Net.HttpListener ()
    hl.Prefixes.Add <| sprintf "http://+:%d/" port
    // Host '+' means 'accept any path on this path.
    // https://msdn.microsoft.com/en-us/library/system.net.httplistenerprefixcollection.add(v=vs.110).aspx
    hl.Start ()
    printfn "Listener up @ 0.0.0.0:%d" port

   
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

