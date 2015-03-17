module Event
open Workflow

let split (str: string) (c: char) = List.ofArray (str.Split([|c|]))

// Attempts to get an event from the workflow
let getEvent event state =
    match tryGet event state with
    | Some( nodeState ) ->
        (Some(nodeState), 200, "OK")
    | None ->
        (None, 404, "Not Found")

// Gets the 'executed' state of an event
let getExecuted event state =
    match getEvent event state with
    | (None, status, msg) ->
        sprintf "Event '%s' not found" event, status, msg, state
    | (Some(executed, _, _), status, msg) -> (string executed), status, msg, state

// Gets the 'included' state of an event
let getIncluded event state =
    match getEvent event state with
    | (None, status, msg) ->
        sprintf "Event '%s' not found" event, status, msg, state
    | (Some(_, included, _), status, msg) -> (string included), status, msg, state

// Gets the 'pending' state of an event
let getPending event state =
    match getEvent event state with
    | (None, status, msg) ->
        sprintf "Event '%s' not found" event, status, msg, state
    | (Some(_, _, pending), status, msg) -> (string pending), status, msg, state

// Attempts to execute the given event
let setExecuted event state =
    match tryExecute event state with
    | None -> (sprintf "'%s' could not be executed!" event), 404, "Error", state
    | Some( state' ) -> "Executed!", 200, "OK", state'

// Adds a new relation
let addRelation event typ dest state =
    let msg, res =
        match typ with
        | "exclusion" -> "Added!", tryAdd event (Exclusion dest) state
        | "condition" -> "Added!", tryAdd event (Dependent dest) state
        | "response" -> "Added!", tryAdd event (Response dest) state
        // TODO IMPLEMENT
        | "include" -> "Not implemented, sorry...", None
        | _ -> "Could not find this relation type", None
    match res with
    | Some(state') -> msg, 200, "OK", state'
    | None -> msg, 404, "Error", state

[<EntryPoint>]
let main args =
    let workflow, port =
        match args with
        | [||] -> failwith "No event argument given for the node"
        | [|n; p|] -> n, int p
        | _ -> failwith "Too many arguments"

    // CONFIG HERE!
    let basepath = sprintf "http://localhost:%d/%s/" port workflow

    printfn "Starting node at %s" basepath

    use hl = new System.Net.HttpListener ()
    hl.Prefixes.Add basepath
    hl.Start ()
    printfn "Listener up @ 0.0.0.0:%d" port

    // Set up the state
    let state = Map.empty

    let rec loop node =
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

        printfn "%s %s from %s %d [%s]" meth path (rip.ToString()) port body

        let reply answer status reason node' =
            printfn " --> %d %s [%s]" status reason answer
            // Set HTTP statuscode and reason (e.g., 200 OK)
            response.StatusCode <- status
            response.StatusDescription <- reason
            // Encode and write body.
            let buffer = System.Text.Encoding.UTF8.GetBytes(answer : string)
            response.ContentLength64 <- int64 buffer.Length;
            response.OutputStream.Write(buffer,0,buffer.Length);
            response.OutputStream.Close();
            loop node'

        // Split the path into parts
        let parts = split (path.[1..]) '/'
        let msg, status, info, state' =
            match parts with
            | [] -> "Nothing asked, nothing found.", 404, "Not found", state
            | wfevent::event::apath ->
                printfn "Workflow: %s Args: %A" wfevent apath
                if not (wfevent = workflow) then
                    reply "There is no workflow with that event here D:" 404 "Not Found" state
                else
                    match meth, apath with
                    | "GET", ["executed"]->
                        getExecuted event state
                    | "PUT", ["executed"] ->
                        setExecuted event state
                    | "GET", ["pending"]->
                        getPending event state
                    | "GET", ["included"] ->
                        getIncluded event state
                    | "CREATE", [relation] ->
                        addRelation event relation body state
                    | _ ->
                        "Unsupported operation", 404, "Not found", state
            | _ -> "Path is too short", 404, "Not found", state
        reply msg status info state'

    loop state |> ignore


    // Dead code.
    0

