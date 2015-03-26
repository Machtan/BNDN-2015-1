module EventTing
open Workflow

let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Attempts to get an event from the workflow
let getEvent event state =
    match tryGet event state with
    | Ok nodeState -> ("Found", 200, "OK", Some(nodeState))
    | MissingEvent n -> (sprintf "'%s' was not Found" n, 404, "Not found", None)

// Gets the 'executed' state of an event
let getExecuted event state =
    let res = getEvent event state
    let (msg, status, info, optState) = res
    match optState with
    | Some(executed, _, _, _) ->
        (string executed.IsSome), status, info, state
    | None -> msg, status, info, state

// Gets the 'included' state of an event
let getIncluded event state =
    let res = getEvent event state
    let (msg, status, info, optState) = res
    match optState with
    | Some(_, included, _, _) ->
        (string included), status, info, state
    | None -> msg, status, info, state

// Gets the 'executable' state of an event
let getExecutable event state =
    let res = getEvent event state
    let (msg, status, info, optState) = res
    match optState with
    | Some(_, _, excluded, _) ->
        (string excluded), status, info, state
    | None -> msg, status, info, state

// Gets the 'pending' state of an event
let getPending event state =
    let res = getEvent event state
    let (msg, status, info, optState) = res
    match optState with
    | Some(_, _, _, pending) ->
        (string pending), status, info, state
    | None -> msg, status, info, state

// Attempts to execute the given event
let setExecuted eventname role state =
    match tryExecute eventname role state with
    | ExecutionResult.Ok state' ->
        (sprintf "Executed '%s'!" eventname), 200, "OK", state'
    | ExecutionResult.Unauthorized ->
        (sprintf "The role '%s' cannot execute '%s'!" role eventname), 402, "Payment required", state
    | ExecutionResult.NotExecutable ->
        (sprintf "The event '%s' is not executable (try /executable)" eventname), 403, "Forbidden", state
    | ExecutionResult.MissingEvent name ->
        (sprintf "Could not execute '%s': '%s' was not Found" eventname name), 400, "Bad request", state

// Attempts to create a new event
let createEvent event role state =
    (sprintf "'%s' created!" event), 201, "Created", create event role state

// Adds a new relation
let addRelation eventname typ dest state =
    let rel =
        match typ with
        | "exclusion"   -> Some (Exclusion dest)
        | "condition"   -> Some (Dependent dest)
        | "response"    -> Some (Response dest)
        | "include"     -> Some (Inclusion dest)
        | _             -> None
    match rel with
    | None -> (sprintf "Unknown relation type '%s'" typ), 400, "Bad request", state
    | Some relation ->
        match tryAdd eventname relation state with
        | UpdateResult.Ok state' ->
            (sprintf "Added '%s' to '%s'" typ eventname), 200, "Ok", state'
        | UpdateResult.MissingEvent e ->
            (sprintf "Could not add relation to '%s': Missing Event '%s'" eventname e), 400, "Bad request", state

// Gets a list of the events in the workflow
let getEvents role state =
    let sep = " "
    let events = getEventNames role state
    let msg = List.foldBack (fun x acc -> acc + sep + x ) events ""
    (if msg = "" then "" else msg.[(sep.Length)..]), 200, "OK", state

// Starts a workflow server at the given port
let start_server workflow port =
    let basepath = (sprintf "http://localhost:%d/%s/" port workflow)

    printfn "Starting node at %s" basepath

    use hl = new System.Net.HttpListener ()
    hl.Prefixes.Add basepath
    hl.Start ()
    printfn "Listener up @ 0.0.0.0:%d" port

    // Set up the state
    let initialState = Map.empty

    // The listener loop
    let rec loop state =
        let cxt      = hl.GetContext()
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
            //printfn "Parts: %A" parts
            match parts with
            | [] ->
                "Nothing asked, nothing found.", 404, "Not found", state
            | [wfevent] ->
                printfn "wfevent: %A + %A" wfevent args
                let role = args.Get "role"
                printfn "role: %s" role
                let action = args.Get "action"
                if action = "reset" then
                    "Resetting...", 200, "Ok", Map.empty
                else
                    if (wfevent = workflow) && (meth = "GET" ) then
                        getEvents role state
                    else
                        "There is no workflow like that here D: (yet)", 404, "Not Found", state
            | wfevent::event::apath ->
                //printfn "Workflow: %s Args: %A" wfevent apath
                if not (wfevent = workflow) then
                    "There is no workflow with that event here D:", 404, "Not Found", state
                else
                    match meth, apath with
                    | "POST", [] ->
                        createEvent event body state
                    | "GET", ["executed"] ->
                        getExecuted event state
                    | "PUT", ["executed"] ->
                        setExecuted event body state
                    | "GET", ["pending"] ->
                        getPending event state
                    | "GET", ["included"] ->
                        getIncluded event state
                    | "POST", [relation] ->
                        addRelation event relation body state
                    | _ ->
                        "Unsupported operation", 404, "Not found", state
            //| _ -> "Path is too short", 404, "Not found", state
        reply msg status info state'

    loop initialState |> ignore

[<EntryPoint>]
let main args =
    match args with
    | [|workflow; port|] ->
        ignore (start_server workflow (int port))
    | _ ->
        printfn "Usage: Event.exe <workflow> <port>"
    0
