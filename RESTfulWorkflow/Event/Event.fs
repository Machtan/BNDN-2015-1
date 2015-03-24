module Event
open Workflow

let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

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
        (sprintf "Event '%s' not found" event), status, msg, state
    | (Some(executed, _, _), status, msg) ->
        (string executed), status, msg, state

// Gets the 'included' state of an event
let getIncluded event state =
    match getEvent event state with
    | (None, status, msg) ->
        (sprintf "Event '%s' not found" event), status, msg, state
    | (Some(_, included, _), status, msg) ->
        (string included), status, msg, state

// Gets the 'pending' state of an event
let getPending event state =
    match getEvent event state with
    | (None, status, msg) ->
        (sprintf "Event '%s' not found" event), status, msg, state
    | (Some(_, _, pending), status, msg) ->
        (string pending), status, msg, state

// Attempts to execute the given event
let setExecuted event role state =
    match tryExecute event role state with
    | None ->
        (sprintf "'%s' could not be executed!" event), 404, "Error", state
    | Some( state' ) ->
        "Executed!", 200, "OK", state'

// Attempts to create a new event
let createEvent event role state =
    match (tryCreate event role state) with
    | None ->
        (sprintf "Already created!"), 200, "OK", state
    | Some state' ->
        (sprintf "'%s' created!" event), 200, "OK", state'

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
            //printfn "Parts: %A" parts
            match parts with
            | [] ->
                "Nothing asked, nothing found.", 404, "Not found", state
            | [wfevent] ->
                //printfn "wfevent: %A" wfevent
                if (wfevent = workflow) && (meth = "GET" ) then
                    getEvents body state
                else
                    reply "There is no workflow like that here D: (yet)" 404 "Not Found" state
            | wfevent::event::apath ->
                printfn "Workflow: %s Args: %A" wfevent apath
                if not (wfevent = workflow) then
                    reply "There is no workflow with that event here D:" 404 "Not Found" state
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
