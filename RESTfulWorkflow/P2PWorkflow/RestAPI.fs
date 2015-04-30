module Rest
open System
open WorkflowOld 
open System.Net

type event = string
type role = string
type eventBody = string  
type included = bool
type executable = bool
type pending = bool
type eventState = DateTime option * included * executable * pending
type httpResponse = string * int * string * eventState option
type httpResponseNoState = string * int * string * Workflow
type restResponse = string * int * string
type typ = string
type dest = string
type url = string

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
        (sprintf "Executed '%s'!" eventname), 200, "OK", state
    | ExecutionResult.Unauthorized ->
        (sprintf "The role '%s' cannot execute '%s'!" role eventname), 402, "Payment required", state
    | ExecutionResult.NotExecutable ->
        (sprintf "The event '%s' is not executable (try /executable)" eventname), 403, "Forbidden", state
    | ExecutionResult.MissingEvent name ->
        (sprintf "Could not execute '%s': '%s' was not Found" eventname name), 400, "Bad request", state

// Attempts to create a new event
let createEvent event body state =
    let args = split body ' '
    let str = List.head args
    let roles = List.tail args
    if not (str.Length = 3) then
        let msg = sprintf "Received invalid initial event state: %s" str
        printfn "%s" msg
        msg, 400, "Bad request", state
    else
        let initialState = (str.[0] = '1', str.[1] = '1', str.[2] = '1')
        (sprintf "'%s' created!" event), 201, "Created", create event roles initialState state

// Adds a new relation
let addRelation eventname typ dest state =
    let rel =
        match typ with
        | "exclusion"   -> Some (Exclusion dest)
        | "condition"   -> Some (Dependent dest)
        | "response"    -> Some (Response dest)
        | "inclusion"   -> Some (Inclusion dest)
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
let ProcessRestCall (request : HttpListenerRequest)  =
        let meth     = request.HttpMethod
        let args     = request.QueryString
        let path     = request.Url.AbsolutePath
        let rip      = request.RemoteEndPoint.Address
        let rport    = request.RemoteEndPoint.Port
        let body =
            use is = new System.IO.StreamReader(request.InputStream, request.ContentEncoding)
            is.ReadToEnd()

        // Set up the state
        let initialState = Map.empty 
                
        // Split the path into parts
        let parts = split (path.[1..]) '/'
        let msg, status, info, state =
            //printfn "Parts: %A" parts
            match parts with
            | [] ->
                "Nothing asked, nothing found.", 404, "Not found", initialState
            | workflow::event::apath ->
                    match meth, apath with
                    | "POST", [] ->
                        createEvent event body initialState
                    | "GET", ["executed"] ->
                        getExecuted event initialState
                    | "PUT", ["executed"] ->
                        setExecuted event body initialState
                    | "GET", ["pending"] ->
                        getPending event initialState
                    | "GET", ["included"] ->
                        getIncluded event initialState
                    | "GET", ["executable"] ->
                        getExecutable event initialState
                    | "POST", [relation] ->
                        addRelation event relation body initialState
                    | _ ->
                        "Unsupported operation", 404, "Not found", initialState
            | _ ->
                "Unsupported operation", 404, "Not found", initialState 
        (msg, status, info, state)

