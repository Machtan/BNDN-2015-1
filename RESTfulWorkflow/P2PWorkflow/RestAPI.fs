module Rest

open Repository_types
open UserLogic
open EventLogic
open WorkflowLogic
open Pastry

// Splits a string into a list by a char
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Attempts to get an event from the workflow
let getEvent event state =
    //match tryGet event state with
    //    | Ok nodeState -> ("Found", 200, "OK", Some(nodeState))
    //    | MissingEvent n -> (sprintf "'%s' was not Found" n, 404, "Not found", None)
    failwith "Not Implemented"

// Gets the 'executed' state of an event
let getExecuted event state =
    //let res = getEvent event state
    //let (msg, status, info, optState) = res
    //match optState with
    //| Some(executed, _, _, _) ->
    //    (string executed.IsSome), status, info, state
    //| None -> msg, status, info, state
    failwith "Not Implemented"

// Gets the 'included' state of an event
let getIncluded event state =
    //let res = getEvent event state
    //let (msg, status, info, optState) = res
    //match optState with
    //| Some(_, included, _, _) ->
    //    (string included), status, info, state
    //| None -> msg, status, info, state
    failwith "Not Implemented"

// Gets the 'executable' state of an event
let getExecutable event state =
    //let res = getEvent event state
    //let (msg, status, info, optState) = res
    //match optState with
    //| Some(_, _, excluded, _) ->
    //    (string excluded), status, info, state
    //| None -> msg, status, info, state
    failwith "Not Implemented"

// Gets the 'pending' state of an event
let getPending event state =
    //let res = getEvent event state
    //let (msg, status, info, optState) = res
    //match optState with
    //| Some(_, _, _, pending) ->
    //    (string pending), status, info, state
    //| None -> msg, status, info, state
    failwith "Not Implemented"

// Attempts to execute the given event
let setExecuted eventname role state =
    //match tryExecute eventname role state with
    //| ExecutionResult.Ok state' ->
    //    (sprintf "Executed '%s'!" eventname), 200, "OK", state
    //| ExecutionResult.Unauthorized ->
    //    (sprintf "The role '%s' cannot execute '%s'!" role eventname), 402, "Payment required", state
    //| ExecutionResult.NotExecutable ->
    //    (sprintf "The event '%s' is not executable (try /executable)" eventname), 403, "Forbidden", state
    //| ExecutionResult.MissingEvent name ->
    //    (sprintf "Could not execute '%s': '%s' was not Found" eventname name), 400, "Bad request", state
    failwith "Not Implemented"

// Attempts to create a new event
let createEvent event body state =
    //let args = split body ' '
    //let str = List.head args
    //let roles = List.tail args
    //if not (str.Length = 3) then
    //    let msg = sprintf "Received invalid initial event state: %s" str
    //    printfn "%s" msg
    //    msg, 400, "Bad request", state
    //else
    //    let initialState = (str.[0] = '1', str.[1] = '1', str.[2] = '1')
    //    (sprintf "'%s' created!" event), 201, "Created", create event roles initialState state
    failwith "Not Implemented"

// Adds a new relation
let addRelation eventname typ dest state =
    //let rel =
    //    match typ with
    //    | "exclusion"   -> Some (Exclusion dest)
    //    | "condition"   -> Some (Dependent dest)
    //    | "response"    -> Some (Response dest)
    //    | "inclusion"   -> Some (Inclusion dest)
    //    | _             -> None
    //match rel with
    //| None -> (sprintf "Unknown relation type '%s'" typ), 400, "Bad request", state
    //| Some relation ->
    //    match tryAdd eventname relation state with
    //    | UpdateResult.Ok state' ->
    //        (sprintf "Added '%s' to '%s'" typ eventname), 200, "Ok", state'
    //    | UpdateResult.MissingEvent e ->
    //        (sprintf "Could not add relation to '%s': Missing Event '%s'" eventname e), 400, "Bad request", state
    failwith "Not Implemented"

// Creates a new repository
let create_repository () : Repository =
    { events = Map.empty; users = Map.empty; workflows = Map.empty; logs = []; }

// Serializes the permissions of a given user for HTTP transfer
let serialize_user_permissions (user: User) : string =
    "Not Implemented"

// Matches the given user and
let handle_user (user_name: string) (meth: string) (repo: Repository) : ResourceResponse<Repository> =
    match meth with
    | "POST" -> // Create a new user
        failwith "Not Implemented"
    | "DELETE" -> // Delete a user
        failwith "Not Implemented"
    | "GET" -> // Get the workflow permissions of a user
        match Map.tryFind user_name repo.users with
        | None ->
            repo, "User not found", 404
        | Some(user) ->
            repo, serialize_user_permissions user, 200
    | _ ->
        repo, "Unsupported user operation"

// So amazing
let SEPARATOR = "\n"

// Handles requests for the givien workflow (getting the events in it)
let handle_workflow (wf: string) (meth: string) (repo: Repository) : ResourceResponse<Repository> =
    match meth with
    | "GET" ->
        failwith "Not Implemented"
    | "POST" ->
        failwith "Not Implemented"
    | "DELETE" ->
        failwith "Not Implemented"
    | _ ->
        repo, "Unsupported workflow operation", 400

// Matches the given event and tries to handle the request
let handle_event (workflow_name: string) (event_name: string) (attribute: string)
        (meth: string) (message: string) (repo: Repository) : ResourceResponse<Repository> =
    // Find the event in this repository if it exists
    let response =
        match meth, attribute with
        | "POST", "" ->
            failwith "Not Implemented"//createEvent event body initialState
        | "DELETE", "" ->
            failwith "Not Implemented"
        | "GET", "executed" ->
            failwith "Not Implemented"//getExecuted event initialState
        | "PUT", "executed" ->
            failwith "Not Implemented"//setExecuted event body initialState
        | "GET", "pending" ->
            failwith "Not Implemented"//getPending event initialState
        | "GET", "included" ->
            failwith "Not Implemented"// getIncluded event initialState
        | "GET", "executable" ->
            failwith "Not Implemented"//getExecutable event initialState
        | "POST", relation ->
            failwith "Not Implemented" //addRelation event relation dest state
        | _ ->
            failwith "Not Implemented"//"Unsupported operation", 404, "Not found", initialState
    response

// The actual resource handling function
let resource_handler (path: string) (meth: string) (message: string) (send_func: SendFunc<Repository>)
        (initial_state: Repository) : ResourceResponse<Repository> =

    let parts = split path '/'
    let response =
        //printfn "Parts: %A" parts
        match parts with
        | [] ->
            initial_state, "Nothing asked, nothing found."
        | "user"::user::[] ->
            handle_user user meth initial_state

        | "workflow"::workflow::[] ->
            handle_workflow workflow meth initial_state

        | "workflow"::workflow::event::attribute::[] ->
            handle_event workflow event attribute meth initial_state
        | _ ->
            printfn "Invalid path gotten: %s" path
            initial_state, "Invalid path", 400
    response

[<EntryPoint>]
let main args =
    match args with
        | [|addr; port; peer|] ->
            let address = sprintf "%s:%s" (if addr = "" then "localhost" else addr) port
            printfn "? Joining pastry network at '%s'..." address
            let known_peer = if peer = "" then None else Some(peer)

            let node = start_server address known_peer resource_handler <| create_repository()
            0
            // Start listening...
        | _ ->
            printfn "Usage: Pastry.exe <address> <port> <peer_address_with_port>"
            1
