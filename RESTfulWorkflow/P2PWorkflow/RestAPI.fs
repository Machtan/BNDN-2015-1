module Rest

open Repository_types
open UserLogic
open EventLogic
open WorkflowLogic
open Pastry
open PastryTypes
open Migrate
open Newtonsoft.Json

// Splits a string into a list by a char
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Attempts to get an event from the workflow
let getEvent workflowName eventName repository : (EventState option * string * int) =
        let event = (workflowName,eventName) : EventName
        let response = get_event_state event repository
        Some(response),"Ok", 200

// Gets the 'executed' state of an event
let getExecuted event workflowName repo : ResourceResponse<Repository> =
    let response = getEvent workflowName event repo
    let (eventState,message,statusCode) = response
    match (eventState) with
        | Some(evState) -> (repo,(string)evState.executed, statusCode)
        | None ->  (repo,"Event could not be received.", statusCode)

// Gets the 'included' state of an event
let getIncluded event workflowName repo : ResourceResponse<Repository>  =
    let response = getEvent workflowName event repo
    let (eventState,message,statusCode) = response
    match (eventState) with
        | Some(evState) -> (repo,(string)evState.included, statusCode)
        | None ->  (repo,"Event could not be received.", statusCode)

// Gets the 'executable' state of an event
let getExecutable event workflowName repo sendfunc  : ResourceResponse<Repository> =
    let response = getEvent workflowName event repo
    let (eventState,message,statusCode) = response
    match (eventState) with
        | Some(evState) ->  (repo,string (check_if_executeble (workflowName,event) sendfunc repo), statusCode)
        | None ->  (repo,"Event could not be received.", statusCode)

// Gets the 'pending' state of an event
let getPending event workflowName repo : ResourceResponse<Repository> =
    let response = getEvent workflowName event repo
    let (eventState,message,statusCode) = response
    match (eventState) with
        | Some(evState) -> (repo,(string)evState.pending, statusCode)
        | None ->  (repo,"Event could not be received.", statusCode)

// Attempts to execute the given event
let setExecuted workflowName eventName userName repo sendFunc :ResourceResponse<Repository>  =
    let event = (workflowName,eventName): EventName
    let response = execute event userName sendFunc repo
    match (response) with
            | Result.Ok(r) -> (r,"Executed", 201)
            | Result.Unauthorized -> (repo,"Unauthorized", 401)
            | Result.NotExecutable -> (repo,"The event is not executable.", 400)
            | Result.MissingRelation -> (repo,"The relation is missing.", 400)
            | Result.LockConflict -> (repo,"Encountered LockConflict.", 400)
            | Result.MissingEvent -> (repo,"Event is missing", 400)
            | Result.MissingWorkflow -> (repo,"Workflow is missing", 400)
            | Result.Error(s) -> (repo,s, 400)

// Attempts to create a new event
let deleteEvent eventName workflowName (repository : Repository) sendFunc : ResourceResponse<Repository> =
        let event = (workflowName,eventName): EventName
        let response = delete_event event sendFunc repository
        match (response) with
            | Result.Ok(r) -> (r,"Deleted.", 200)
            | Result.Unauthorized -> (repository,"Unauthorized", 401)
            | Result.NotExecutable -> (repository,"The event is not executable.", 400)
            | Result.MissingRelation -> (repository,"The relation is missing.", 400)
            | Result.LockConflict -> (repository,"Encountered LockConflict.", 400)
            | Result.MissingEvent -> (repository,"Event is missing", 400)
            | Result.MissingWorkflow -> (repository,"Workflow is missing", 400)
            | Result.Error(s) -> (repository,s, 400)

let createEvent (eventName: string) (workflowName: string) (message: string)
        (repository : Repository) : ResourceResponse<Repository> =
    let args = split message ','
    let state_str = List.head args
    let roles = List.tail args
    if not (state_str.Length = 4) then
        let msg = sprintf "Received invalid initial event state: %s" state_str
        printfn "%s" msg
        (repository, msg, 400)
    else
        let des index =
            state_str.[index] = '1'
        let initialState = {included = des 0; pending = des 1; executed = des 2} : EventState
        let locked = des 3
        let event = (workflowName,eventName): EventName
        let response = create_event event initialState locked repository
        match (response) with
            | Result.Ok(r) -> (r,"Created.", 201)
            | Result.Unauthorized -> (repository,"Unauthorized", 401)
            | Result.NotExecutable -> (repository,"The event is not executable.", 400)
            | Result.MissingRelation -> (repository,"The relation is missing.", 400)
            | Result.LockConflict -> (repository,"Encountered LockConflict.", 400)
            | Result.MissingEvent -> (repository,"Event is missing", 400)
            | Result.MissingWorkflow -> (repository,"Workflow is missing", 400)
            | Result.Error(s) -> (repository,s, 400)

// Adds a new relation
let addRelation (relation: RelationType) (connection: Connection)
        (send_func: SendFunc<Repository>) (repo: Repository)
        : ResourceResponse<Repository>=
    let response =
        match connection with
        | From(a, b) -> add_relation_to a relation b send_func repo
        | To  (a, b) -> add_relation_from a relation b repo
    match (response) with
    | Result.Ok(repo')          -> (repo', "Created.", 201)
    | Result.Unauthorized       -> (repo, "Unauthorized", 401)
    | Result.NotExecutable      -> (repo, "The event is not executable.", 400)
    | Result.MissingRelation    -> (repo, "The relation is missing.", 400)
    | Result.LockConflict       -> (repo, "Encountered LockConflict.", 400)
    | Result.Error(error)       -> (repo, error, 400)
    | Result.MissingEvent       -> (repo, "Event is missing", 400)
    | Result.MissingWorkflow    -> (repo, "Workflow is missing", 400)

// Deletes a relation
let delete_relation (relation: RelationType) (connection: Connection)
        (send_func: SendFunc<Repository>) (repo: Repository)
        : ResourceResponse<Repository>=
    let response =
        match connection with
        | From(a, b) -> remove_relation_to a relation b send_func repo
        | To  (a, b) -> remove_relation_from a relation b send_func repo
    match (response) with
    | Result.Ok(repo')          -> (repo', "Created.", 201)
    | Result.Unauthorized       -> (repo, "Unauthorized", 401)
    | Result.NotExecutable      -> (repo, "The event is not executable.", 400)
    | Result.MissingRelation    -> (repo, "The relation is missing.", 400)
    | Result.LockConflict       -> (repo, "Encountered LockConflict.", 400)
    | Result.Error(error)       -> (repo, error, 400)
    | Result.MissingEvent       -> (repo, "Event is missing", 400)
    | Result.MissingWorkflow    -> (repo, "Workflow is missing", 400)

let createWorkflow workflowName repo  : ResourceResponse<Repository> =
    let response = create_workflow workflowName repo
    match (response) with
    | Result.Ok(r) -> (r,"Created.", 201)
    | Result.Unauthorized -> (repo,"Unauthorized", 401)
    | Result.NotExecutable -> (repo,"The event is not executable.", 400)
    | Result.MissingRelation -> (repo,"The relation is missing.", 400)
    | Result.LockConflict -> (repo,"Encountered LockConflict.", 400)
    | Result.MissingEvent -> (repo,"Event is missing", 400)
    | Result.MissingWorkflow -> (repo,"Workflow is missing", 400)
    | Result.Error(s) -> (repo,s, 400)

let createUser userName repo  : ResourceResponse<Repository> =
    let response = create_user userName repo
    match (response) with
    | ResultUser.Ok(r) -> (r,"Created.", 201)
    | ResultUser.Unauthorized -> (repo,"Unauthorized", 401)
    | ResultUser.NotExecutable -> (repo,"The event is not executable.", 400)
    | ResultUser.MissingUser -> (repo,"user is missing", 400)

let deleteUser userName repo  : ResourceResponse<Repository> =
    let response = delete_user userName repo
    match (response) with
    | ResultUser.Ok(w) -> (repo,"Deleted.", 200)
    | ResultUser.Unauthorized -> (repo,"Unauthorized", 401)
    | ResultUser.NotExecutable -> (repo,"The event is not executable.", 400)
    | ResultUser.MissingUser -> (repo,"The relation is missing.", 400)

// Creates a new repository
let create_repository () : Repository =
    { events = Map.empty; users = Map.empty; workflows = Map.empty; }

// Serializes the permissions of a given user for HTTP transfer
let serialize_user_permissions (user: User) : string =
    "Not Implemented"

// Matches the given user and
let handle_user (user_name: string) (meth: string) (repo: Repository) : ResourceResponse<Repository> =
    match meth with
    | "POST" -> // Create a new user
        createUser user_name repo
    | "DELETE" -> // Delete a user
        deleteUser user_name repo
    | "GET" -> // Get the workflow permissions of a user
        match Map.tryFind user_name repo.users with
        | None ->
            repo, "User not found", 404
        | Some(user) ->
            repo, serialize_user_permissions user, 200
    | _ ->
        repo, "Unsupported user operation", 400

// Handles requests for the givien workflow (getting the events in it)
let handle_workflow (workflow: string) (meth: string) (data: string) (repo: Repository)
        : ResourceResponse<Repository> =
    match meth with
    | "GET" ->
        match get_workflow_events workflow repo with
        | Some(events) ->
            repo, (String.concat "," events), 200
        | None ->
            let msg = sprintf "The workflow '%s' was not found!" workflow
            repo, msg, 404

    | "POST" ->
        createWorkflow workflow repo

    | "DELETE" ->
        match delete_workflow workflow repo with
        | Some(updated_state) ->
            updated_state, "Deleted", 200
        | None ->
            let msg = sprintf "The workflow '%s' was not found!" workflow
            repo, msg, 404

    | "PUT" ->
        match add_event_to_workflow (workflow, data) repo with
        | Some(updated_state) ->
            updated_state, "Event added!", 200
        | None ->
            let msg = sprintf "The workflow '%s' was not found!" workflow
            repo, msg, 404
    | _ ->
        repo, "Unsupported workflow operation", 400

// Matches the given event and tries to handle the request
let handle_event (workflow_name: string) (event_name: string) (attribute: string)
        (meth: string) (message: string) (repo: Repository)
        (sendFunc : SendFunc<Repository>) : ResourceResponse<Repository> =
    // Find the event in this repository if it exists
    match meth, attribute with
    | "POST", "" ->
        createEvent event_name workflow_name message repo
    | "DELETE", "" ->
        deleteEvent event_name workflow_name repo sendFunc
    | "GET", "executed" ->
        getExecuted event_name workflow_name repo
    | "PUT", "executed" ->
        setExecuted workflow_name event_name message repo sendFunc
    | "GET", "pending" ->
        getPending event_name workflow_name repo
    | "GET", "included" ->
        getIncluded event_name workflow_name repo
    | "PUT", "lock" ->
        match lock_event (workflow_name, event_name) repo with
        | Result.Ok(updated_state) ->
            updated_state, "Locked!", 200
        | Result.LockConflict ->
            repo, "Event already locked!", 400
        | Result.MissingEvent ->
            repo, "Event not found", 404
        | err ->
            repo, (sprintf "Got error %A" err), 400
    | "PUT", "unlock" ->
        match unlock_event (workflow_name, event_name) repo with
        | Result.Ok(updated_state) ->
            updated_state, "Unlocked!", 200
        | err ->
            repo, (sprintf "Got error %A" err), 400
    | "GET", "executable" ->
        getExecutable event_name workflow_name repo sendFunc
    | _ ->
        (repo, "Unknown event command gotten!", 400)

// Handles a request about the relation of an event
let handle_relation (workflow_name: string) (event_name: string) (reltype: string)
        (con_str: string) (meth: string) (message: string) (repo: Repository)
        (sendFunc : SendFunc<Repository>) : ResourceResponse<Repository> =
    let args = split message ','
    if not ((List.length args) = 2) then
        let msg = sprintf "Invalid arguments: %A should be on the from <workflow>,<event>" message
        (repo, msg, 400)
    else
        let rel =
            match reltype with
            | "exclusion"   -> Some(Exclusion)
            | "condition"   -> Some(Condition)
            | "response"    -> Some(Response)
            | "inclusion"   -> Some(Inclusion)
            | _             -> None

        let data = (args.[0], args.[1])

        let con =
            match con_str with
            | "to"      -> Some( To((workflow_name, event_name), data) )
            | "from"    -> Some( From(data, (workflow_name, event_name)) )
            | _         -> None

        match rel, con with
        | Some(relation), Some(connection) ->
            match meth with
            | "PUT" ->
                addRelation relation connection sendFunc repo

            | "DELETE" ->
                delete_relation relation connection sendFunc repo

            | _ ->
                (repo, "Invalid method for adding a relation", 400)

        | None, _ ->
            let msg = sprintf "Invalid relation type '%s'" reltype
            (repo, msg, 400)

        | _, None ->
            let msg = sprintf "Invalid connection '%s' should be 'to' or 'from'" con_str
            (repo, msg, 400)


// Handles a full migration (a node has died, and is being remade)
let handle_full_migration (meth: string) (data: string)
        (send_func: SendFunc<Repository>) (initial_state: Repository)
        : ResourceResponse<Repository> =
    if data = "" then
        (initial_state, "Nothing to migrate", 200)
    else
        match meth with
        | "PUT" ->
            printfn ">>> Migrating........"
            let dead_repo = JsonConvert.DeserializeObject<Repository> data
            let cmds = get_all_migration_commands dead_repo
            let migrate cmd state =
                let (new_state, resp, status) = send_func cmd.path cmd.meth cmd.data state
                new_state
            let updated_state = List.foldBack migrate cmds initial_state
            printfn ">>> Finished migrating!"
            (updated_state, "Migrated!", 200)
        | _ ->
            (initial_state, "Bad method used: migrate requires PUT!", 400)

// Handles the migration of resources on this repository that are closer to
// a newly-joined node
let handle_partial_migration (meth: string) (from_guid: string) (to_guid: string)
        (send_func: SendFunc<Repository>) (initial_state: Repository)
        : ResourceResponse<Repository> =
    match meth with
    | "PUT" ->
        let should_migrate path = belongs_on_other from_guid path to_guid
        let (updated_state, cmds) = get_migratable_commands initial_state should_migrate
        let migrate cmd state =
            let (new_state, resp, status) = send_func cmd.path cmd.meth cmd.data state
            new_state
        let final_state = List.foldBack migrate cmds updated_state
        (final_state, "Migrated!", 200)

    | _ ->
        (initial_state, "Bad method used: migrate requires PUT!", 400)

// Handles requests related to logs
let handle_log (workflow: string) (event: string) (meth: string) (data: string)
        (repo: Repository) : ResourceResponse<Repository> =
    match (meth, event) with
    | "PUT", "" ->
        (repo, "No event given for log request!", 400)
    | "PUT", ev ->
        let args = split data ','
        if not ((List.length args) = 2) then
            (repo, "The arguments must be on the form <datetime>,<username>", 400)
        else
            let datetime = args.[0]
            let user = args.[1]
            let res = add_log (workflow, event) datetime user repo
            match res with
            | Result.Ok(updated_repo) ->
                (updated_repo, "Log added!", 200)
            | error ->
                let msg = sprintf "Got an unsupported error type %A" error
                (repo, msg, 400)
    | "GET", "" ->
        match get_logs workflow repo with
        | Some(logs) ->
            let resp = String.concat "\n" logs
            (repo, resp, 200)
        | None ->
            (repo, "Workflow not found", 404)
    | "GET", ev ->
        (repo, "ERROR: Event given for log get request", 400)
    | _ ->
        (repo, "ERROR: Bad log request method: " + meth, 400)

// The actual resource handling function
let handle_resource (path: string) (meth: string) (message: string) (send_func: SendFunc<Repository>)
        (initial_state: Repository) : ResourceResponse<Repository> =

    let parts = split path '/'
    let response =
        //printfn "Parts: %A" parts
        match parts with
        | [] ->
            initial_state, "Nothing asked, nothing found.",400

        | "user"::user::[] ->
            handle_user user meth initial_state

        | "workflow"::workflow::[] ->
            handle_workflow workflow meth message initial_state

        | "workflow"::workflow::event::[] -> // Create event
            handle_event workflow event "" meth message initial_state send_func

        | "workflow"::workflow::event::attribute::[] ->
            handle_event workflow event attribute meth message initial_state send_func

        | "workflow"::workflow::event::relation::connection::[] ->
            handle_relation workflow event relation connection meth message initial_state send_func

        | "migrate"::[] ->
            handle_full_migration meth message send_func initial_state

        | "migrate"::from_guid::to_guid::[] ->
            handle_partial_migration meth from_guid to_guid send_func initial_state

        | "log"::workflow::event::[] ->
            handle_log workflow event meth message initial_state

        | "log"::workflow::[] ->
            handle_log workflow "" meth message initial_state

        | _ ->
            printfn "Invalid path gotten: %s" path
            initial_state, "Invalid path", 400
    response