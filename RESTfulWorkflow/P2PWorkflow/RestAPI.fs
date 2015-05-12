module Rest

open Repository_types
open UserLogic
open EventLogic
open WorkflowLogic
open Pastry
open PastryTypes
open Migrate
open Newtonsoft.Json

// Relation deserialization type
type Connection =
| From of EventName * EventName // From A to B
| To   of EventName * EventName // From A to B

// Attribute deserialization type
type Attribute =
| Included
| Executed
| Pending

// An attribute on an event that can be set
type SetAttribute =
| Included
| Excluded
| Pending

// User serialization type
type UserAction =
| Create of UserName
| Delete of UserName
| GetRoles of UserName * WorkflowName
| AddRoles of UserName * WorkflowName * Roles
| RemoveRoles of UserName * WorkflowName * Roles

// Splits a string into a list by a char
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Attempts to get an event from the workflow
let getEvent (event: EventName) (state: PastryState<Repository>)
        : (Event option * string * int) =
    match get_event event state with
    | ReadResult.Ok((event_result, _)) ->
        Some(event_result),"Ok", 200
    | NotFound(NotFoundError.Event) ->
        None, "Event not found", 404
    | NotFound(NotFoundError.Workflow) ->
        None, "Workflow not found", 404

// Gets the state of an attribute of an event
let get_attribute (event: EventName) (attribute: Attribute)
        (state: PastryState<Repository>) : ResourceResponse<Repository> =
    let (event_result, message, status) = getEvent event state
    match event_result with
    | Some(event) ->
        let bool_value =
            match attribute with
            | Attribute.Included  -> event.included
            | Attribute.Executed  -> event.executed
            | Attribute.Pending   -> event.pending
        let resp = if bool_value then "true" else "false"
        resource_response state resp 200
    | None ->
        resource_response state message status

// Gets the 'included' state of an event
let getIncluded (event: EventName) (state: PastryState<Repository>)
        : ResourceResponse<Repository>  =
    get_attribute event Attribute.Included state

// Gets the 'pending' state of an event
let getPending (event: EventName) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    get_attribute event Attribute.Pending state

// Gets the 'executed' state of an event
let getExecuted (event: EventName) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    get_attribute event Attribute.Executed state

// Gets the 'executable' state of an event
let getExecutable (event: EventName) (user: string)
        (send_func: SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    let result = check_if_executable event user send_func state
    match result.result with
    | ReadResult.Ok(executable_state) ->
        match executable_state with
        | ExecutableState.Executable ->
            resource_response result.state "true" 200
        | ExecutableState.Unauthorized ->
            let message = sprintf "'%s' is not authorized to execute this" user
            resource_response result.state message 402
        | ExecutableState.NotExecutable ->
            resource_response result.state "false" 200
        | ExecutableState.Locked ->
            resource_response result.state "The event is locked!" 400

    | NotFound(NotFoundError.Event) ->
        resource_response result.state "Event not found" 404

    | NotFound(NotFoundError.Workflow) ->
        resource_response result.state "Workflow not found" 404

// Handles an event result in a default way
let handle_result (ok_message: string) (result: Result)
        : ResourceResponse<Repository> =
    match result with
    | Result.Ok(new_state) ->
        resource_response new_state ok_message 200
    | Result.Unauthorized(new_state) ->
        resource_response new_state "Unauthorized" 401
    | Result.NotExecutable(new_state) ->
        resource_response new_state "The event is not executable." 400
    | Result.MissingRelation(new_state) ->
        resource_response new_state "The relation is missing." 400
    | Result.LockConflict(new_state) ->
        resource_response new_state "Encountered LockConflict." 400
    | Result.MissingEvent(new_state) ->
        resource_response new_state "Event is missing" 400
    | Result.MissingWorkflow(new_state) ->
        resource_response new_state "Workflow is missing" 400
    | Result.Error(msg, new_state) ->
        resource_response new_state msg 400

// Attempts to execute the given event
let setExecuted (event: EventName) (user: UserName)
        (send_func: SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    handle_result <| "Executed" <| execute event user send_func state

// Attempts to create a new event
let deleteEvent (event: EventName) (send_func: SendFunc<Repository>)
    (state: PastryState<Repository>) : ResourceResponse<Repository> =
    handle_result <| "Deleted" <| delete_event event send_func state

// Creates a new event
let createEvent (event: EventName) (message: string)
        (send_func: SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    let args = split message ','
    let state_str = List.head args
    let roles = List.tail args
    if not (state_str.Length = 4) then
        let msg = sprintf "Received invalid initial event state: %s" state_str
        printfn "%s" msg
        resource_response state msg 400
    else
        let des index = // deserialize bool
            state_str.[index] = '1'

        let event_state: EventState = {
            included = des 0;
            pending = des 1;
            executed = des 2
        }
        //printfn "> Creating event '%s' with state %A..." eventName initialState
        let locked = des 3

        handle_result <| "Created" <| create_event event event_state roles locked state

// Adds a new relation
let addRelation (relation: RelationType) (connection: Connection)
        (send_func: SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository>=
    let con_desc =
        match connection with
        | To(a, b) ->
            sprintf "to %A from %A" b a
        | From(a, b) ->
            sprintf "from %A to %A" a b
    //printfn "RELATION: Adding %A %s" relation con_desc
    let response =
        match connection with
        | To(a, b) ->
            add_relation_to a relation b send_func state
        | From(a, b) ->
            add_relation_from a relation b state
    handle_result <| "Relation added" <| response


// Deletes a relation
let delete_relation (relation: RelationType) (connection: Connection)
        (send_func: SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository>=
    let response =
        match connection with
        | From(a, b) ->
            remove_relation_to a relation b send_func state
        | To  (a, b) ->
            remove_relation_from a relation b send_func state
    handle_result "Relation deleted" response

// Creates a new workflow
let createWorkflow (workflow: string) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    handle_result |> "Workflow created"|> create_workflow workflow state

// Creates a new repository
let create_repository () : Repository =
    { events = Map.empty; users = Map.empty; workflows = Map.empty; }

// Matches the given user and
let handle_user (action: UserAction) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    match action with
    | UserAction.Create(user) ->
        match create_user user repo with
        | CreateUserResult.Ok(updated_repo) ->
            updated_repo, "User created!", 200
        | CreateUserResult.UserAlreadyExists ->
            repo, "User already exists!", 400

    | UserAction.Delete(user) ->
        match delete_user user repo with
        | DeleteUserResult.Ok(updated_repo) ->
            updated_repo, "User deleted!", 200
        | DeleteUserResult.UserNotFound ->
            repo, "User not found!", 404

    | UserAction.GetRoles(user, workflow) ->
        let roles = get_user_roles user workflow repo
        let message = String.concat "," roles
        repo, message, 200

    | UserAction.AddRoles(user, workflow, roles) ->
        match add_user_roles user workflow roles repo with
        | AddRolesResult.Ok(updated_repo) ->
            updated_repo, "Roles added!", 200
        | AddRolesResult.UserNotFound ->
            repo, "User not found", 404

    | UserAction.RemoveRoles(user, workflow, roles) ->
        match remove_user_roles user workflow roles repo with
        | RemoveRolesResult.Ok(updated_repo) ->
            updated_repo, "Roles removed!", 200
        | RemoveRolesResult.NoRolesForWorkflow ->
            repo, "The user has no roles for that workflow", 200
        | RemoveRolesResult.UserNotFound ->
            repo, "The user could not be found", 404

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

// Sets the attribute of an event
let set_attribute (event: EventName) (attribute: SetAttribute)
        (send_func: SendFunc<Repository>) (repo: Repository)
        : ResourceResponse<Repository> =
        let result =
            match attribute with
            | SetAttribute.Pending ->
                set_pending event true repo
            | SetAttribute.Included ->
                set_included event true repo
            | SetAttribute.Excluded ->
                set_included event false repo
        handle_result <| "Created" <| result


// Prints the lock state of the given repository
let print_lock_state (repo: Repository) =
    let check_folder workflow map statelist =
        let inner_folder event (locked, _) statelist =
            if locked then (workflow, event)::statelist
            else statelist
        Map.foldBack inner_folder map statelist
    let event_state = Map.foldBack check_folder repo.events []
    printfn "LOCK STATE: %A" event_state

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
        setExecuted (workflow_name, event_name) message repo sendFunc
    | "GET", "pending" ->
        getPending event_name workflow_name repo
    | "PUT", "pending" ->
        match message with
        | "true" ->
            set_attribute (workflow_name, event_name) SetAttribute.Pending sendFunc repo
        | _ ->
            (repo, "Invalid pending state: Must be 'true'", 400)
    | "GET", "included" ->
        getIncluded event_name workflow_name repo
    | "PUT", "included" ->
        match message with
        | "true" ->
            set_attribute (workflow_name, event_name) SetAttribute.Included sendFunc repo
        | "false" ->
            set_attribute (workflow_name, event_name) SetAttribute.Excluded sendFunc repo
        | _ ->
            (repo, "Invalid include state: Must be 'true' or 'false'", 400)
    | "PUT", "lock" ->
        printfn "> REPO Lock Before:"
        print_lock_state repo
        match lock_event (workflow_name, event_name) repo with
        | Result.Ok(updated_repo) ->
            printfn "> REPO Lock After:"
            print_lock_state updated_repo
            updated_repo, "Locked!", 200
        | Result.LockConflict ->
            repo, "Event already locked!", 400
        | Result.MissingEvent ->
            repo, "Event not found", 404
        | err ->
            repo, (sprintf "Got error %A" err), 400
    | "PUT", "unlock" ->
        match unlock_event (workflow_name, event_name) repo with
        | Result.Ok(updated_repo) ->
            printfn "REPO Unlock:"
            print_lock_state updated_repo
            updated_repo, "Unlocked!", 200
        | Result.LockConflict ->
            repo, "The event was not locked!", 400
        | err ->
            repo, (sprintf "Got error %A" err), 400
    | "GET", "executable" ->
        getExecutable (workflow_name, event_name) message sendFunc repo
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
let handle_resource (path: string) (meth: string) (message: string)
        (send_func: SendFunc<Repository>) (repo: Repository)
        : ResourceResponse<Repository> =
    match split path '/' with
    | [] ->
        repo, "Nothing asked, nothing found.",400

    | "user"::args ->
        match meth, args with
        | "POST", user::[] ->
            handle_user <| UserAction.Create(user) <| repo

        | "DELETE", user::[] ->
            handle_user <| UserAction.Delete(user) <| repo

        | "PUT", user::"roles"::workflow::[] ->
            let roles = Set.ofList (split message ',')
            handle_user <| UserAction.AddRoles(user, workflow, roles) <| repo

        | "GET", user::"roles"::workflow::[] ->
            handle_user <| UserAction.GetRoles(user, workflow) <| repo

        | "DELETE", user::"roles"::workflow::[] ->
            let roles = Set.ofList (split message ',')
            handle_user <| UserAction.RemoveRoles(user, workflow, roles) <| repo

        | _ ->
            (repo, "Unsupported user operation", 400)

    | "workflow"::workflow::[] ->
        handle_workflow workflow meth message repo

    | "workflow"::workflow::event::[] -> // Create event
        handle_event workflow event "" meth message repo send_func

    | "workflow"::workflow::event::attribute::[] ->
        handle_event workflow event attribute meth message repo send_func

    | "workflow"::workflow::event::relation::connection::[] ->
        handle_relation workflow event relation connection meth message repo send_func

    | "migrate"::[] ->
        handle_full_migration meth message send_func repo

    | "migrate"::from_guid::to_guid::[] ->
        handle_partial_migration meth from_guid to_guid send_func repo

    | "log"::workflow::event::[] ->
        handle_log workflow event meth message repo

    | "log"::workflow::[] ->
        handle_log workflow "" meth message repo

    | _ ->
        printfn "Invalid path gotten: %s" path
        repo, "Invalid path", 400
