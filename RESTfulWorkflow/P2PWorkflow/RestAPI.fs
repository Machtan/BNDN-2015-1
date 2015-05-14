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
    handle_result <| "Workflow created" <| create_workflow workflow state

// Creates a new repository
let create_repository () : Repository =
    { events = Map.empty; users = Map.empty; workflows = Map.empty; }

// Matches the given user and
let handle_user (action: UserAction) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    match action with
    | UserAction.Create(user) ->
        match create_user user state with
        | CreateUserResult.Ok(new_state) ->
            resource_response new_state "User created!" 200
        | CreateUserResult.UserAlreadyExists ->
            resource_response state "User already exists!" 400

    | UserAction.Delete(user) ->
        match delete_user user state with
        | DeleteUserResult.Ok(new_state) ->
            resource_response new_state "User deleted!" 200
        | DeleteUserResult.UserNotFound ->
            resource_response state "User not found!" 404

    | UserAction.GetRoles(user, workflow) ->
        let roles = get_user_roles user workflow state
        let message = String.concat "," roles
        printfn "USER: Request for %s @ %s: %s" user workflow message
        resource_response state message 200

    | UserAction.AddRoles(user, workflow, roles) ->
        match add_user_roles user workflow roles state with
        | AddRolesResult.Ok(new_state) ->
            resource_response new_state "Roles added!" 200
        | AddRolesResult.UserNotFound ->
            resource_response state "User not found" 404

    | UserAction.RemoveRoles(user, workflow, roles) ->
        match remove_user_roles user workflow roles state with
        | RemoveRolesResult.Ok(new_state) ->
            resource_response new_state "Roles removed!" 200
        | RemoveRolesResult.NoRolesForWorkflow ->
            resource_response state "The user has no roles for that workflow" 200
        | RemoveRolesResult.UserNotFound ->
            resource_response state "The user could not be found" 404

// Handles requests for the givien workflow (getting the events in it)
let handle_workflow (workflow: string) (meth: string) (data: string)
        (send_func: SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    match meth with
    | "GET" ->
        match get_workflow_events workflow state with
        | Some(events) ->
            resource_response state (String.concat "," events) 200
        | None ->
            let msg = sprintf "The workflow '%s' was not found!" workflow
            resource_response state msg 404

    | "POST" ->
        createWorkflow workflow state

    | "DELETE" ->
        match delete_workflow workflow send_func state with
        | Some(new_state) ->
            resource_response new_state "Deleted" 200
        | None ->
            let msg = sprintf "The workflow '%s' was not found!" workflow
            resource_response state msg 404

    | "PUT" ->
        match add_event_to_workflow (workflow, data) state with
        | Some(new_state) ->
            resource_response new_state "Event added!" 200
        | None ->
            let msg = sprintf "The workflow '%s' was not found!" workflow
            resource_response state msg 404
    | _ ->
        resource_response state "Unsupported workflow operation" 400

// Sets the attribute of an event
let set_attribute (event: EventName) (attribute: SetAttribute)
        (send_func: SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
        let result =
            match attribute with
            | SetAttribute.Pending ->
                set_pending event true state
            | SetAttribute.Included ->
                set_included event true state
            | SetAttribute.Excluded ->
                set_included event false state
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
let handle_event (event: EventName) (attribute: string)
        (meth: string) (message: string) (send_func : SendFunc<Repository>)
        (state: PastryState<Repository>) : ResourceResponse<Repository> =
    // Find the event in this repository if it exists
    match meth, attribute with
    | "POST", "" ->
        createEvent event message send_func state
    | "DELETE", "" ->
        deleteEvent event send_func state

    | "GET", "getif" ->
        match check_condition event state with
        | ReadResult.Ok(fulfilled) ->
            printfn ">>>>>>>> GETIF %A -> %A" event fulfilled
            let msg = if fulfilled then "true" else "false"
            resource_response state msg 200
        | ReadResult.NotFound(error) ->
            let msg = sprintf "Getif target not found: %A" error
            resource_response state msg 404

    | "GET", "executed" ->
        getExecuted event state
    | "PUT", "executed" ->
        setExecuted event message send_func state

    | "GET", "pending" ->
        getPending event state
    | "PUT", "pending" ->
        match message with
        | "true" ->
            set_attribute event SetAttribute.Pending send_func state
        | _ ->
            resource_response state "Invalid pending state: Must be 'true'" 400

    | "GET", "included" ->
        getIncluded event state

    | "PUT", "included" ->
        match message with
        | "true" ->
            set_attribute event SetAttribute.Included send_func state
        | "false" ->
            set_attribute event SetAttribute.Excluded send_func state
        | _ ->
            let msg = "Invalid include state: Must be 'true' or 'false'"
            resource_response state msg 400

    | "PUT", "lock" ->
        printfn "> REPO Lock Before:"
        print_lock_state state.data
        handle_result <| "Locked" <| lock_event event state

    | "PUT", "unlock" ->
        handle_result <| "Unlocked" <| unlock_event event state

    | "GET", "executable" ->
        getExecutable event message send_func state

    | _ ->
        resource_response state "Unknown event command gotten!" 400

// Handles a request about the relation of an event
let handle_relation (event: EventName) (reltype: string)
        (con_str: string) (meth: string) (message: string)
        (send_func : SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    let args = split message ','
    if not ((List.length args) = 2) then
        let msg = sprintf "Invalid arguments: %A should be on the from <workflow>,<event>" message
        resource_response state msg 400
    else
        let rel =
            match reltype with
            | "exclusion" ->
                Some(Exclusion)
            | "condition" ->
                Some(Condition)
            | "response" ->
                Some(Response)
            | "inclusion" ->
                Some(Inclusion)
            | _  ->
                None

        let data = (args.[0], args.[1])

        let con =
            match con_str with
            | "to" ->
                Some( To(event, data) )
            | "from" ->
                Some( From(data, event) )
            | _ ->
                None

        match rel, con with
        | Some(relation), Some(connection) ->
            match meth with
            | "PUT" ->
                addRelation relation connection send_func state

            | "DELETE" ->
                delete_relation relation connection send_func state

            | _ ->
                resource_response state "Invalid method for adding a relation" 400

        | None, _ ->
            let msg = sprintf "Invalid relation type '%s'" reltype
            resource_response state msg 400

        | _, None ->
            let msg = sprintf "Invalid connection '%s' should be 'to' or 'from'" con_str
            resource_response state msg 400

// Prints this stuff in a very recognizable way
let print_header (msg: string) =
    printfn ""
    printfn "%s" (String.replicate 50 "=")
    printfn "%s" msg
    printfn "%s" (String.replicate 50 "=")
    printfn ""

// Handles a full migration (a node has died, and is being remade)
let handle_full_migration (meth: string) (data: string)
        (send_func: SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    if data = "" then
        resource_response state "Nothing to migrate" 200
    else
        match meth with
        | "PUT" ->
            print_header "Starting migration!"
            let dead_repo = JsonConvert.DeserializeObject<Repository> data
            let cmds = get_all_migration_commands { state with data = dead_repo; }
            let migrate new_state cmd =
                let result = send_func cmd.path cmd.meth cmd.data new_state
                result.state
            let new_state = List.fold migrate state cmds
            print_header "Finished migrating!"
            resource_response new_state "Migrated!" 200
        | _ ->
            resource_response state "Bad method used: migrate requires PUT!" 400

// Handles the migration of resources on this repository that are closer to
// a newly-joined node
let handle_partial_migration (meth: string) (this_nodes_guid: string)
        (new_nodes_guid: string) (send_func: SendFunc<Repository>)
        (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    match meth with
    | "PUT" ->
        print_header "Starting partial migration!"
        let should_migrate path =
            let res = belongs_on_other this_nodes_guid path new_nodes_guid
            printfn "Should migrate %s from %s to %s? -> %A" path this_nodes_guid new_nodes_guid res
            res
        let (new_state, cmds) = get_migratable_commands should_migrate state
        let migrate new_state cmd =
            let result = send_func cmd.path cmd.meth cmd.data new_state
            result.state
        let final_state = List.fold migrate new_state cmds
        print_header "Finished migrating!"
        resource_response final_state "Migrated!" 200

    | _ ->
        resource_response state "Bad method used: migrate requires PUT!" 400

// Handles requests related to logs
let handle_log (workflow: string) (event: string) (meth: string) (data: string)
        (state: PastryState<Repository>) : ResourceResponse<Repository> =
    match (meth, event) with
    | "PUT", "" ->
        resource_response state "No event given for log request!" 400
    | "PUT", ev ->
        let args = split data ','
        if not ((List.length args) = 2) then
            let msg = "The arguments must be on the form <datetime>,<username>"
            resource_response state msg 400
        else
            let datetime = args.[0]
            let user = args.[1]
            let res = add_log (workflow, event) datetime user state
            match res with
            | Result.Ok(new_state) ->
                resource_response new_state "Log added!" 200
            | error ->
                let msg = sprintf "Got an unsupported error type %A" error
                resource_response state msg 400
    | "GET", "" ->
        match get_logs workflow state with
        | Some(logs) ->
            let resp = String.concat "\n" logs
            resource_response state resp 200
        | None ->
            resource_response state "Workflow not found" 404
    | "GET", ev ->
        resource_response state "ERROR: Event given for log get request" 400
    | _ ->
        let msg = sprintf "ERROR: Bad log request method: %s" meth
        resource_response state msg 400

// Debugs the current workflow and sends its state as a message
let debug_state (workflow: string) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    let event_folder workflow event_map lines =
        let inner_folder name (locked, state) lines =
            let i = if state.included then "i" else " "
            let p = if state.pending then "!" else " "
            let e = if state.executed then "x" else " "
            let l = if locked then "{locked} " else ""
            let r = String.concat ", " state.roles
            let line = sprintf "- %s%s%s %s %s(%s)" i p e name l r
            let rel_folder (typ, event) rel_list =
                let wf, ename = event
                let entry = sprintf "%A(%s/%s)" typ wf ename
                entry::rel_list
            let from_rel_list = Set.foldBack rel_folder state.fromRelations []
            let from_rels = String.concat " | " <| "- From: "::from_rel_list
            let to_rel_list = Set.foldBack rel_folder state.toRelations []
            let to_rels = String.concat " | " <| "- To: "::to_rel_list
            line::lines
        let new_lines = Map.foldBack inner_folder event_map lines
        let workflow_line = sprintf "Workflow: '%s'" workflow
        workflow_line::new_lines
    let event_lines = Map.foldBack event_folder state.data.events []

    let workflow_folder (name: string) (workflow: Workflow) lines =
        let name_folder (eventname: string) lines =
            (sprintf "- '%s'" eventname)::lines
        let title_line = sprintf "\nWorkflow events [ %s ]:" name
        let event_list_lines =
            if not (workflow.events = []) then
                title_line::(List.foldBack name_folder workflow.events lines)
            else
                title_line::"<No events in the list>"::lines
        let log_lines =
            if not (workflow.logs = []) then
                "Logs:\n"::workflow.logs
            else
                ["Logs:\n<No logs in the list>"]
        List.append event_list_lines log_lines
    let workflow_lines = Map.foldBack workflow_folder state.data.workflows []
    let all_lines = List.append workflow_lines event_lines

    let result = String.concat "\n" all_lines
    resource_response state result 200

// The actual resource handling function
let handle_resource (path: string) (meth: string) (message: string)
        (send_func: SendFunc<Repository>) (state: PastryState<Repository>)
        : ResourceResponse<Repository> =
    match split path '/' with
    | [] ->
        resource_response state "Nothing asked, nothing found." 400

    | "user"::args ->
        match meth, args with
        | "POST", user::[] ->
            handle_user <| UserAction.Create(user) <| state

        | "DELETE", user::[] ->
            handle_user <| UserAction.Delete(user) <| state

        | "PUT", user::"roles"::workflow::[] ->
            let roles = Set.ofList (split message ',')
            handle_user <| UserAction.AddRoles(user, workflow, roles) <| state

        | "GET", user::"roles"::workflow::[] ->
            handle_user <| UserAction.GetRoles(user, workflow) <| state

        | "DELETE", user::"roles"::workflow::[] ->
            let roles = Set.ofList (split message ',')
            handle_user <| UserAction.RemoveRoles(user, workflow, roles) <| state

        | _ ->
            resource_response state "Unsupported user operation" 400

    | "workflow"::workflow::[] ->
        handle_workflow workflow meth message send_func state

    | "workflow"::workflow::event::[] -> // Create event
        handle_event (workflow, event) "" meth message send_func state

    | "workflow"::workflow::event::attribute::[] ->
        handle_event (workflow, event) attribute meth message send_func state

    | "workflow"::workflow::event::relation::connection::[] ->
        handle_relation (workflow, event) relation connection meth message send_func state

    | "migrate"::[] ->
        handle_full_migration meth message send_func state

    | "migrate"::this_nodes_guid::new_nodes_guid::[] ->
        handle_partial_migration meth this_nodes_guid new_nodes_guid send_func state

    | "log"::workflow::event::[] ->
        printfn "REST: Logging event..."
        handle_log workflow event meth message state

    | "log"::workflow::[] ->
        printfn "REST: Getting logs..."
        handle_log workflow "" meth message state

    | "debug"::workflow::[] ->
        debug_state workflow state

    | _ ->
        printfn "Invalid path gotten: %s" path
        resource_response state "Invalid path" 400
