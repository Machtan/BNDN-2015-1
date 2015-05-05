module Repository

open Repository_types
open UserLogic
open EventLogic
open WorkflowLogic
open Pastry

// USER LOGIC
//val create_user:                UserName -> ResultUser
//Read

/// Metodes used when finding all executabel event for a user
//val find_executable_events:     User -> Executable

//Update

/// Adds given workflow roles to a given user and returns the result
//val add_user_roles:             User -> WorkflowName -> Roles -> ResultUser

//Delete

/// Deletes given user and returns it if its susesful
//val delete_user:                User -> ResultUser
/// Removes given workflow roles form given user and returns the result
//val remove_user_roles:          User -> Workflow -> Roles -> ResultUser


// WORKFLOW LOGIC
// Creates and returns a new workflow from a name
//let create_workflow (workflow : WorkflowName) : ResultWorkflow =
//    failwith "Not implemented yed."

//Read

/// Metodes used when finding all executabel event for a user
//let find_executable_with_roles (wrkflow : Workflow) (roles : Roles) : ExecutableInWorkflow =
//    failwith "Not implemented yed."

//Update

/// Adds a given event to a given Workflow and returns the result
//let add_event (workflow : Workflow) (event : EventName) : ResultWorkflow =
//    failwith "Not implemented yed."

//Delete

/// Deletes given workflow and returns it if its susesful
//let delete_workflow (workflow : Workflow) : ResultWorkflow =
//    failwith "Not implemented yed."

/// Removes given event form given workflow and returns the result
//let remove_event (workflow : Workflow) (event : EventName) : ResultWorkflow =
//    failwith "Not implemented yed."

// From pastry, basically
type SendFunc<'a> = string -> string -> string -> 'a -> 'a * string

// Updated state, response, status code
type ResourceResponse<'a> = 'a * string * int

// Creates a new repository
let create_repository () : Repository =
    { events = Map.empty; users = Map.empty; workflows = Map.empty; logs = []; }

// Splits a string at a char 'yay'
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

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
        match Map.tryFind wf repo.workflows with
        | None ->
            repo, "Could not find workflow", 404
        | Some(event_name_list) ->
            String.concat SEPARATOR event_name_list
    | "POST" ->
        match Map.tryFind wf repo.workflows with
        | None ->
            ignore <| create_workflow wf
            failwith "Not Implemented"
        | Some(_) ->
            repo, "Workflow already exists!", 400
    | "DELETE" ->
        
    | _ ->
        repo, "Unsupported workflow operation", 400,

    failwith "Not Implemented"

// Attempts to find an event in the given repository
let find_event (workflow: string) (event: string) (repo: Repository): (bool * Event) option =
    match Map.tryFind workflow repo.events with
    | None -> None
    | Some(map) -> Map.tryFind event map

// Matches the given event and tries to handle the request
let handle_event (workflow_name: string) (event_name: string) (attribute: string)
        (meth: string) (repo: Repository) : ResourceResponse<Repository> =
    // Find the event in this repository if it exists
    match find_event workflow_name event_name repo with
    | None ->
        repo, "Event not found!", 404
    | Some(locked, event) ->
        // Find out what needs to be done
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
                match relation with
                | "exclusion"   -> ()//Some (Exclusion dest)
                | "condition"   -> ()//Some (Dependent dest)
                | "response"    -> ()//Some (Response dest)
                | "inclusion"   -> ()//Some (Inclusion dest)
                | _             -> ()//None
                failwith "Not Implemented"
            | _ ->
                failwith "Not Implemented"//"Unsupported operation", 404, "Not found", initialState
        response

// The actual resource handling function
let resource_handler (path: string) (meth: string) (send_func: SendFunc<Repository>)
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