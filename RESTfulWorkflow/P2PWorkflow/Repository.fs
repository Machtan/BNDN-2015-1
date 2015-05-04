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

type Repository = {
    logs: string list;
}

// The actual resource handling function
let resource_handler (path: string) (meth: string) (send_func: SendFunc<Repository>)
        (state: Repository) : Repository * string =
    failwith "Not Implemented"






[<EntryPoint>]
let main args =
    match args with
        | [|addr; port; peer|] ->
            let address = sprintf "%s:%s" (if addr = "" then "localhost" else addr) port
            printfn "? Joining pastry network at '%s'..." address
            let known_peer = if peer = "" then None else Some(peer)

            // url, method, send_func, state -> state, response
            // Yay!
            let dummy_handler path meth send_func (state: 'a) : 'a * string =
                printfn "REPOSITORY: Dummy handler is handling '%s' '%s'" meth path
                state, "Hello World"

            let node = start_server address known_peer dummy_handler 0
            0
            // Start listening...
        | _ ->
            printfn "Usage: Pastry.exe <address> <port> <peer_address_with_port>"
            1