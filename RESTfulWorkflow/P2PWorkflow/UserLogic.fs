module UserLogic

open Repository_types

//Create

/// Creates and returns a new user from a name
let create_user (user : UserName) : ResultUser =
    failwith "Not implemented yed."

//Read

/// Metodes used when finding all executabel event for a user 
//let find_executable_events (user : User) : Executable =
//    failwith "Not implemented yed."
/// Return name and state of given event
let get_user_roles (user : UserName)  (workflow : WorkflowName) (repository : Repository) : Roles list =
    failwith "Not implemented yed."

//Update

/// Adds given workflow roles to a given user and returns the result
let add_user_roles (user : User) (workflow : WorkflowName) (roles : Roles) : ResultUser =
    failwith "Not implemented yed."

//Delete

/// Deletes given user and returns it if its susesful
let delete_user (user : User) : ResultUser =
    failwith "Not implemented yed."

/// Removes given workflow roles form given user and returns the result
let remove_user_roles (user : User) (workflow : Workflow) (roles : Roles) : ResultUser =
    failwith "Not implemented yed."
