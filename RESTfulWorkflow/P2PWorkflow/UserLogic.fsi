module UserLogic

open Pastry
open Repository_types

//Create

/// Creates and returns a new user from a name
val create_user: UserName -> Repository -> CreateUserResult

//Read

/// Return name and state of given event
val get_user_roles: UserName -> WorkflowName -> Repository -> Roles

//Update

/// Adds given workflow roles to a given user and returns the result
val add_user_roles: UserName -> WorkflowName -> Roles -> Repository -> AddRolesResult

//Delete

/// Deletes given user and returns it if its susesful
val delete_user: UserName -> Repository -> DeleteUserResult

/// Removes given workflow roles form given user and returns the result
val remove_user_roles: UserName -> WorkflowName -> Roles -> Repository -> RemoveRolesResult
