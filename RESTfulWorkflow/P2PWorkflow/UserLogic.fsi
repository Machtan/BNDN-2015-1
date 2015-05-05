module UserLogic

open Repository_types

//Create

/// Creates and returns a new user from a name
val create_user:                UserName -> Repository -> ResultUser

//Read

/// Metodes used when finding all executabel event for a user 
val find_executable_events:     UserName -> Repository -> Executable

//Update

/// Adds given workflow roles to a given user and returns the result
val add_user_roles:             UserName -> WorkflowName -> Roles -> Repository -> ResultUser

//Delete

/// Deletes given user and returns it if its susesful
val delete_user:                UserName -> Repository -> ResultUser
/// Removes given workflow roles form given user and returns the result
val remove_user_roles:          UserName -> Workflow -> Roles -> Repository -> ResultUser