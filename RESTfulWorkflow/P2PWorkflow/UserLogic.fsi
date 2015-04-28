module User


type UserName = string              // The name of a user
type WorkflowName = string          // The name of a workflow
type Roles = string list            //A list of roles
type User = UserName*(WorkflowName*Roles list)  // The name of a user + witch roles he have it witch workflow

type EventName = WorkflowName*string      // WorkflowName*EventName
type Workflow = WorkflowName*(EventName list)  // The name of a workflow and the events it contain

type EventState = bool*bool*bool    // Executed*Pending*Includet
type Executable = Map<WorkflowName, string*EventState list>

// posible results when working with users
type ResultUser =
    | Ok of User
    | Unauthorized
    | NotExecutable
    | MissingEvent of string


//Create

/// Creates and returns a new user from a name
val create_user:                UserName -> ResultUser

//Read

/// Metodes used when finding all executabel event for a user 
val find_executable_events:     User -> Executable

//Update

/// Adds given workflow roles to a given user and returns the result
val add_user_roles:             User -> WorkflowName -> Roles -> ResultUser

//Delete

/// Deletes given user and returns it if its susesful
val delete_user:                User -> ResultUser
/// Removes given workflow roles form given user and returns the result
val remove_user_roles:          User -> Workflow -> Roles -> ResultUser