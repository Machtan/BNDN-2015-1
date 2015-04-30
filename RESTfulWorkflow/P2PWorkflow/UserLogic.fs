module UserLogic


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
let create_user (user : UserName) : ResultUser =
    failwith "Not implemented yed."

//Read

/// Metodes used when finding all executabel event for a user 
let find_executable_events (user : User) : Executable =
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
