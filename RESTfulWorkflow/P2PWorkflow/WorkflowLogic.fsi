module Workflow

type Roles = string list            //A list of roles
type WorkflowName = string          // The name of a workflow
type EventName = WorkflowName*string      // WorkflowName*EventName
type EventState = bool*bool*bool    // Executed*Pending*Includet

type Workflow = WorkflowName*(EventName list)  // The name of a workflow and the events it contain

// Shemas used when finding all executabel event for a user
type ExecutableInWorkflow = WorkflowName*(string*EventState list)

// posible results when working with workflows
type ResultWorkflow =
    | Ok of Workflow
    | Unauthorized
    | NotExecutable
    | MissingEvent of string


//Create

/// Creates and returns a new workflow from a name
val create_workflow:            WorkflowName -> ResultWorkflow

//Read

/// Metodes used when finding all executabel event for a user
val find_executable_with_roles: Workflow -> Roles -> ExecutableInWorkflow

//Update

/// Adds a given event to a given Workflow and returns the result
val add_event:                  Workflow -> EventName -> ResultWorkflow

//Delete

/// Deletes given workflow and returns it if its susesful
val delete_workflow:            Workflow -> ResultWorkflow
/// Removes given event form given workflow and returns the result
val remove_event:               Workflow -> EventName -> ResultWorkflow

