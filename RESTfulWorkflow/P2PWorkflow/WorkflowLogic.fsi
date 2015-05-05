module WorkflowLogic

open Repository_types

//Create

/// Creates and returns a new workflow from a name
val create_workflow:            WorkflowName -> Repository -> Result

//Read

/// Metodes used when finding all executabel event for a user
val find_executable_with_roles: WorkflowName -> Roles -> SendFunc<'a> -> Repository -> ExecutableInWorkflow

//Update

/// Adds a given event to a given Workflow and returns the result
val add_event:                  WorkflowName -> EventName -> Repository -> Result

//Delete

/// Deletes given workflow and returns it if its susesful
val delete_workflow:            WorkflowName -> SendFunc<'a> -> Repository -> Result
/// Removes given event form given workflow and returns the result
val remove_event:               WorkflowName -> EventName -> SendFunc<'a> -> Repository -> Result

