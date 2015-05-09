module WorkflowLogic

open Pastry
open Repository_types

// Create

/// Creates and returns a new workflow from a name
val create_workflow: WorkflowName -> Repository -> Result

// Read

/// Metodes used when finding all executabel event for a user
//val find_executable_with_roles: WorkflowName -> Roles -> SendFunc<Repository> -> Repository -> ExecutableInWorkflow

/// Check if given workflow exsist
val check_workflow: WorkflowName -> Repository -> bool

/// Gets all events in given workflow
val get_workflow_events: WorkflowName -> Repository -> (string list) option

// Returns the logs of the given workflow if it is found
val get_logs: WorkflowName -> Repository -> (string list) option

// Update

/// Adds a given event to a given Workflow and returns the result
val add_event_to_workflow: EventName -> Repository -> Repository option

/// Adds a log to a workflow if it is found
val add_log: EventName -> string -> UserName -> Repository -> Result

/// Delete

/// Deletes given workflow and returns it if its susesful
val delete_workflow: WorkflowName -> Repository -> Repository option

/// Removes given event form given workflow and returns the result
val remove_event_from_workflow: EventName -> Repository -> Repository option

