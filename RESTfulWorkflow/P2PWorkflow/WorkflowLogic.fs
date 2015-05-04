module WorkflowLogic

open Repository_types

//Create

/// Creates and returns a new workflow from a name
let create_workflow (workflow : WorkflowName) : ResultWorkflow =
    failwith "Not implemented yed."

//Read

/// Metodes used when finding all executabel event for a user
let find_executable_with_roles (wrkflow : Workflow) (roles : Roles) : ExecutableInWorkflow =
    failwith "Not implemented yed."

//Update

/// Adds a given event to a given Workflow and returns the result
let add_event (workflow : Workflow) (event : EventName) : ResultWorkflow =
    failwith "Not implemented yed."

//Delete

/// Deletes given workflow and returns it if its susesful
let delete_workflow (workflow : Workflow) : ResultWorkflow =
    failwith "Not implemented yed."

/// Removes given event form given workflow and returns the result
let remove_event (workflow : Workflow) (event : EventName) : ResultWorkflow =
    failwith "Not implemented yed."



