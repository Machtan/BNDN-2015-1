module WorkflowLogic

open Repository_types

//Create

/// Creates and returns a new workflow from a name
let create_workflow (workflow : WorkflowName) : ResultWorkflow =
    failwith "Not implemented yed."
    ResultWorkflow.Ok(workflow,[])

//Read

/// Metodes used when finding all executabel event for a user
let find_executable_with_roles (workflow : Workflow) (roles : Roles) : ExecutableInWorkflow =
//    let name, eventList = workflow
//    let rec findEvents
//        match eventList with
//        | [] -> []

//recursive find the events and :: them together and return them?
    failwith "Not implemented yed."

//Update

/// Adds a given event to a given Workflow and returns the result
let add_event (workflow : Workflow) (event : EventName) : ResultWorkflow =
    failwith "Not implemented yed."
    let wfName, eventNameList = workflow
    ResultWorkflow.Ok(wfName, event::eventNameList)

//Delete

/// Removes given event form given workflow and returns the result
let remove_event (workflow : Workflow) (event : EventName) : ResultWorkflow =
    let wfName, eventNameList = workflow
    let rec remove_first eventNameList event =
        match eventNameList with
        |h::eventNameList when h = event -> eventNameList
        |h::eventNameList -> h::(remove_first eventNameList event)
        | _ -> []
    ResultWorkflow.Ok(wfName, (remove_first eventNameList event))

/// Deletes given workflow and returns it if its susesful
let delete_workflow (workflow : Workflow) : ResultWorkflow =
    let wfName, eventNameList = workflow
    let rec deleteEvents eventNameList=
        match eventNameList with
        | h::eventNameList ->
            match remove_event workflow h with 
            |ResultWorkflow.Ok(x) -> 
                let wfN, eventNList = x
                deleteEvents eventNList
            | _ -> []
        | _ -> []
    //call method to remove workflow fom repository?
    ResultWorkflow.Ok("", [])




