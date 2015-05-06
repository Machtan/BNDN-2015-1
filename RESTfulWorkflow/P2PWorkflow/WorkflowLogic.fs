module WorkflowLogic

open Repository_types

//Create

/// Creates and returns a new workflow from a name
let create_workflow (wfName : WorkflowName) (repos : Repository) : Result =
    Result.Ok({repos with workflows = Map.add wfName [] repos.workflows})

//Read
let check_workflow (wfName : WorkflowName) (repos : Repository) : bool =
        match repos.workflows.TryFind(wfName) with
        | Some(v) -> true
        | None -> false

/// Metodes used when finding all executabel event for a user   SHOULD NOT BE IMPLEMENTED
//let find_executable_with_roles (workflow : Workflow) (roles : Roles) (magic : SendFunc<'a>) (repos : Repository) : ExecutableInWorkflow =
//    failwith "Not implemented yed."

//Update

/// Adds a given event to a given Workflow and returns the result
let add_event (wfName : WorkflowName) (event : EventName) (repos : Repository) : Result =
    let workflowName, event = event
    let events = 
        match repos.workflows.TryFind(wfName) with
        | Some(v) -> v
        | None -> failwith "Workflow Does not exist" 
    Result.Ok({repos with workflows = Map.add wfName (event::events) repos.workflows})


//Delete

/// Removes given event form given workflow and returns the result
let remove_event (wfName : WorkflowName) (event : EventName) (magic : SendFunc<Repository>) (repos : Repository) : Result =
    let workflowName, event = event
    let events = 
        match repos.workflows.TryFind(wfName) with
        | Some(v) -> v
        | None -> failwith "Workflow Does not exist" 
    let rec remove_first events event =
        match events with
        |h::events when h = event -> events
        |h::events -> h::(remove_first events event)
        | _ -> []
    Result.Ok({repos with workflows = Map.add wfName (remove_first events event) repos.workflows})

/// Deletes given workflow and returns it if its susesful
let delete_workflow (wfName : WorkflowName) (magic : SendFunc<Repository>) (repos : Repository) : Result =
    let events = 
        match repos.workflows.TryFind(wfName) with
        | Some(v) -> v
        | None -> failwith "Workflow Does not exist" 
    let rec deleteEvents events=
        match events with
        | h::events ->
            match remove_event wfName h magic repos with  
            |Result.Ok(x) -> 
                deleteEvents events
            | _ -> []
        | _ -> []
    Result.Ok({repos with workflows = Map.remove wfName repos.workflows})