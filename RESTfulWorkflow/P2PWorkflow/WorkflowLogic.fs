module WorkflowLogic

open Repository_types

//Get
// let get_events (wfName : WorkflowName)(repos : Repository) : EventName list =
//    let events = 
//        match repos.workflows.TryFind(wfName) with
//        | Some(v) -> v
//        | None -> failwith "Noooooo" 
//    let (makeEventnames : EventName list) =
//        events |> List.map (fun (event) -> (wfName, event))
//    makeEventnames(events)

//Create

/// Creates and returns a new workflow from a name
let create_workflow (wfName : WorkflowName) (repos : Repository) : Result =
    Result.Ok({repos with workflows = Map.add wfName [] repos.workflows})

//Read
let check_workflow (wfName : WorkflowName) (repos : Repository) : bool =
    failwith "Not implemented yed."

/// Metodes used when finding all executabel event for a user
let find_executable_with_roles (workflow : Workflow) (roles : Roles) (magic : SendFunc<'a>) (repos : Repository) : ExecutableInWorkflow =
//    let name, eventList = workflow
//    let rec findEvents
//        match eventList with
//        | [] -> []

//recursive find the events and :: them together and return them?
    failwith "Not implemented yed."

//Update

/// Adds a given event to a given Workflow and returns the result
let add_event (wfName : WorkflowName) (event : EventName) (repos : Repository) : Result =
    let workflowName, event = event
    let events = 
        match repos.workflows.TryFind(wfName) with
        | Some(v) -> v
        | None -> failwith "Workflow Does not exist" 
    Result.Ok({repos with workflows = Map.add wfName (event::events) repos.workflows})
    //add to event. Event is not a list?

//Delete

/// Removes given event form given workflow and returns the result
let remove_event (wfName : WorkflowName) (event : EventName) (magic : SendFunc<'a>) (repos : Repository) : Result =
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
let delete_workflow (wfName : WorkflowName) (magic : SendFunc<'a>) (repos : Repository) : Result =
    let events = 
        match repos.workflows.TryFind(wfName) with
        | Some(v) -> v
        | None -> failwith "Workflow Does not exist" 
    let (makeEventnames : EventName list) =
        events |> List.map (fun (event) -> (wfName, event))
    let rec deleteEvents eventNameList=
        match eventNameList with
        | h::eventNameList ->
            match remove_event wfName h magic repos with  
            |Result.Ok(x) -> 
                let wfN, eventNList = x
                deleteEvents eventNList
            | _ -> []
        | _ -> []
    //call method to remove workflow fom repository?
    Result.Ok({repos with workflows = Map.remove wfName repos.workflows})
