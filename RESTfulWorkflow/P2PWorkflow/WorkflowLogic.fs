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
    let workflows = repos.workflows.Add(wfName, [])
    Result.Ok(repos)

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
let add_event (wfName : WorkflowName) (event : EventName) (repos : Repository) : ResultWorkflow =
    let events = 
        match repos.workflows.TryFind(wfName) with
        | Some(v) -> v
        | None -> failwith "Noooooo" 
    let (makeEventnames : EventName list) =
        events |> List.map (fun (event) -> (wfName, event))
    ResultWorkflow.Ok(wfName, event::makeEventnames)
    //does this meathod rly add a event to the repos?

//Delete

/// Removes given event form given workflow and returns the result
let remove_event (workflow : WorkflowName) (event : EventName) (magic : SendFunc<'a>) (repos : Repository) : ResultWorkflow =
    let wfName, eventNameList = workflow
    let rec remove_first eventNameList event =
        match eventNameList with
        |h::eventNameList when h = event -> eventNameList
        |h::eventNameList -> h::(remove_first eventNameList event)
        | _ -> []
    ResultWorkflow.Ok(wfName, (remove_first eventNameList event))

/// Deletes given workflow and returns it if its susesful
let delete_workflow (workflow : Workflow) (magic : SendFunc<'a>) (repos : Repository) : ResultWorkflow =
    let wfName, eventNameList = workflow
    let rec deleteEvents eventNameList=
        match eventNameList with
        | h::eventNameList ->
            match remove_event workflow h magic repos with  
            |ResultWorkflow.Ok(x) -> 
                let wfN, eventNList = x
                deleteEvents eventNList
            | _ -> []
        | _ -> []
    //call method to remove workflow fom repository?
    ResultWorkflow.Ok("", [])




