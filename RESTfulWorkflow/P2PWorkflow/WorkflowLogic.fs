module WorkflowLogic

open Pastry
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

/// Gets all events in given workflow
let get_workflow_events (wfName : WorkflowName) (repos : Repository) : (string list) option =
    Map.tryFind wfName repos.workflows

let get_log (workflow : WorkflowName) (repository : Repository) : (string list) option  =
    match Map.tryFind workflow repository.logs with
    | Some(logs)    -> Some(logs)
    | None          -> None
    
//Update

/// Adds a given event to a given Workflow and returns the result
let add_event_to_workflow (event : EventName) (repo : Repository) : Repository option =
    let workflow, event_name = event
    match repo.workflows.TryFind(workflow) with
    | Some(events) ->
        let updated_workflows = Map.add workflow (event_name::events) repo.workflows
        Some({repo with workflows = updated_workflows; })
    | None ->
        None

/// Adds a log
let add_log (event : EventName) (date : string) (userName : UserName) (repository : Repository) : Result =
    let workflow, event = event
    let log = sprintf "%s, %s, %s" event date userName
    let logList = 
        match Map.tryFind workflow repository.logs with
        | Some(oldLogs) -> log::oldLogs
        | None          -> log::[]
    Ok({repository with logs = Map.add workflow logList repository.logs})


//Delete

/// Removes given event form given workflow and returns the result
let remove_event_from_workflow (event_name : EventName) (repo : Repository) : Repository option =
    let workflow_name, event = event_name
    match Map.tryFind workflow_name repo.workflows with
    | Some(events) ->
        let updated_events = List.filter (fun e -> not (e = event)) events
        let updated_workflows = Map.add workflow_name updated_events repo.workflows
        Some({ repo with workflows = updated_workflows; })
    | None ->
        None

/// Deletes given workflow and returns it if its susesful
let delete_workflow (wfName : WorkflowName) (repos : Repository) : Repository option =
    match repos.workflows.TryFind(wfName) with
    | Some(_) ->
        Some({repos with workflows = Map.remove wfName repos.workflows})
    | None ->
        None
    //let rec deleteEvents events=
    //    match events with
    //    | event::events ->
    //        match remove_event (wfName, event) repos with
    //        |Result.Ok(x) ->
    //            deleteEvents events
    //        | _ -> []
    //    | _ -> []
