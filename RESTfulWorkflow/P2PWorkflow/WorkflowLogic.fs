module WorkflowLogic

open Pastry
open Repository_types

//Create

/// Creates and returns a new workflow from a name
let create_workflow (wfName : WorkflowName) (repos : Repository) : Result =
    let workflow = {
        name = wfName;
        events = [];
        logs = [];
    }
    Result.Ok({repos with workflows = Map.add wfName workflow repos.workflows})

//Read
let check_workflow (wfName : WorkflowName) (repos : Repository) : bool =
    match repos.workflows.TryFind(wfName) with
    | Some(_) -> true
    | None -> false

/// Metodes used when finding all executabel event for a user   SHOULD NOT BE IMPLEMENTED
//let find_executable_with_roles (workflow : Workflow) (roles : Roles) (magic : SendFunc<'a>) (repos : Repository) : ExecutableInWorkflow =
//    failwith "Not implemented yed."

/// Gets all events in given workflow
let get_workflow_events (wfName : WorkflowName) (repos : Repository) : (string list) option =
    match Map.tryFind wfName repos.workflows with
    | Some(workflow) -> Some(workflow.events)
    | None -> None

// Gets the logs of the given workflow, if it is found
let get_logs (workflow_name: WorkflowName) (repository : Repository)
        : (string list) option  =
    match Map.tryFind workflow_name repository.workflows with
    | Some(workflow) -> Some(workflow.logs)
    | None -> None

//Update

/// Adds a given event to a given Workflow and returns the result
let add_event_to_workflow (event : EventName) (repo : Repository) : Repository option =
    let workflow_name, event_name = event
    match repo.workflows.TryFind(workflow_name) with
    | Some(workflow) ->
        let updated_events = event_name::(workflow.events)
        let updated_workflow = { workflow with events = updated_events; }
        let updated_workflows = Map.add workflow_name updated_workflow repo.workflows
        Some({repo with workflows = updated_workflows; })
    | None ->
        None

/// Adds a log
let add_log (event : EventName) (date : string) (userName : UserName) (repo : Repository) : Result =
    let workflow_name, event_name = event
    let log = sprintf "%s, %s, %s" event_name date userName
    match repo.workflows.TryFind(workflow_name) with
    | Some(workflow) ->
        let updated_logs = log::workflow.logs
        let updated_workflow = { workflow with logs = updated_logs; }
        let updated_workflows = Map.add workflow_name updated_workflow repo.workflows
        Ok({ repo with workflows = updated_workflows; })
    | None ->
        MissingWorkflow

//Delete

/// Removes given event form given workflow and returns the result
let remove_event_from_workflow (event : EventName) (repo : Repository) : Repository option =
    let workflow_name, event_name = event
    match Map.tryFind workflow_name repo.workflows with
    | Some(workflow) ->
        let updated_events = List.filter (fun e -> not (e = event_name)) workflow.events
        let updated_workflow = { workflow with events = updated_events; }
        let updated_workflows = Map.add workflow_name updated_workflow repo.workflows
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
