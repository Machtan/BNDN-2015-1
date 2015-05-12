module WorkflowLogic

open EventLogic
open Pastry
open Repository_types

//Create

/// Creates and returns a new workflow from a name
let create_workflow (wfName : WorkflowName) (state : PastryState<Repository>)
        : Result =
    let workflow = {
        name = wfName;
        events = [];
        logs = [];
    }
    let new_data = {state.data with workflows = Map.add wfName workflow state.data.workflows}
    Result.Ok({state with data = new_data;})

//Read
let check_workflow (wfName: WorkflowName) (state: PastryState<Repository>)
        : bool =
    match state.data.workflows.TryFind(wfName) with
    | Some(_) ->
        true
    | None ->
        false

/// Gets all events in given workflow
let get_workflow_events (wfName: WorkflowName) (state: PastryState<Repository>)
        : (string list) option =
    match Map.tryFind wfName state.data.workflows with
    | Some(workflow) ->
        Some(workflow.events)
    | None ->
        None

// Gets the logs of the given workflow, if it is found
let get_logs (workflow_name: WorkflowName) (state : PastryState<Repository>)
        : (string list) option  =
    match Map.tryFind workflow_name state.data.workflows with
    | Some(workflow) ->
        Some(workflow.logs)
    | None ->
        None

//Update

/// Adds a given event to a given Workflow and returns the result
let add_event_to_workflow (event : EventName) (state : PastryState<Repository>)
        : PastryState<Repository> option =
    let workflow_name, event_name = event
    match state.data.workflows.TryFind(workflow_name) with
    | Some(workflow) ->
        let updated_events = event_name::(workflow.events)
        let updated_workflow = { workflow with events = updated_events; }
        let updated_workflows = Map.add workflow_name updated_workflow state.data.workflows
        Some({state with data = {state.data with workflows = updated_workflows;};})
    | None ->
        None

/// Adds a log
let add_log (event : EventName) (date : string) (user : UserName)
        (state : PastryState<Repository>) : Result =
    let workflow_name, event_name = event
    let log = sprintf "%s, %s, %s" event_name date user
    match state.data.workflows.TryFind(workflow_name) with
    | Some(workflow) ->
        let updated_logs = log::workflow.logs
        let updated_workflow = { workflow with logs = updated_logs; }
        let updated_workflows = Map.add workflow_name updated_workflow state.data.workflows
        Ok({state with data = { state.data with workflows = updated_workflows; }})
    | None ->
        MissingWorkflow(state)

//Delete

/// Removes given event form given workflow and returns the result
let remove_event_from_workflow (event : EventName)
        (state : PastryState<Repository>) : PastryState<Repository> option =
    let workflow_name, event_name = event
    match Map.tryFind workflow_name state.data.workflows with
    | Some(workflow) ->
        let updated_events = List.filter (fun e -> not (e = event_name)) workflow.events
        let updated_workflow = { workflow with events = updated_events; }
        let updated_workflows = Map.add workflow_name updated_workflow state.data.workflows
        Some({ state with data = { state.data with workflows = updated_workflows; }})
    | None ->
        None

/// Deletes given workflow and returns it if its susesful (also deletes the events in its list)
let delete_workflow (wfName : WorkflowName) (send_func: SendFunc<Repository>)
        (state : PastryState<Repository>) : PastryState<Repository> option =
    match state.data.workflows.TryFind(wfName) with
    | Some(workflow) ->
        let delete_event event new_state =
            let res = delete_event (wfName, event) send_func new_state
            match res with
            | Result.Ok(result_state) ->
                result_state
            | Result.Error(msg, result_state) ->
                result_state
            | Unauthorized(rs) | NotExecutable(rs) | MissingRelation(rs)
            | MissingEvent(rs) | LockConflict(rs)  | MissingWorkflow(rs) ->
                printfn "Got error %A when deleting event %s" res event
                rs
        let new_state = List.foldBack delete_event workflow.events state
        let new_data = {new_state.data with workflows = Map.remove wfName new_state.data.workflows}
        Some({new_state with data = new_data;})
    | None ->
        None