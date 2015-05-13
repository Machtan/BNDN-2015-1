module Migrate

open Repository_types
open Pastry
open System

type Command = {
    path: string;
    meth: string;
    data: string;
}

type CommandType =
| CreateUser of string // name
| AddRoles of string * string * Set<string> // user, wf, roles
| CreateWorkflow of string
// workflow event included pending executed locked roles
| CreateEvent of string * string * bool * bool * bool * bool * Set<string>
// workflow event type workflow event
| AddToRelation of string * string * RelationType * string * string
| AddFromRelation of string * string * RelationType * string * string
| AddEvent of string * string
| AddLog of string * string * string * string

let cmd path meth data : Command =
    { path = path; meth = meth; data = data; }

// Gets the actual command to run from a type
let get_command (typ: CommandType) : Command =
    match typ with
    | CreateUser(user) ->
        cmd (sprintf "user/%s" user) "POST" ""

    | AddRoles(user, workflow, roles) ->
        cmd (sprintf "user/%s/roles/%s" user workflow) "PUT" (String.concat "," roles)

    | CreateWorkflow(workflow) ->
        cmd (sprintf "workflow/%s" workflow) "POST" ""

    | CreateEvent(workflow, event, included, pending, executed, locked, roles) ->
        let ser b = if b then "1" else "0"
        let initstate = (ser included) + (ser pending) + (ser executed) + (ser locked)
        let data = initstate + "," + (String.concat "," roles)
        cmd (sprintf "workflow/%s/%s" workflow event) "POST" data

    | AddToRelation(workflow, event, reltype, dst_workflow, dst_event) ->
        let rel =
            match reltype with
            | Condition -> "condition"
            | Exclusion -> "exclusion"
            | Response  -> "response"
            | Inclusion -> "inclusion"
        let path = sprintf "workflow/%s/%s/%s/to" workflow event rel
        let data = sprintf "%s,%s" dst_workflow dst_event
        cmd path "PUT" data

    | AddFromRelation(workflow, event, reltype, dst_workflow, dst_event) ->
        let rel =
            match reltype with
            | Condition -> "condition"
            | Exclusion -> "exclusion"
            | Response  -> "response"
            | Inclusion -> "inclusion"
        let path = sprintf "workflow/%s/%s/%s/from" dst_workflow dst_event rel
        let data = sprintf "%s,%s" workflow event
        cmd path "PUT" data

    | AddLog(workflow, event, datetime, user) ->
        let data = sprintf "%s,%s" datetime user
        let path = sprintf "log/%s/%s" workflow event
        cmd path "PUT" data

    | AddEvent(workflow, event) ->
        let path = sprintf "workflow/%s" workflow
        let data = event
        cmd path "PUT" data

// Debug pretty print
let print_cmd (command: Command) =
    printfn "%-5s | %-50s | %s" command.meth command.path command.data

// Gets the commands to migrate the needed parts from this repo
let get_migratable_commands (predicate: string -> bool)
    (state: PastryState<Repository>): PastryState<Repository> * Command list =
    // Add the event creation commands to the back of the list
    // And find out which events to not migrate
    let event_folder workflow events (event_map, cmds, rel_cmds) =
        let content_folder eventname (locked, state) (wf_events, i_cmds, i_rel_cmds) =
            // Get a create command for the event
            let create_cmd = CreateEvent(workflow, eventname, state.included, state.pending, state.executed, locked, state.roles)
            if predicate (get_command create_cmd).path then
                // Fold the relations in their own list (must be added after all events)
                let relation_folder (rt, (rwf, rev)) ii_rel_cmds =
                    let to_cmd = AddToRelation(workflow, eventname, rt, rwf, rev)
                    let from_cmd = AddFromRelation(workflow, eventname, rt, rwf, rev)
                    from_cmd::to_cmd::ii_rel_cmds
                wf_events, create_cmd::i_cmds, (Set.foldBack relation_folder state.toRelations i_rel_cmds)
            else
                (Map.add eventname (locked, state) wf_events), i_cmds, i_rel_cmds
        let wf_events, i_cmds, i_rel_cmds = Map.foldBack content_folder events (Map.empty, cmds, rel_cmds)
        if wf_events.Count > 0 then // Don't keep empty workflow data
            Map.add workflow wf_events event_map, i_cmds, i_rel_cmds
        else
            event_map, i_cmds, i_rel_cmds
    let (events, create_event_cmds, rel_cmds) =
        Map.foldBack event_folder state.data.events (Map.empty, [], [])

    let event_cmds = List.append create_event_cmds rel_cmds

    // Add the workflow creation commands before the events
    let workflow_folder (name: string) (workflow: Workflow) (updated_workflows, cmds) =
        let typ = CreateWorkflow(name)
        if predicate (get_command typ).path then

            let log_folder (log: string) cmds =
                let args = log.Split([|", "|], StringSplitOptions.None)
                let event = args.[0]
                let datetime = args.[1]
                let user = args.[2]
                let cmd = AddLog(name, event, datetime, user)
                cmd::cmds
            let log_commands = List.foldBack log_folder workflow.logs cmds

            let event_folder (event: string) event_cmds =
                let cmd = AddEvent(name, event)
                cmd::event_cmds
            let updated_commands = typ::(List.foldBack event_folder workflow.events log_commands)

            updated_workflows, updated_commands
        else
            Map.add name workflow updated_workflows, cmds
    let (workflows, workflow_cmds) =
        Map.foldBack workflow_folder state.data.workflows (Map.empty, event_cmds)

    // Add the user creation commands before the workflows
    let user_folder (name: string) (user: User) (users, cmds) = // string User
        let role_folder (workflow: string) (roles: Roles) role_cmds =
            (AddRoles(name, workflow, roles))::role_cmds

        let create_cmd = CreateUser(name)

        if predicate (get_command create_cmd).path then // Should migrate user
            let updated_commands = Map.foldBack role_folder user.roles cmds
            users, create_cmd::updated_commands
        else
            (Map.add name user users), cmds
    let (users, all_cmds) =
        Map.foldBack user_folder state.data.users (Map.empty, workflow_cmds)

    let new_data = { state.data with events = events; workflows = workflows; users = users; }
    let new_state = { state with data = new_data; }
    let commands = List.map get_command all_cmds
    //printfn "MIGRATE: Commands -\n%A" commands
    new_state, commands

// Gets all the migration commands (YES!)
let get_all_migration_commands (state: PastryState<Repository>) : Command list =
    let (_, cmds) = get_migratable_commands (fun _ -> true) state
    cmds