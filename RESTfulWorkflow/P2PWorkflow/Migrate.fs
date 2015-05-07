module Migrate

open Repository_types

type Command = {
    path: string;
    meth: string;
    data: string;
}

type CommandType =
| CreateUser of string // name
| AddRoles of string * string * Set<string> // user, wf, roles
| CreateWorkflow of string * (string list) // name events
// workflow event included pending executed locked roles
| CreateEvent of string * string * bool * bool * bool * bool * Set<string>
// workflow event type workflow event
| AddRelation of string * string * RelationType * string * string

let cmd path meth data : Command =
    { path = path; meth = meth; data = data; }

// Gets the actual command to run from a type
let get_command (typ: CommandType) : Command =
    match typ with
    | CreateUser(user) ->
        cmd (sprintf "user/%s" user) "POST" ""
    | AddRoles(user, workflow, roles) ->
        cmd (sprintf "user/%s/%s" user workflow) "PUT" (String.concat "," roles)
    | CreateWorkflow(workflow, events) ->
        cmd (sprintf "workflow/%s" workflow) "POST" (String.concat "," events)
    | CreateEvent(workflow, event, included, pending, executed, locked, roles) ->
        let ser b = if b then "1" else "0"
        let initstate = (ser included) + (ser pending) + (ser executed) + (ser locked)
        let data = initstate + "," + (String.concat "," roles)
        cmd (sprintf "workflow/%s/%s" workflow event) "POST" data
    | AddRelation(workflow, event, reltype, dst_workflow, dst_event) ->
        let rel =
            match reltype with
            | Condition -> "condition"
            | Exclusion -> "exclusion"
            | Response  -> "response"
            | Inclusion -> "inclusion"
        let path = sprintf "workflow/%s/%s/%s/to" workflow event rel
        let data = sprintf "%s,%s" dst_workflow dst_event
        cmd path "PUT" data

// Debug pretty print
let print_cmd (command: Command) =
    printfn "%-5s | %-50s | %s" command.meth command.path command.data

// Gets the commands to migrate the needed parts from this repo
let get_migratable_commands (repo: Repository) (predicate: string -> bool)
    : Repository * Command list =
    // Add the event creation commands to the back of the list
    // And find out which events to not migrate
    let event_folder workflow events (event_map, cmds) =
        let content_folder eventname (locked, state) (wf_events, i_cmds) =
            let typ = CreateEvent(workflow, eventname, state.included, state.pending, state.executed, locked, state.roles)
            if predicate (get_command typ).path then
                let relation_folder (rt, (rwf, rev)) ii_cmds =
                    (AddRelation(workflow, eventname, rt, rwf, rev))::ii_cmds
                wf_events, typ::(Set.foldBack relation_folder state.toRelations i_cmds)
            else
                (Map.add eventname (locked, state) wf_events), i_cmds
        let wf_events, i_cmds = Map.foldBack content_folder events (Map.empty, cmds)
        if wf_events.Count > 0 then // Don't keep empty workflow data
            Map.add workflow wf_events event_map, i_cmds
        else
            event_map, i_cmds
    let (events, event_cmds) = Map.foldBack event_folder repo.events (Map.empty, [])

    // Add the workflow creation commands before the events
    let workflow_folder name events (workflow, cmds) =
        let typ = CreateWorkflow(name, events)
        if predicate (get_command typ).path then
            workflow, typ::cmds
        else
            Map.add name events workflow, cmds
    let (workflows, workflow_cmds) = Map.foldBack workflow_folder repo.workflows (Map.empty, event_cmds)

    // Add the user creation commands before the workflows
    let user_folder name user (users, cmds) = // string User
        let (_, permissions) = user
        let perm_folder (wfname, roles) acc' =
            (AddRoles(name, wfname, roles))::acc'
        let create_cmd = CreateUser(name)
        if predicate (get_command create_cmd).path then // Should migrate user
            users, create_cmd::(List.foldBack perm_folder permissions cmds)
        else
            (Map.add name user users), cmds
    let (users, all_cmds) = Map.foldBack user_folder repo.users (Map.empty, workflow_cmds)

    let updated_repo = { repo with events = events; workflows = workflows; users = users; }
    updated_repo, List.map get_command all_cmds

// Gets all the migration commands (YES!)
let get_all_migration_commands (repo: Repository) : Command list =
    let (_, cmds) = get_migratable_commands repo (fun _ -> true)
    cmds