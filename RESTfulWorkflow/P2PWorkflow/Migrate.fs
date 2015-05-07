module Migrate

open Repository_types

type Command = {
    path: string;
    meth: string;
    data: string;
}

//type RelationType =                 //The types of relations
//    | Condition of string
//    | Exclusion of string
//    | Response of string
//    | Inclusion of string

type CommandType =
| CreateUser of string // name
| AddRoles of string * string * Set<string> // user, wf, roles
| CreateWorkflow of string * (string list) // name events
// workflow name included pending executed roles
| CreateEvent of string * string * bool * bool * bool * Set<string>

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
    | CreateEvent(workflow,event, included, pending, executed, roles) ->
        let ser b = if b then "1" else "0"
        let initstate = (ser included) + (ser pending) + (ser executed)
        let data = initstate + "," + (String.concat "," roles)
        cmd (sprintf "workflow/%s/%s" workflow event) "POST" data

let print_cmd (typ: CommandType) =
    let command = get_command typ
    printfn "%-5s | %-40s | %s" command.meth command.path command.data

let get_all_migrate_commands (repo: Repository) : CommandType list =
    // Add the event creation commands to the back of the list
    let event_folder workflow events acc =
        let content_folder eventname (locked, state) acc' =
            (CreateEvent(workflow, eventname, state.included, state.pending,
                state.executed, state.roles))::acc'
        Map.foldBack content_folder events acc
    let event_cmds = Map.foldBack event_folder repo.events []

    // Add the workflow creation commands before the events
    let workflow_folder name events acc =
        (CreateWorkflow(name, events))::acc
    let workflow_cmds = Map.foldBack workflow_folder repo.workflows event_cmds

    // Add the user creation commands before the workflows
    let user_folder name user acc = // string User
        let (_, permissions) = user
        let perm_folder (wfname, roles) acc' =
            (AddRoles(name, wfname, roles))::acc'
        let create_cmd = CreateUser(name)
        create_cmd::(List.foldBack perm_folder permissions acc)
    let all_cmds = Map.foldBack user_folder repo.users workflow_cmds

    List.iter print_cmd all_cmds

    all_cmds

[<EntryPoint>]
let main args =
    printfn "Hello migrator"

    let user_list = [
        ("Bob", ("Bob", [("Hospital", Set.ofList ["Medic"; "Patient"; "Janitor"])]));
        ("Alice", ("Alice", [("Jail", Set.ofList ["Medic"; "Inmate"; "Head honcho"])]));
    ]
    let repo_list = [
        ("Hospital", ["Enter"; "Leave"; "Surgery"; "Terrible accident"; "Zombie outbreak"]);
        ("Kindergarten", ["Enter"; "Eat"; "Run with scissors"; "Play with mud"])
    ]
    let eve w n i p e r t =
        let event = {
            name = (w, n);
            included = i;
            pending = p;
            executed = e;
            toRelations = Set.ofList t;
            fromRelations = Set.empty;
            roles = Set.ofList r
        }
        (true, event)
    let event_list = [
        ("Hospital", Map.ofList [
            ("Enter", eve "Hospital" "Enter" true true false ["Patient"] [(Condition,("Hospital", "Leave")); (Condition, ("Hospital", "Surgery"))]);
            ("Surgery", eve "Hospital" "Surgery" false false true ["Medic"] [(Condition, ("Hospital", "Terrible accident"))]);
            ("Zombie outbreak", eve "Hospital" "Zombie outbreak" true false false ["Janitor"] []);
        ]);
    ]
    let test_repository = {
        events = Map.ofList event_list;
        users = Map.ofList user_list;
        workflows = Map.ofList repo_list;
        logs = [];
    }

    ignore <| get_all_migrate_commands test_repository
    0