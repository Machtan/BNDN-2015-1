open System

// A result for the http functions
type HttpResult<'a> =
| Ok of string
| Error of 'a

// The state for the program
type State = {
    user: string;
    workflow: string;
    peer: string;
}

let DUMMY_GUID = "1020304050102030405010203040501020304050"

// Splits a string into a list by a char
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Uploads a string
let upload (url: string) (meth: string) (data: string) : HttpResult<exn> =
    try
        use w = new System.Net.WebClient ()
        Ok(w.UploadString(url, meth, data))
    with
        | error -> Error(error)

// Downloads a string
let download (url : string) (data: string) : HttpResult<exn> =
    try
        use w = new System.Net.WebClient ()
        let full_url = sprintf "%s?data=%s" url data
        Ok(w.DownloadString(full_url))
    with
        | error -> Error(error)

// Attempts to get a list of the events of a given workflow
let get_workflow_events (workflow: string) (peer_address: string)
        : (string list) option =
    let url = sprintf "%s/resource/workflow/%s" peer_address workflow
    match download url "" with
    | Ok(resp) ->
        Some(split resp ',')
    | Error(err) ->
        printfn "! Error: %A" err
        None

// Prints the logs of a given workflow if it exists
let print_logs (state: State): State =
    let url = sprintf "%s/resource/log/%s" state.peer state.workflow
    match download url "" with
    | Ok(resp) ->
        if resp = "" then
            printfn "> No events logged yet"
        else
            printfn "=== Logs ==="
            let logs = split resp '\n'
            List.iter (fun entry -> printfn "- %s" entry) logs
    | Error(err) ->
        printfn "! Could not get logs: '%A'" err
    state

// Changes the active workflow
let change_workflow (state: State): State =
    printfn "> Enter a new workflow:"
    printf "$ "
    let workflow = Console.ReadLine()
    { state with workflow = workflow; }

// Changes the current user
let change_user (state: State): State =
    printfn "> Enter a new user name:"
    printf "$ "
    let user = Console.ReadLine()
    { state with user = user; }

// Debugs the state of the server this client is connected to
let debug_state (state: State): State =
    let url = sprintf "%s/pastry/debug/%s" state.peer DUMMY_GUID
    match download url "" with
    | Ok(message) ->
        printfn "> ==== DEBUG ===="
        printfn "%s" message
    | Error(err) ->
        printfn "! Error: %A" err
    state
// Exits the program
let exit_program (state: State) =
    printfn "> Goodbye..."
    exit 0

// Valid actions for the user to take
let actions: (string * string * (State -> State)) list = [
    ("1", "Change workflow", change_workflow);
    ("2", "Change user", change_user);
    ("3", "Show logs", print_logs);
    ("4", "Debug", debug_state);
    ("9", "Exit program", exit_program);
]

// The main loop
let rec loop (state: State) =
    printfn "> ==== Actions ==== | %s | %s " state.user state.workflow
    let print_action (identifier, desc, _) =
        printfn "- %s: %s" identifier desc
    List.iter print_action actions
    printf "$ "
    let action = Console.ReadLine()
    let find_action (identifier, _, _) =
        action = identifier
    match List.tryFind (fun (id, _, _) -> action = id) actions with
    | Some((_, _, func)) ->
        loop <| func state
    | None ->
        printfn "! Unrecognized action '%s'" action
        loop <| state

// Checks that the peer the client is connecting to is running
let ensure_peer_is_working (peer_address: string): bool =
    printfn "> Contacting peer at '%s'..." peer_address
    let url = sprintf "%s/pastry/ping/%s" peer_address DUMMY_GUID
    match upload url "PUT" "" with
    | Ok(resp) -> true
    | Error(_) -> false

[<EntryPoint>]
let main args =
    printfn "> Starting Client..."
    match args with
    | [|addr|] ->
        let address = sprintf "http://%s" addr
        if not (ensure_peer_is_working address) then
            printfn "ERROR: Could not contact peer at '%s'!" address
        else
            printfn "> Got response, continuing..."
            let state = {
                user = "";
                workflow = "";
                peer = address;
            }
            loop state
    | [|addr; user; workflow|] ->
        let address = sprintf "http://%s" addr
        if not (ensure_peer_is_working address) then
            printfn "ERROR: Could not contact peer at '%s'!" address
        else
            printfn "> Got response, continuing..."
            let state = {
                user = user;
                workflow = workflow;
                peer = address;
            }
            loop state
    | _ ->
        printfn "Usage: NewClient.exe <peer_address> [<user> <workflow>]"
    0