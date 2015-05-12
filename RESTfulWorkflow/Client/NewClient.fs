open System
open System.Net
open System.IO
open System.Text

// A result for the http functions
type HttpResult =
| Ok of string
| Error of string * int
| ConnectionError of exn

// The state for the program
type State = {
    user: string;
    workflow: string;
    peer: string;
}

// An event type
type Event = {
    name: string;
    included: bool;
    pending: bool;
    executed: bool;
    executable: bool;
}

type Workflow = Event list

type HttpAction =
| Download of string * string
| Upload of string * string * string

let DUMMY_GUID = "1020304050102030405010203040501020304050"

// Splits a string into a list by a char
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Prints the actions of the given list
let print_actions (actions: (string * string * 'a) list) (state: State) =
    printfn "> ==== Actions ==== | %s | %s " state.user state.workflow
    let print_action (identifier, desc, _) =
        printfn "- %s: %s" identifier desc
    List.iter print_action actions

// Does some http stuff (like error handling)
let send_http (action: HttpAction) : HttpResult =
    try
        use w = new System.Net.WebClient ()
        match action with
        | Download(url, data) ->
            let full_url = sprintf "%s?data=%s" url data
            Ok(w.DownloadString(full_url))
        | Upload(url, meth, data) ->
            Ok(w.UploadString(url, meth, data))
    with
    | :? WebException as error ->
        //printfn "Got exception: %A" error
        //printfn "Error status: %A" error.Status
        match error.Status with
        | WebExceptionStatus.Success | WebExceptionStatus.ProtocolError ->
            let response: HttpWebResponse = error.Response :?> HttpWebResponse
            let status: int = int response.StatusCode
            let message =
                let encoding = Encoding.UTF8
                use is = new System.IO.StreamReader(response.GetResponseStream(), encoding)
                is.ReadToEnd()
            //printfn "Web error: %d | %s" status message
            Error(message, status)
        | _ ->
            printfn "WebException error status: %A" error.Status
            Error(sprintf "Unhandled web exception: %A" error, 400)
    | error ->
        ConnectionError(error)

// Uploads a string
let upload (url: string) (meth: string) (data: string) : HttpResult =
    send_http <| Upload(url, meth, data)

// Downloads a string
let download (url : string) (data: string) : HttpResult =
    send_http <| Download(url, data)

// Attempts to get a list of the events of a given workflow
let get_workflow_events (state: State) : (string list) option =
    let url = sprintf "%s/resource/workflow/%s" state.peer state.workflow
    match download url "" with
    | Ok(resp) ->
        Some(split resp ',')
    | Error(resp, status) ->
        printfn "! Get Workflow Events: Error - %d %s" status resp
        None
    | ConnectionError(error) ->
        printfn "! Get Workflow Events: Connection Error - %A!" error
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
    | Error(resp, status) ->
        printfn "! Print logs: Could not get logs - %d %s" status resp
    | ConnectionError(error) ->
        printfn "! Print Logs: Connection Error - %A!" error
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
    let url = sprintf "%s/resource/debug/%s" state.peer state.workflow
    match download url "" with
    | Ok(message) ->
        printfn "> ==== DEBUG ===="
        printfn "%s" message
    | Error(resp, status) ->
        printfn "! Debug state: Error - %d %s" status resp
    | ConnectionError(error) ->
        printfn "! Debug state: Connection Error - %A!" error
    state

// Gets the state of the active workflow
let get_workflow (state: State): Workflow option =
    match get_workflow_events state with
    | None ->
        None
    | Some(events) ->
        let folder (event: string) (workflow: Workflow): Workflow =
            let event_url = sprintf "%s/resource/workflow/%s/%s" state.peer state.workflow event
            let attributes = ["included"; "pending"; "executed"; "executable"]
            let get_state (attribute: string) (attrs: bool list): bool list =
                let url = sprintf "%s/%s" event_url attribute
                match download url state.user with
                | Ok(message) ->
                    if message = "true" then
                        true::attrs
                    else
                        false::attrs
                | Error(message, status) ->
                    false::attrs
                | ConnectionError(error) ->
                    attrs
            let status = List.foldBack get_state attributes []
            if (List.length status) = 4 then
                let event_state = {
                    name = event;
                    included = status.[0];
                    pending = status.[1];
                    executed = status.[2];
                    executable = status.[3];
                }
                event_state::workflow
            else
                printfn "! Get Workflow: Could not load event '%s'" event
                workflow
        let workflow = List.foldBack folder events []

        let sorter event =
            let n = event.name
            match event.pending, event.executable, event.included with
            | true, true, true -> (1, n)
            | _, true, _ -> (2, n)
            | _, _, true -> (3, n)
            | true, _, _ -> (4, n)
            | _ -> (5, n)

        let sorted = List.sortBy sorter workflow
        Some(sorted)

// Shows the status of the workflow
let show_status_from_workflow (workflow: Workflow) =
    let print_event_status (event: Event) =
        let executable = if event.executable then "->" else "| "
        let pending = if event.pending then "!" else " "
        let executed = if event.executed then "x" else " "
        let included = if event.included then "i" else " "
        printfn "%s %s%s%s %s" executable included pending executed event.name
    List.iter print_event_status workflow

// Shows the state of the active workflow
let show_status (state: State): State =
    if state.workflow = "" then
        printfn "! No active workflow, please change it first"
    else
        match get_workflow state with
        | None ->
            printfn "! Workflow '%s' not found!" state.workflow
        | Some(workflow) ->
            show_status_from_workflow workflow
    state

// Exits the program
let exit_program (state: State) =
    printfn "> Goodbye..."
    exit 0

let EXECUTE_ACTIONS = [
    ("q", "Cancel", (fun () -> false));
]

// Attempts to execute an event
let rec execute_event (state: State) (updated: bool): State =
    match get_workflow state with
    | Some(workflow) ->
        if updated then
            show_status_from_workflow workflow
        let filter (event: Event) = event.executable
        let folder (event: Event) (num, actions) =
            let func (): bool =
                let url = sprintf "%s/resource/workflow/%s/%s/executed" state.peer state.workflow event.name
                match upload url "PUT" state.user with
                | Ok(resp) ->
                    printfn "> Executed!"
                    true
                | Error(resp, status) ->
                    printfn "! Execute: Error - %d | %s" status resp
                    false
                | ConnectionError(error) ->
                    printfn "! Execute: Connection error - %A!" error
                    false
            num - 1, (string num, event.name, func)::actions
        let executable_events: Event list = List.filter filter workflow
        let fold_state = (List.length executable_events, [])
        let (_, enumerated_actions) = List.foldBack folder executable_events fold_state
        let actions = List.append <| enumerated_actions <| EXECUTE_ACTIONS
        print_actions actions state
        printf "$ "
        let action = Console.ReadLine()
        let find_action (identifier, _, _) =
            action = identifier
        match List.tryFind (fun (id, _, _) -> action = id) actions with
        | Some((_, _, func)) ->
            let should_continue = func()
            match should_continue with
            | true  -> execute_event state true
            | false -> state
        | None ->
            printfn "! Unrecognized action '%s'" action
            execute_event state false
    | None ->
        printfn "! Could not connect to the workflow"
        state

// Valid actions for the user to take
let MAIN_ACTIONS: (string * string * (State -> State)) list = [
    ("1", "Change workflow", change_workflow);
    ("2", "Change user", change_user);
    ("3", "Show status", show_status);
    ("4", "Execute events", fun state -> execute_event state true);
    ("l", "Show logs", print_logs);
    ("d", "Debug", debug_state);
    ("q", "Exit program", exit_program);
]

// The main loop
let rec loop (state: State) =
    print_actions MAIN_ACTIONS state
    printf "$ "
    let action = Console.ReadLine()
    let find_action (identifier, _, _) =
        action = identifier
    match List.tryFind (fun (id, _, _) -> action = id) MAIN_ACTIONS with
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
    | _ -> false

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