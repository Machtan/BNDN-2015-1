open System
open System.Net
open System.IO
open System.Text

// A result for the http functions
type HttpResult<'a> =
| Ok of string
| Error of string * int
| ConnectionError

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
    | :? WebException as error ->
        printfn "Got exception: %A" error
        printfn "Error status: %A" error.Status
        match error.Status with
        | WebExceptionStatus.Success ->
            let response: HttpWebResponse = error.Response :?> HttpWebResponse
            let status: int = int response.StatusCode
            printfn "Status code: %d" status
            let message =
                let encoding = Encoding.GetEncoding(response.ContentEncoding)
                use is = new System.IO.StreamReader(response.GetResponseStream(), encoding)
                is.ReadToEnd()
            Error(message, status)
        | _ ->
            ConnectionError

// Downloads a string
let download (url : string) (data: string) : HttpResult<exn> =
    try
        use w = new System.Net.WebClient ()
        let full_url = sprintf "%s?data=%s" url data
        Ok(w.DownloadString(full_url))
    with
    | :? WebException as error ->
        printfn "Got exception: %A" error
        printfn "Error status: %A" error.Status
        match error.Status with
        | WebExceptionStatus.Success ->
            let response: HttpWebResponse = error.Response :?> HttpWebResponse
            let status: int = int response.StatusCode
            printfn "Status code: %d" status
            let message =
                let encoding = Encoding.GetEncoding(response.ContentEncoding)
                use is = new System.IO.StreamReader(response.GetResponseStream(), encoding)
                is.ReadToEnd()
            Error(message, status)
        | _ ->
            ConnectionError

// Attempts to get a list of the events of a given workflow
let get_workflow_events (state: State) : (string list) option =
    let url = sprintf "%s/resource/workflow/%s" state.peer state.workflow
    match download url "" with
    | Ok(resp) ->
        Some(split resp ',')
    | Error(resp, status) ->
        printfn "! Error: %d %s" status resp
        None
    | ConnectionError ->
        printfn "! Connection Error!"
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
        printfn "! Could not get logs: %d %s" status resp
    | ConnectionError ->
        printfn "! Connection Error!"
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
    | Error(resp, status) ->
        printfn "! Error: %d %s" status resp
    | ConnectionError ->
        printfn "! Connection Error!"
    state

// Shows the state of the active workflow
let show_status (state: State): State =
    if state.workflow = "" then
        printfn "! No active workflow, please change it first"
    else
        match get_workflow_events state with
        | None ->
            printfn "! Workflow '%s' not found!" state.workflow
        | Some(events) ->
            let print_event_status (event: string) =
                let event_url = sprintf "%s/resource/workflow/%s/%s" state.peer state.workflow event
                let attributes = ["included"; "pending"; "executed"; "executable"]
                let get_state (attribute: string) (stats: bool list) =
                    let url = event_url + "/" + attribute
                    match download url state.user with
                    | Ok(message) ->
                        if message = "true" then
                            true::stats
                        else
                            false::stats
                    | Error(message, status) ->
                        false::stats
                    | ConnectionError ->
                        stats
                let event_status = List.foldBack get_state attributes []
                if not ((List.length event_status) = 4) then
                    printfn "! Could not get the state of '%s'" event
                else
                    let executable = if event_status.[3] then "->" else "| "
                    let pending = if event_status.[1] then "!" else " "
                    let executed = if event_status.[2] then "x" else pending
                    let name = if event_status.[0] then " " + event else "(" + event + ")"
                    printfn "%s %s %s" executable executed name
            List.iter print_event_status events
    state

// Attemts to execute an event
let execute_event (state: State): State =
    state

// Exits the program
let exit_program (state: State) =
    printfn "> Goodbye..."
    exit 0

// Valid actions for the user to take
let actions: (string * string * (State -> State)) list = [
    ("1", "Change workflow", change_workflow);
    ("2", "Change user", change_user);
    ("3", "Show status", show_status);
    ("4", "Execute event", execute_event);
    ("7", "Show logs", print_logs);
    ("8", "Debug", debug_state);
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