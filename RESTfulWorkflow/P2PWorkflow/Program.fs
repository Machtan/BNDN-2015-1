module something
open Pastry
open Newtonsoft.Json
open Rest
open Repository_types

// Small-scale testing entry point
[<EntryPoint>]
let main args =
    printfn "Danish Pastry!"
    match args with
    | [|addr; port; peer; guid|] ->
        let address = sprintf "%s:%s" (if addr = "" then "localhost" else addr) port
        printfn "? Joining pastry network at '%s'..." address
        let known_peer = if peer = "" then None else Some(peer)
        printfn "Peer: %A" known_peer
        // url, method, send_func, state -> state, response
        // Yay!

        // So serious
        let serialize_state (state: Repository) : string =
            JsonConvert.SerializeObject state

        let pad_guid_with_zeroes (guid_str: string) =
            (String.replicate (40 - String.length guid_str) "0") + guid_str

        let guid_str = pad_guid_with_zeroes guid

        let state = {
            events = Map.empty;
            users = Map.empty;
            workflows = Map.empty;
            logs = Map.empty;
        }

        let handler = handle_resource // RestAPI

        let node = test_server<Repository> address known_peer handler serialize_state guid_str state
        ()
        // Start listening...
    | _ ->
        printfn "Usage: Pastry.exe <address> <port> <peer_address_with_port> <GUID>"
    0