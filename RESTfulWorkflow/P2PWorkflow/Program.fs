module something
open Pastry
open Newtonsoft.Json
open Rest
open Repository_types

// Small-scale testing entry point
[<EntryPoint>]
let main args =
    let r  = KonoTestoKawaii
    printfn "Danish Pastry!"
    match args with
    | [|addr; port; peer; guid|] ->
        let address = sprintf "%s:%s" (if addr = "" then "localhost" else addr) port
        printfn "? Joining pastry network at '%s'..." address
        let known_peer = if peer = "" then None else Some(peer)
        printfn "Peer: %A" known_peer
        // url, method, send_func, state -> state, response
        // Yay!
        let dummy_handler path meth send_func (state: 'a) : ResourceResponse<'a> =
            printfn "%s" <| String.replicate 50 "="
            printfn "REPOSITORY: Dummy handler is handling '%s' '%s'" meth path
            printfn "%s" <| String.replicate 50 "="
            state, "Hello World", 200

        // So serious
        let dummy_serializer (state: Repository) : string =
            JsonConvert.SerializeObject state

        let pad_guid_with_zeroes (guid_str: string) =
            (String.replicate (40 - String.length guid_str) "0") + guid_str

        let guid_str = pad_guid_with_zeroes guid

        let state = {
            events = Map.empty;
            users = Map.empty;
            workflows = Map.empty;
            logs = [];
        }

        let node = test_server<Repository> address known_peer resource_handler dummy_serializer guid_str state
        ()
        // Start listening...
    | _ ->
        printfn "Usage: Pastry.exe <address> <port> <peer_address_with_port> <GUID>"
    0