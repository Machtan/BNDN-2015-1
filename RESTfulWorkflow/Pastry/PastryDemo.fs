open Pastry
open Newtonsoft.Json

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
        let dummy_handler path meth data send_func (state: PastryState<'a>)
                : ResourceResponse<'a> =
            printfn "%s" <| String.replicate 50 "="
            printfn "REPOSITORY: Dummy handler is handling '%s' '%s' '%s'" meth path data
            printfn "%s" <| String.replicate 50 "="
            resource_response state "Hello World" 200

        // So serious
        let dummy_serializer (state: 'a) : string =
            JsonConvert.SerializeObject state

        let pad_guid_with_zeroes (guid_str: string) =
            (String.replicate (40 - String.length guid_str) "0") + guid_str

        let guid_str = pad_guid_with_zeroes guid

        let node = test_server address known_peer dummy_handler dummy_serializer guid_str 0
        ()
        // Start listening...
    | _ ->
        printfn "Usage: Pastry.exe <address> <port> <peer_address_with_port> <GUID>"
    0