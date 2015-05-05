open Pastry
open Newtonsoft.Json

// Small-scale testing entry point
[<EntryPoint>]
let main args =
    printfn "Danish Pastry!"
    match args with
    | [|addr; port; peer|] ->
        let address = sprintf "%s:%s" (if addr = "" then "localhost" else addr) port
        printfn "? Joining pastry network at '%s'..." address
        let known_peer = if peer = "" then None else Some(peer)
        printfn "Peer: %A" known_peer
        // url, method, send_func, state -> state, response
        // Yay!
        let dummy_handler path meth send_func (state: 'a) : ResourceResponse<'a> =
            printfn "REPOSITORY: Dummy handler is handling '%s' '%s'" meth path
            state, "Hello World", 200

        // So serious
        let dummy_serializer (state: 'a) : string =
            JsonConvert.SerializeObject state

        let node = start_server address known_peer dummy_handler dummy_serializer 0
        ()
        // Start listening...
    | _ ->
        printfn "Usage: Pastry.exe <address> <port> <peer_address_with_port>"
    0