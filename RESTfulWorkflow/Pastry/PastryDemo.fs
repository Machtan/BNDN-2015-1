open Pastry

// Small-scale testing entry point
[<EntryPoint>]
let main args =
    printfn "Danish Pastry!"
    match args with
    | [|addr; port; peer|] ->
        let address = sprintf "%s:%s" (if addr = "" then "localhost" else addr) port
        printfn "? Joining pastry network at '%s'..." address
        let known_peer = if peer = "" then None else Some(peer)

        // url, method, send_func, state -> state, response
        // Yay!
        let dummy_handler path meth send_func (state: 'a) : 'a * string =
            printfn "REPOSITORY: Dummy handler is handling '%s' '%s'" meth path
            state, "Hello World"

        let node = start_server address known_peer dummy_handler 0
        ()
        // Start listening...
    | _ ->
        printfn "Usage: Pastry.exe <address> <port> <peer_address_with_port>"
    0