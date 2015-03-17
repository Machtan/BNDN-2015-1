
let port = "8080"
let serverName = "Midlertidigt navn"
let url = sprintf "http://localhost:%s/%s" port serverName

let createEvent name =
    use w = new System.Net.WebClient ()
    let url = (sprintf " %s/%s" url name)
    w.UploadString(url, "CREATE")
        |> printfn "CREATE %s/%s --> %s" serverName name

let createRelen fromEvent reTyp toEvent =
    use w = new System.Net.WebClient ()
    let url = (sprintf " %s/%s" url fromEvent)
    w.UploadString(url, "CREATE", (sprintf "%s %s" reTyp toEvent)) 
        |> printfn "CREATE %s/%s/%s [%s] --> %s" serverName fromEvent reTyp toEvent

let parse (line : string) =
    let words = List.ofArray(line.Split [|' '|])
    match words with
        | "Event"::name::[]                     -> createEvent name
        | "Relen"::event::typ::toEvent::[]      -> createRelen event typ toEvent
        | _                                     -> failwith "prut"

[<EntryPoint>]
let main argv =
    
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- "server.exe"
    p.StartInfo.Arguments <- (serverName)
    p.Start() |> ignore

    parse "Event A 8081 Elev"
    parse "Event B 8082 Elev"
    parse "Relen A Condition B"

    0 // <- skal vare der
