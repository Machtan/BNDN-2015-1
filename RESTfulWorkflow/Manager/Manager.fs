
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
        | x                                     -> printfn "%s --> Is not parseble" (List.fold (fun acc x -> acc + x + " ") "" x)

[<EntryPoint>]
let main argv =
    
    (*
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- "server.exe"
    p.StartInfo.Arguments <- (serverName)
    p.Start() |> ignore*)

    parse "Event A"
    parse "Event B"
    parse "Relen A Condition B"

    0 // <- skal vare der
