
let port = "8080"
let serverName = "MidlertidigtNavn"
let url = sprintf "http://localhost:%s/%s" port serverName

let create (url : string) data =
    use w = new System.Net.WebClient ()
    if not (data = "")
    then w.UploadString(url, "POST", data) |> printfn "CREATE %s [%s] --> %s" url data
    else w.UploadString(url, "POST")       |> printfn "CREATE %s --> %s" url

let parse (line : string) =
    let words = List.ofArray(line.Split [|' '|])
    match words with
        | "Event"::name::[]                     -> create (sprintf " %s/%s" url name) ""
        | "Relen"::event::typ::toEvent::[]      -> create (sprintf " %s/%s/%s" url event typ) toEvent
        | x                                     -> printfn "%s --> Is not parseble" (List.fold (fun acc x -> acc + x + " ") "" x)

[<EntryPoint>]
let main argv =
    
    
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- "server.exe"
    p.StartInfo.Arguments <- (serverName + " " + port)
    p.Start() |> ignore

    parse "Event A"
    parse "Event B"
    parse "Relen A condition B"

    0 // <- skal vare der
