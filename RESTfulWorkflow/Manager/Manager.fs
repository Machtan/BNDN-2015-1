
let port = "8080"

let mutable events = "test"::[]

let CreateEvent name =
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- "Node.exe"
    p.StartInfo.Arguments <- (name)
    ignore (p.Start())
    ignore (events = name::events)
    printfn "Created event %s" name

let Put event reTyp tohttp =
    let url = sprintf "http://localhost:%s/%s/(%s,%s)" port event reTyp tohttp

    use w = new System.Net.WebClient ()
    w.UploadString(url, "PUT", "new relation")      |> printfn "PUT /%s/%s [%s] --> %s" event reTyp tohttp



[<EntryPoint>]
let main argv =
    CreateEvent "A"
    // relation: condition, response, exclusion or inclusion.
    //Put "A" "condition" "http://localhost:8080/A"

    let rec writeNames = function
        | [] -> ""
        | x::xs -> x + " " + (writeNames xs)

    printfn "%s" (writeNames events)

    System.Console.ReadLine()
    printfn "Exsiting manager"
    0
