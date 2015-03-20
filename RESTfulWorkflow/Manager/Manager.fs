open System.IO

let port = "8080"
let serverName = "Test"
let url = sprintf "http://localhost:%s/%s" port serverName

// POST new events and ralentienships to the server
let post (url : string) data =
    use w = new System.Net.WebClient ()
    if not (data = "")
    then w.UploadString(url, "POST", data) |> printfn "POST %s [%s] --> %s" url data
    else w.UploadString(url, "POST")       |> printfn "POST %s --> %s" url

// parse's a string into a spisifik post to the server
let parse (line : string) =
    let words = List.ofArray(line.Split ' ')
    match words with
        | "Event"::name::[]                     -> post (sprintf " %s/%s" url name) ""
        | "Relen"::event::typ::toEvent::[]      -> post (sprintf " %s/%s/%s" url event typ) toEvent
        | x                                     -> printfn "%s --> Is not parseble" (List.fold (fun acc x -> acc + x + " ") "" x)

[<EntryPoint>]
let main argv =

    // Try to make and move a new event.exe, from teh Event project
    if File.Exists("event.exe")
    then File.Delete("event.exe")
    File.Copy(@"C:\Users\stin7054\Documents\GitHub\BNDN-2015-1\RESTfulWorkflow\Event\bin\Debug\Event.exe",@"C:\Users\stin7054\Documents\GitHub\BNDN-2015-1\RESTfulWorkflow\Event\bin\Debug\Event2.exe")
    File.Move(@"C:\Users\stin7054\Documents\GitHub\BNDN-2015-1\RESTfulWorkflow\Event\bin\Debug\Event2.exe", @"C:\Users\stin7054\Documents\GitHub\BNDN-2015-1\RESTfulWorkflow\Manager\bin\Debug\event.exe");

    //Starts the server form the .exe fil server form same plasments as the program
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- "event.exe"
    p.StartInfo.Arguments <- (serverName + " " + port)
    p.Start() |> ignore

    //A test identical to the one given i the project decripion
    parse "Event register"
    parse "Event pass"
    parse "Event fail"

    parse "Relen register exclusion register"
    parse "Relen register condition pass"
    parse "Relen register response pass"
    parse "Relen register condition fail"

    parse "Relen pass exclusion pass"
    parse "Relen pass exclusion fail"

    parse "Relen fail exclusion fail"
    parse "Relen fail exclusion pass"

    System.Console.ReadLine()

    0 // <- skal vare der
