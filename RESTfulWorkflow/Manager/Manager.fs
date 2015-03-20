open System.IO

let port = "8080"
let serverName = "Test"
let url = sprintf "http://localhost:%s/%s" port serverName
let roles = []

// adds a new role to the workflow
let addRole name roles =
    if List.exists (fun x -> x = name) roles
    then printfn "%s --> The role do alderady exist" name; roles
    else name::roles

// POST new events and ralentienships to the server
let post (url : string) data roles =
    use w = new System.Net.WebClient ()
    if not (data = "")
    then w.UploadString(url, "POST", data) |> printfn "POST %s [%s] --> %s" url data
    else w.UploadString(url, "POST")       |> printfn "POST %s --> %s" url
    roles

// parse's a string into a spisifik post to the server
let parse (line : string) roles =
    let words = List.ofArray(line.Split ' ')
    match words with
        | "Role"::name::[]                      -> addRole name roles;
        | "Event"::name::role::[]               ->
            if List.exists (fun x -> x = role) roles
            then post (sprintf " %s/%s" url name) role roles
            else printfn "%s --> Do not exist" name; roles
        | "Relen"::event::typ::toEvent::[]      -> post (sprintf " %s/%s/%s" url event typ) toEvent roles
        | x                                     -> printfn "%s --> Is not parseble" (List.fold (fun acc x -> acc + x + " ") "" x); roles

//Her kan vi iterere gennem arrayet med linjer fra text filen.
//File.ReadAllLines("test.txt") |> Array.iteri (fun i line -> printfn "%d> %s" (i + 1) line) //Reads each line into an array. Thereafter iterates through the list and prints each line.

//Atleast i tried lal.
//File.ReadAllLines("test.txt") |> Array.iteri (fun i line -> parse"(i + 1) line")  //Tænker det er noget i denne retning der skal bruges.


[<EntryPoint>]
let main argv =

    // Try to make and move a new event.exe, from teh Event project
    #if TARGET_MAC
    #else
    if File.Exists("event.exe")
    then File.Delete("event.exe")
    File.Copy(@"..\..\..\Event\bin\Debug\Event.exe",@"Event.exe")

    //Starts the server form the .exe fil server form same plasments as the program
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- "event.exe"
    p.StartInfo.Arguments <- (serverName + " " + port)
    p.Start() |> ignore
    #endif

    //A test identical to the one given i the project decripion
    let roles = parse "Role Student" []
    let roles = parse "Role Teacher" roles

    let roles = parse "Event register Student" roles
    let roles = parse "Event pass Teacher" roles
    let roles = parse "Event fail Teacher" roles

    let roles = parse "Relen register exclusion register" roles
    let roles = parse "Relen register condition pass" roles
    let roles = parse "Relen register response pass" roles
    let roles = parse "Relen register condition fail" roles

    let roles = parse "Relen pass exclusion pass" roles
    let roles = parse "Relen pass exclusion fail" roles

    let roles = parse "Relen fail exclusion fail" roles
    let roles = parse "Relen fail exclusion pass" roles

    //System.Console.ReadLine()

    0 // <- skal vare der
