open System.IO

let port = "8080"
let serverName = "Test"
let url = sprintf "http://localhost:%s/%s" port serverName
let roles = []

//Put all given files names in a map
let rec addFilesToMap (files : FileInfo []) : Map<string,string> =
        let names = Array.fold (fun names' (x : FileInfo) -> (x.Name)::names') [] files
        let rec inner fileMap n filesName : Map<string,string> =
            match filesName with
            | []    -> fileMap
            | x::xs -> inner (fileMap.Add (n.ToString(),x)) (n+1) xs
        inner Map.empty 1 names

// adds a new role to the workflow
let addRole name roles =
    if List.exists (fun x -> x = name) roles
    then printfn "ERROR: The role (%s) do alderady exist" name; roles
    else name::roles

// POST new events and ralentienships to the server
let post (url : string) data roles =
    use w = new System.Net.WebClient ()
    try
        if not (data = "")
        then w.UploadString(url, "POST", data) |> printfn "POST %s [%s] --> %s" url data
        else w.UploadString(url, "POST")       |> printfn "POST %s --> %s" url
    with
        | x -> 
            printfn "POST %s [%s] --> \n%s" url data x.Message
            printfn "ERROR: The workflow can not be completed. continue?"
            System.Console.ReadKey() |> ignore
    roles

// parse's a string into a spisifik post to the server
let parse (line : string) roles =
    let words = List.ofArray(line.Split ' ')
    match words with
        | "Role"::name::[]                      -> addRole name roles;
        | "Event"::name::role::[]               ->
            if List.exists (fun x -> x = role) roles
            then post (sprintf " %s/%s" url name) role roles
            else printfn "ERROR: The role \"%s\" do not exist" role; roles
        | "Relen"::event::typ::toEvent::[]      -> post (sprintf " %s/%s/%s" url event typ) toEvent roles
        | x                                     -> printfn "ERROR: \"%s\" Is not parseble" (List.fold (fun acc x -> acc + x + " ") "" x); roles

//Parse alle lines in select file or writen
let rec parseTxtFile fileMap =
    Map.iter (fun key filename -> printfn "%s : %s" key filename) fileMap
    printfn "Select a file or a filepath"
    let filename = System.Console.ReadLine()
    let filename =  match fileMap.TryFind filename with
                    | None          -> filename
                    | Some(name)    -> name
    if File.Exists(filename)
    then File.ReadAllLines(filename) |> List.ofArray |> List.fold (fun roles line -> parse line roles) []
    else printfn "ERROR: Could not find file"; parseTxtFile fileMap

[<EntryPoint>]
let main argv =

    //Try to make and move a new event.exe, from teh Event project
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

    //Find a list of alle available txt files
    let dir = new DirectoryInfo(Directory.GetCurrentDirectory())
    let files = dir.GetFiles("*.txt")
    let fileMap = addFilesToMap files

    parseTxtFile fileMap |> ignore

    printfn "All line in the file have been iterated. Exit?"
    System.Console.ReadKey() |> ignore

    0 // <- skal vare der
