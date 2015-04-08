module Manager

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

//Adds a new role to the workflow
let addRole name roles =
    if List.exists (fun x -> x = name) roles
    then printfn "WARNING: The role (%s) do alderady exist" name; roles
    else name::roles

//POST new events and relationships to the server
let post (urladdon : string) data =
    use w = new System.Net.WebClient ()
    try
        if not (data = "")
        then w.UploadString((sprintf " %s/%s" url urladdon), "POST", data) |> printfn "POST /%s [%s] --> %s" urladdon data
        else w.UploadString((sprintf " %s/%s" url urladdon), "POST")       |> printfn "POST /%s --> %s" urladdon
    with
        | x ->
            printfn "POST %s/%s [%s] --> \n%s" url urladdon data x.Message
            printfn "ERROR: The workflow can not be completed. continue?"
            System.Console.ReadKey() |> ignore

//Parses a string into a specific post to the server.
let parse (line : string) roles useroles =
    let words = List.ofArray(line.Split ' ')
    match words with
    | "rol"::name::[] -> addRole name roles
    | "eve"::flag::name::eventroles -> 
        if useroles
        then
            if List.forall (fun x -> List.exists (fun x' -> x' = x) roles) eventroles
            then post name (sprintf "%s %s" flag (String.concat " " eventroles))
        else post name flag
        roles
    | "rel"::event::typ::toEvent::[] -> 
        post (sprintf "%s/%s" event typ) toEvent
        roles
    | "//"::xs | "#"::xs -> roles
    | ""::[] -> roles
    | x -> 
        printfn "WARNING: \"%s\" Is not parseble" (String.concat " " x)
        roles

//Parse all lines in selected file or written file.
let rec promptParseFile fileMap =
    Map.iter (fun key filename -> printfn "%s : %s" key filename) fileMap
    printfn "Select a file or a filepath"
    let filename = System.Console.ReadLine()
    let filename =
        match fileMap.TryFind filename with
        | None          -> filename
        | Some(name)    -> name

    printfn "Use roles? (y/n)"
    let useroles = System.Console.ReadLine()
    let useroles =
        match useroles with
        | "y"           -> true
        | "n"           -> false
        | _             -> printfn "Defaults to yes" ; true

    if File.Exists(filename)
    then
        let folder roles line =
            System.Threading.Thread.Sleep(10)
            parse line roles useroles
        File.ReadAllLines(filename) |> List.ofArray |> List.fold folder []
    else
        printfn "ERROR: Could not find file"
        promptParseFile fileMap

let Start =

    //Try to make and move a new event.exe, from the Event project
    #if TARGET_MAC
    #else
    if File.Exists("event.exe")
    then File.Delete("event.exe")
    File.Copy(@"..\..\..\Event\bin\Debug\Event.exe",@"Event.exe")

    //Starts the server form the .exe fil server from same placement as the program.
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- "event.exe"
    p.StartInfo.Arguments <- (serverName + " " + port)
    p.Start() |> ignore
    #endif

    //Find a list of alle available txt files
    let dir = new DirectoryInfo(Directory.GetCurrentDirectory())
    let files = dir.GetFiles("*.txt")
    let fileMap = addFilesToMap files

    promptParseFile fileMap |> ignore

    printfn "All lines in the file have been iterated. Exit?"
    System.Console.ReadKey() |> ignore
