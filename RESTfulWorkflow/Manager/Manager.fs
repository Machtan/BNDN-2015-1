module Manager

open System.IO

let port = "8080"
let url = sprintf "http://localhost:%s/resource" port
let user_url = sprintf "%s/user" url
let roles = []

let get_wf (workflow: string) =
    sprintf "%s/workflow/%s" url workflow

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
    then printfn "WARNING: The role (%s) already exists" name; roles
    else name::roles

type Query =
| Workflow of string
| User of string


//POST new events and relationships to the server
let post (query : Query) (data: string) (wf: string) =
    use w = new System.Net.WebClient ()
    let query_url =
        match query with
        | Workflow(path) -> sprintf "%s/%s" (get_wf wf) path
        | User(path) -> sprintf "%s/%s" user_url path
    try
        if not (data = "")
        then w.UploadString(query_url, "POST", data) |> printfn "POST /%s [%s] \n\t--> %s" query_url data
        else w.UploadString(query_url, "POST")       |> printfn "POST /%s \n\t--> %s" query_url
    with
        | x ->
            printfn "POST %s [%s] --> \n%s" query_url data x.Message
            printfn "ERROR: The workflow can not be completed. continue?"
            System.Console.ReadKey() |> ignore

//Parses a string into a specific post to the server.
let parse (line : string) roles useroles (workflow: string) : string list * string=
    let words = List.ofArray(line.Split ' ')
    match words with
    | "wor"::name::[] ->
        printfn ">> Setting workflow name to '%s'" name
        [], name
    | "rol"::name::[] ->
        (addRole name roles), workflow
    | "eve"::flags::name::eventroles ->
        if useroles
        then
            if List.forall (fun x -> List.exists (fun x' -> x' = x) roles) eventroles
            then
                let data = sprintf "%s,%s" flags (String.concat "," eventroles)
                post (Workflow(name)) data workflow
        else post (Workflow(name)) flags workflow
        roles, workflow
    | "rel"::event::typ::toEvent::[] ->
        let query = Workflow(sprintf "%s/%s/to/" event typ)
        post query toEvent workflow
        roles, workflow
    | "//"::xs | "#"::xs ->
        roles, workflow
    | ""::[] ->
        roles, workflow
    | x ->
        printfn "WARNING: '%s' is not parsable" (String.concat " " x)
        roles, workflow

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
        let folder (roles, workflow) line =
            System.Threading.Thread.Sleep(10)
            parse line roles useroles workflow
        File.ReadAllLines(filename) |> List.ofArray |> List.fold folder ([], "test")
    else
        printfn "ERROR: Could not find file"
        promptParseFile fileMap

let Start =

    #if TARGET_MAC
    #else
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
