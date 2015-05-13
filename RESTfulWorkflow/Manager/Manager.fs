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
    then
        printfn "WARNING: The role (%s) already exists" name
        roles
    else
        name::roles

type Query =
| Workflow of string * string
| User of string


//POST new events and relationships to the server
let upload (query : Query) (meth: string) (data: string) =
    use w = new System.Net.WebClient ()
    let query_url =
        match query with
        | Workflow(wf, path) ->
            if path = "" then get_wf wf
            else sprintf "%s/%s" (get_wf wf) path
        | User(path) -> sprintf "%s/%s" user_url path
    try
        if not (data = "")
        then w.UploadString(query_url, meth, data) |> printfn "%s /%s [%s] \n\t--> %s" meth query_url data
        else w.UploadString(query_url, meth)       |> printfn "%s /%s \n\t--> %s" meth query_url
    with
        | x ->
            printfn "%s %s [%s] --> \n%s" meth query_url data x.Message
            printfn "ERROR: The workflow can not be completed. continue?"
            System.Console.ReadKey() |> ignore

//Parses a string into a specific post to the server.
let parse (line : string) (roles: string list) (use_roles: bool)
        (workflow: string) : string list * string=
    //printfn "Workflow: %s | Roles: %A" workflow roles
    let words = List.ofArray(line.Split ' ')
    System.Threading.Thread.Sleep(10)
    match words with
    | "wor"::name::[] -> // Workflow
        printfn ">> Setting workflow name to '%s'" name
        upload (Workflow(name, "")) "POST" ""
        [], name

    | "rol"::name::[] -> // Role
        (addRole name roles), workflow

    | "use"::name::user_roles -> // User
        let create_query = User(name)
        upload create_query "POST" ""
        System.Threading.Thread.Sleep(10)

        let data = String.concat "," user_roles
        let query = User(sprintf "%s/roles/%s" name workflow)
        upload query "PUT" data
        roles, workflow

    | "eve"::flags::name::eventroles -> // Event
        let query = Workflow(workflow, name)
        if use_roles then
            let role_found event_role =
                List.exists (fun role -> role = event_role) roles
            let all_roles_found = List.forall role_found eventroles
            if all_roles_found then
                let data = sprintf "%s,%s" flags (String.concat "," eventroles)
                upload query "POST" data
            else
                failwith "ERROR: All roles of the event were not found!"
        else
            upload query "POST" flags

        let add_query = Workflow(workflow, "")
        System.Threading.Thread.Sleep(10)
        upload add_query "PUT" name

        roles, workflow

    | "rel"::event::typ::to_event::[] -> // Relation
        let query = Workflow(workflow, sprintf "%s/%s/to" event typ)
        let data = sprintf "%s,%s" workflow to_event
        upload query "PUT" data
        System.Threading.Thread.Sleep(10)

        let from_query = Workflow(workflow, sprintf "%s/%s/from" to_event typ)
        let from_data = sprintf "%s,%s" workflow event
        upload from_query "PUT" from_data
        roles, workflow

    | "//"::xs | "#"::xs -> // Comment
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
    let use_roles = System.Console.ReadLine()
    let use_roles =
        match use_roles with
        | "y"           -> true
        | "n"           -> false
        | _             -> printfn "Defaults to yes" ; true

    if File.Exists(filename)
    then
        let folder (roles, workflow) line =
            parse line roles use_roles workflow
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
