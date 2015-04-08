module EasyManager

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
    then printfn "ERROR: The role (%s) do alderady exist" name; roles
    else name::roles

//POST new events and relationships to the server
let post (url : string) data =
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

//Parses a string into a specific post to the server.
let parse (line : string) roles useroles =
    let words = List.ofArray(line.Split ' ')
    match words with
    | "rol"::name::[] -> addRole name roles;
    | "eve"::name::eventroles ->
        if useroles
        then
            let rec inner x : bool =
                match x with
                | role::xs  ->
                    if List.exists (fun x -> x = role) roles
                    then
                        inner xs
                    else
                        printfn "ERROR: \"%s\" Is not a role" role
                        false
                | _  ->
                    true
            if inner eventroles
            then post (sprintf " %s/%s" url name) (String.concat " " eventroles)
        else post (sprintf " %s/%s" url name) ""
        roles
    | "rel"::event::typ::toEvent::[] ->
        post (sprintf " %s/%s/%s" url event typ) toEvent ; roles
    | "//"::xs | "#"::xs ->
        roles
    | x ->
        printfn "ERROR: \"%s\" Is not parseble" (String.concat " " x)
        roles

//Parse all lines in selected file or written file.
let promptParseFile x =
    let filename = "hospitaldcr.txt"

    if File.Exists(filename)
    then
        let folder roles line =
            System.Threading.Thread.Sleep(10)
            parse line roles true
        File.ReadAllLines(filename) |> List.ofArray |> List.fold folder [] |> ignore
    else
        printfn "ERROR: Could not find file"

let easyStart =
    //Starts the server form the Event.exe fil server from same placement as the program.
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- "Event.exe"
    p.StartInfo.Arguments <- (serverName + " " + port)
    p.Start() |> ignore

    promptParseFile 0 |> ignore

    printfn "All lines in the file have been iterated."