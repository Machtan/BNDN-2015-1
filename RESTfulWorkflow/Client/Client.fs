open System

type eventStatus = string * bool * bool * bool

//String split function, splits a string based on a delimiter
let split (s : string) (delimiter : char) = List.ofArray (s.Split(delimiter))

//Role
let mutable role = "Student"

//BaseUrl
let mutable baseUrl = "http://localhost:8080/Test"

// HTTPREQUEST with a given httpverb that returns an option of None on error and a response string on success
let HTTPRequestUpload (url : string) verb = 
    try
        use w = new System.Net.WebClient () 
        Some(w.UploadString(url,verb, role))
    with
        | ex -> None

// HTTPREQUEST GET returns an option of None on error and a response string on success
let HTTPRequestDownload (url : string) = 
    try
        use w = new System.Net.WebClient () 
        Some(w.DownloadString(url+"?role=" + role))
    with
        | ex -> None
          
//Converts a list of eventnames to a list of the EventStatus type
let rec stringListToEventStatus eventsList =
    match (eventsList) with
    | x::xs -> (x,false,false,false) :: stringListToEventStatus xs
    | xs -> []

//Returns a list of event names with ' ' as delimiter
let GetAllEvents baseUrl =
    let eventsResponse = HTTPRequestDownload baseUrl
    if (eventsResponse.IsNone) then
        None
    else
        Some(stringListToEventStatus(split(eventsResponse.Value) ' '))

//Gets the status of a particular event
let eventStatus baseUrl eventName = 
    let executed = HTTPRequestDownload (baseUrl + "/" + eventName + "/executed")
    let executable = HTTPRequestDownload (baseUrl + "/" + eventName + "/executable")
    let pending = HTTPRequestDownload (baseUrl + "/" + eventName + "/pending")
    if (executed.IsNone || executable.IsNone || pending.IsNone) then
        None
    else
        Some((eventName, Convert.ToBoolean(executed.Value),Convert.ToBoolean(pending.Value),Convert.ToBoolean(executable.Value)) : eventStatus)

//Write out the status of a list of EventStatus
let rec writeOutEventStatus events baseUrl =
    match (events) with
    | (eventName, executed,pended,executeable) :: xs -> 
                    match ((eventName, executed,pended,executeable)) with
                    //| (executed,included,pended) when executed = false && included = false && pended = true -> 
                    | (eventName, executed,pended,executeable) when executed = false && executeable = true && pended = false ->  printfn "%s   EXECUTABLE" eventName
                    | (eventName, executed,pended,executeable) when executed = false && executeable = true && pended = true ->  printfn "!%s   EXECUTABLE" eventName
                    | (eventName, executed,pended,executeable) when executed = true && executeable = false && pended = false ->  printfn "X%s" eventName
                    | (eventName, executed,pended,executeable) when executed = true && executeable = false && pended = true ->   printfn "X%s" eventName
                    | (eventName, executed,pended,executeable) when executed = true && executeable = true && pended = false ->   printfn "X%s" eventName
                    | (eventName, executed,pended,executeable) when executed = true && executeable = true && pended = true ->   printfn "X%s" eventName
                    | _ -> 0 |> ignore
                    (eventName, executed,pended,executeable) :: writeOutEventStatus xs baseUrl
    | _ -> []

//Writes the status of each event to the console and returns a list containing only the events that was responsive.
let rec getStatusAndRemovefaulty events baseUrl = 
    let rec getValidEvents ev =
        match (ev) with
            | (eventName, executed,pended,executeable) :: xs -> 
                        let eventStatusResponse = eventStatus baseUrl eventName
                        if (eventStatusResponse.IsSome) then
                            let (eventName, executed,pended, executeable) = eventStatusResponse.Value
                            (eventName, executed,pended,executeable) :: getValidEvents xs
                        else
                            getValidEvents xs
            | _ -> []
    writeOutEventStatus (List.sortBy (fun k -> let (eventName, executed,pended,executeable) = k
                                               not pended) (getValidEvents events)) baseUrl
//Prompts the user to enter the BaseURL
let rec selectRole n = 
    printfn "Whats your role?"
    printf "-> "
    match(Console.ReadLine()) with
        | "" -> selectRole n
        | v -> v

//Prompts the user to enter the BaseURL
let selectBaseUrl n = 
    printfn "Please provide BaseURL and press enter:"
    Console.ReadLine()

//Mainloop, covers event selection and exchanging information with the events.    
let rec mainLoop (events : eventStatus list) =
    printfn "Actions:"
    printfn "1 -> Get all events"
    printfn "2 -> Execute an event"
    printfn "3 -> Change BaseURL"
    printfn "4 -> Change Role"
    printfn "5 -> View tasks/status"
    printfn "6 -> Exit program"
    printf "-> "
    match(Console.ReadLine()) with
    | "1" -> //Get all events
             printfn "Downloading the list of events..."
             let eventsResponse = GetAllEvents baseUrl
             if (eventsResponse.IsNone) then
                printfn "Program failed to get a list of events. Connection may be at fault."  
             if (eventsResponse.IsSome) then
                printfn "%s downloaded." (eventsResponse.Value.Length.ToString())
                mainLoop eventsResponse.Value
             else                
                mainLoop events
    | "2" -> //Connect an event
             printfn "Please provide event name:"
             let eventName = Console.ReadLine()
             let eventStatusResponse = eventStatus baseUrl eventName
             if (eventStatusResponse.IsNone) then
                    printfn "Program failed to exchange data with the event. Eventname, baseURL or connection may be at fault."
                    mainLoop events
             else
                    let (eventName, executed,pended,executeable) = eventStatusResponse.Value
                    //Write out status:
                    printfn "Event status:"
                    printfn "IsExecuted: %s" (executed.ToString())
                    printfn "IsPending: %s" (pended.ToString())
                    printfn "IsExecuteable: %s" (executeable.ToString())
                    if (executed) then
                        printfn "Event has already been executed, so it cant be executed."                   
                    else
                        let response = HTTPRequestUpload (baseUrl + "/" + eventName + "/executed") "PUT"
                        if (response.IsNone) then
                            printfn "Program failed to issue the command to the event. Connection may be at fault."                             
                        if (response.IsSome) then 
                            response.Value |> printfn "Success! Response: %s" 
                    printfn "action completed"
                    mainLoop events
    | "3" -> baseUrl <- selectBaseUrl 0
             Some(true) //Change BaseURL
    | "4" -> role <- selectRole 0 //Change role
             Some(true)
    | "5" -> //View Status
             printfn "Task:"
             mainLoop (getStatusAndRemovefaulty events baseUrl)
    | "6" -> None //Exit program
    | _   -> None

 

//Provides the loop that allows the mainLoop to exit and rerequest the BaseURL.
let rec outerLoop m = 
    let m = mainLoop []
    if (m.IsSome) then
      outerLoop m
            

//Entry point
[<EntryPoint>]
let rec main argv = 
    printfn "Welcome to the testclient of the workflow client"
    Console.Title <- "Workflow test client"
    role <- selectRole 0 //Change role
    //http://localhost:8080/<process>/<session_id>/<event>/<attribute>
    outerLoop None
    0

        
