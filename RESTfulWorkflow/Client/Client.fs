open System

type eventStatus = string * bool * bool * bool

//String split function, splits a string based on a delimiter
let split (s : string) (delimiter : char) = List.ofArray (s.Split(delimiter))

//Role
let role = "Client"

// HTTPREQUEST with a given httpverb that returns an option of None on error and a response string on success
let HTTPRequestUpload (url : string) verb = 
    try
        use w = new System.Net.WebClient () 
        Some(w.UploadString(url+"?role=" + role,verb, "true"))
    with
        | _ -> None

// HTTPREQUEST GET returns an option of None on error and a response string on success
let HTTPRequestDownload (url : string) = 
    try
        use w = new System.Net.WebClient () 
        Some(w.DownloadString(url+"?role="+role))
    with
        | _ -> None
          
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
    let included = HTTPRequestDownload (baseUrl + "/" + eventName + "/included")
    let pending = HTTPRequestDownload (baseUrl + "/" + eventName + "/pending")
    if (executed.IsNone || included.IsNone || pending.IsNone) then
        None
    else
        Some((eventName, Convert.ToBoolean(executed.Value),Convert.ToBoolean(included.Value),Convert.ToBoolean(pending.Value)) : eventStatus)

//Write out the status of a list of EventStatus
let rec writeOutEventStatus events baseUrl =
    match (events) with
    | (eventName, executed,included,pended) :: xs -> 
                    match ((eventName, executed,included,pended)) with
                    //| (executed,included,pended) when executed = false && included = false && pended = true -> 
                    | (eventName, executed,included,pended) when executed = false && included = true && pended = false ->  printfn "%s   EXECUTABLE" eventName
                    | (eventName, executed,included,pended) when executed = false && included = true && pended = true ->  printfn "!%s   EXECUTABLE" eventName
                    | (eventName, executed,included,pended) when executed = true && included = false && pended = false ->  printfn "X%s" eventName
                    | (eventName, executed,included,pended) when executed = true && included = false && pended = true ->   printfn "X%s" eventName
                    | (eventName, executed,included,pended) when executed = true && included = true && pended = false ->   printfn "X%s" eventName
                    | (eventName, executed,included,pended) when executed = true && included = true && pended = true ->   printfn "X%s" eventName
                    | _ -> 0 |> ignore
                    (eventName, executed,included,pended) :: writeOutEventStatus xs baseUrl
    | _ -> []

//Writes the status of each event to the console and returns a list containing only the events that was responsive.
let rec getStatusAndRemovefaulty events baseUrl = 
    let rec getValidEvents ev =
        match (ev) with
            | (eventName, executed,included,pended) :: xs -> 
                        let eventStatusResponse = eventStatus baseUrl eventName
                        if (eventStatusResponse.IsSome) then
                            let (eventName, executed,included,pended) = eventStatusResponse.Value
                            (eventName, executed,included,pended) :: getValidEvents xs
                        else
                            getValidEvents xs
            | _ -> []
    writeOutEventStatus (List.sortBy (fun k -> let (eventName, executed,included,pended) = k
                                               not pended) (getValidEvents events)) baseUrl

//Mainloop, covers event selection and exchanging information with the events.    
let rec mainLoop baseUrl (events : eventStatus list) =
    printfn "Actions:"
    printfn "1 -> Get all events"
    printfn "2 -> Execute an event"
    printfn "3 -> Change BaseURL"
    printfn "4 -> View tasks/status"
    printfn "5 -> Exit program"
    printf "-> "
    match(Console.ReadLine()) with
    | "1" -> //Get all events
             printfn "Downloading the list of events..."
             let eventsResponse = GetAllEvents baseUrl
             if (eventsResponse.IsNone) then
                printfn "Program failed to get a list of events. Connection may be at fault."  
             if (eventsResponse.IsSome) then
                printfn "%s downloaded." (eventsResponse.Value.Length.ToString())
                mainLoop baseUrl eventsResponse.Value
             else                
                mainLoop baseUrl events
    | "2" -> //Connect an event
             printfn "Please provide event name:"
             let eventName = Console.ReadLine()
             let eventStatusResponse = eventStatus baseUrl eventName
             if (eventStatusResponse.IsNone) then
                    printfn "Program failed to exchange data with the event. Eventname, baseURL or connection may be at fault."
                    mainLoop baseUrl events
             else
                    let (eventName, executed,included,pended) = eventStatusResponse.Value
                    //Write out status:
                    printfn "Event status:"
                    printfn "IsExecuted: %s" (executed.ToString())
                    printfn "IsIncluded: %s" (included.ToString())
                    printfn "IsPending: %s" (pended.ToString())
                    if (executed) then
                        printfn "Event has already been executed, so it cant be executed."                   
                    else
                        let response = HTTPRequestUpload (baseUrl + "/" + eventName + "/executed") "PUT"
                        if (response.IsNone) then
                            printfn "Program failed to issue the command to the event. Connection may be at fault."                             
                        if (response.IsSome) then 
                            response.Value |> printfn "Success! Response: %s" 
                    printfn "action completed"
                    mainLoop baseUrl events
    | "3" -> Some(true) //Change BaseURL
    | "4" -> //View Status
             printfn "Task:"
             mainLoop baseUrl (getStatusAndRemovefaulty events baseUrl)
    | "5" -> None //Exit program
    | _   -> None
    
//Prompts the user to enter the BaseURL
let SelectBaseUrl n = 
    printfn "Please provide BaseURL and press enter:"
    Console.ReadLine()

//Provides the loop that allows the mainLoop to exit and rerequest the BaseURL.
let rec OuterLoop m = 
    let baseUrl = SelectBaseUrl 0
    let m = mainLoop baseUrl []
    if (m.IsSome) then
        OuterLoop m

//Entry point
[<EntryPoint>]
let rec main argv = 
    printfn "Welcome to the testclient of the workflow client"
    Console.Title <- "Workflow test client"
    //http://localhost:8080/<process>/<session_id>/<event>/<attribute>
    OuterLoop None
    0

        
