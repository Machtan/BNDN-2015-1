open System

type EventStatus = string * string * string

//String split function, splits a string based on a delimiter
let Split (s : string) (delimiter : char) = List.ofArray (s.Split(delimiter))

// HTTPREQUEST GET returns an option of None on error and a response string on success
let HTTPRequestDownload (url : string) = 
    try
        use w = new System.Net.WebClient () 
        Some(w.DownloadString(url))
    with
        | _ -> None
        
// HTTPREQUEST with a given httpverb that returns an option of None on error and a response string on success
let HTTPRequestUpload (url : string) verb value = 
    try
        use w = new System.Net.WebClient () 
        Some(w.UploadString(url,verb, value))
    with
        | _ -> None

//Returns a list of event names with ' ' as delimiter
let GetAllEvents baseUrl =
    let eventsResponse = HTTPRequestDownload (baseUrl)
    if (eventsResponse.IsNone) then
        None
    else
        Some(Split(eventsResponse.Value) ' ')
    
//Gets the status of a particular event
let eventStatus baseUrl eventName = 
    let executed = HTTPRequestDownload (baseUrl + "/" + eventName + "/executed")
    let included = HTTPRequestDownload (baseUrl + "/" + eventName + "/included")
    let pending = HTTPRequestDownload (baseUrl + "/" + eventName + "/pending")
    if (executed.IsNone || included.IsNone || pending.IsNone) then
        None
    else
        Some((executed.Value,included.Value,pending.Value) : EventStatus)

//Writes the status of each event to the console and returns a list containing only the events that was responsive.
let rec getStatusAndRemovefaulty events baseUrl = 
    match (events) with
    | x :: xs -> 
                let eventStatusResponse = eventStatus baseUrl x
                if (eventStatusResponse.IsSome) then
                    let (executed,included,pended) = eventStatusResponse.Value
                    printfn "EventName: %s" x
                    printfn "IsExecuted: %s" executed
                    printfn "IsIncluded: %s" included
                    printfn "IsPending: %s" pended
                    x :: getStatusAndRemovefaulty xs baseUrl
                else
                    getStatusAndRemovefaulty xs baseUrl
    | _ -> events

//Mainloop, covers event selection and exchanging information with the events.    
let rec mainLoop baseUrl events =
    printfn "Actions:"
    printfn "1 -> Get all events"
    printfn "2 -> Connect an event"
    printfn "3 -> Change BaseURL"
    printfn "4 -> View status"
    printfn "5 -> Exit program"
    printf "-> "
    match(Console.ReadLine()) with
    | "1" -> //Get all events
             printfn "Downloading the list of events..."
             let eventsResponse = GetAllEvents baseUrl
             if (eventsResponse.IsNone) then
                printfn "Program failed to get a list of events. Connection may be at fault."   
             printfn "%s downloaded." (eventsResponse.Value.Length.ToString())
             mainLoop baseUrl (getStatusAndRemovefaulty eventsResponse.Value baseUrl)
    | "2" -> //Connect an event
             printfn "Please provide event name:"
             let eventName = Console.ReadLine()
             let eventStatusResponse = eventStatus baseUrl eventName
             if (eventStatusResponse.IsNone) then
                    printfn "Program failed to exchange data with the event. Eventname, baseURL or connection may be at fault."
                    mainLoop baseUrl events
             else
                    let (executed,included,pended) = eventStatusResponse.Value
                    //Write out status:
                    printfn "Event status:"
                    printfn "IsExecuted: %s" executed
                    printfn "IsIncluded: %s" included
                    printfn "IsPending: %s" pended
                    printfn "Actions:"
                    printfn "1 -> Execute"
                    printfn "2 -> Include"
                    printfn "3 -> Pending"
                    printfn "4 -> Exit event"
                    printf "-> "
                    let action = Console.ReadLine()
                    printfn "Executing action"
                    let mutable response = None
                    match (action) with
                    | "1" ->  response <- HTTPRequestUpload (baseUrl + "/" + eventName + "/executed") "PUT" "true"
                    | "2" ->  response <- HTTPRequestUpload (baseUrl + "/" + eventName + "/executed") "PUT" "true"
                    | "3" ->  response <- HTTPRequestUpload (baseUrl + "/" + eventName + "/executed") "PUT" "true"
                    | _   ->  response <- HTTPRequestUpload (baseUrl + "/" + eventName + "/executed") "PUT" "true"
                    if (response.IsNone) then
                        printfn "Program failed to issue the command to the event. Connection may be at fault."                             
                    if (response.IsSome) then 
                        response.Value |> printfn "Success! Response: %s" 
                    printfn "action completed"
                    mainLoop baseUrl events
    | "3" -> Some(true) //Change BaseURL
    | "4" -> //View Status
             printfn "Status:"
             mainLoop baseUrl (getStatusAndRemovefaulty events baseUrl)
    | "5" -> None //Exit program
    | _   -> None

//Returns an url with 2 segments only if possible.
let GetWorkFlowUrl url = Some(url)

//Prompts the user to enter the BaseURL
let rec SelectBaseUrl n = 
    printfn "Please provide BaseURL and press enter:"
    let baseUrl = GetWorkFlowUrl (Console.ReadLine())
    if (baseUrl.IsSome) then
        baseUrl.Value
    else
        printfn "Entered url was invalid, please try again."
        SelectBaseUrl n

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

        
