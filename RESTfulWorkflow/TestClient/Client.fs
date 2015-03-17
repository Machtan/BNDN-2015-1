open System
type EventStatus = string * string * string

// HTTPREQUEST GET
let HTTPRequestDownload (url : string) = 
    try
        let port = 8080
        use w = new System.Net.WebClient () 
        Some(w.DownloadString(url))
    with
        | _ -> None
        
// HTTPREQUEST using a provided HTTP verb
let HTTPRequestUpload (url : string) verb value = 
    try
        let port = 8080
        use w = new System.Net.WebClient () 
        Some(w.UploadString(url,verb, value))
    with
        | _ -> None
//Get event status
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
    printfn "1 -> Connect an event"
    printfn "2 -> Change BaseURL"
    printfn "3 -> View status"
    printfn "4 -> Exit program"
    printf "-> "
    let action = Console.ReadLine()
    if (action = "4") then //Exit program
        None    
    else   
        if (action = "2") then //Change BaseURL
            Some(true)
        else
            if (action = "3") then //View Status
                    printfn "Status:"
                    mainLoop baseUrl (getStatusAndRemovefaulty events baseUrl)
            else
                if (action = "1") then
                    //Connect an event
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
                        if (action = "1") then
                            response <- HTTPRequestUpload (baseUrl + "/" + eventName + "/executed") "PUT" "true"
                        else
                            if (action = "2") then
                                response <-  HTTPRequestUpload (baseUrl + "/" + eventName + "/included") "PUT" "true"
                            else
                                if (action = "3") then
                                    response <-  HTTPRequestUpload (baseUrl + "/" + eventName + "/pending") "PUT" "true"
                                else   
                                   if (response.IsNone) then
                                       printfn "Program failed to issue the command to the event. Connection may be at fault."    
                        if (response.IsSome) then 
                            response.Value |> printfn "Success! Response: %s" 
                        printfn "action completed"
                        mainLoop baseUrl events
                else
                    None

//Returns an url with 2 segments only if possible.
let GetWorkFlowUrl url = 
    if (not (Uri.IsWellFormedUriString(url, UriKind.Absolute))) then
        None
    else
        match ((new Uri(url))) with
        | x when x.Segments.Length > 1 -> Some("http://"+ x.Host + "/" + (string (x.Segments.GetValue(1))))
        | _ -> None

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

        
