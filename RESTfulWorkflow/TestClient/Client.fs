open System

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
        
//Mainloop, covers event selection and exchanging information with the events.    
let rec mainLoop baseUrl =
    printfn "Actions:"
    printfn "1 -> Connect an event"
    printfn "2 -> Change BaseURL"
    printfn "3 -> Exit program"
    printf "-> "
    let action = Console.ReadLine()
    if (action = "3") then
        None    
    else   
        if (action = "2") then
            Some(true)
        else
            printfn "Please provide event name:"
            let eventName = Console.ReadLine()
            let executed = HTTPRequestDownload (baseUrl + "/" + eventName + "/executed")
            let included = HTTPRequestDownload (baseUrl + "/" + eventName + "/included")
            let pending = HTTPRequestDownload (baseUrl + "/" + eventName + "/pending")
            if (executed.IsNone || included.IsNone || pending.IsNone) then
                printfn "Program failed to exchange data with the event. Eventname, baseURL or connection may be at fault."
                mainLoop baseUrl
            else
                //Write out status:
                printfn "Event status:"
                printfn "IsExecuted: %s" executed.Value 
                printfn "IsIncluded: %s" included.Value 
                printfn "IsPending: %s" pending.Value 
                printfn "Actions:"
                printfn "1 -> Execute"
                printfn "2 -> Include"
                printfn "3 -> Pending"
                printfn "4 -> Lock"
                printfn "5 -> Exit event"
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
                            if (action = "4") then
                                response <-  HTTPRequestUpload (baseUrl + "/" + eventName + "/locked") "PUT" "true"       
                            else
                                if (response.IsNone) then
                                    printfn "Program failed to issue the command to the event. Connection may be at fault."    
                if (response.IsSome) then 
                    response.Value |> printfn "Success! Response: %s" 
                printfn "action completed"
                mainLoop baseUrl 
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
    let m = mainLoop baseUrl
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

        
