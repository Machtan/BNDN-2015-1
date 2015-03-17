open System

// --debois, Mar '15
let HTTPRequestDownload (url : string) = 
    let port = 8080
    use w = new System.Net.WebClient () 
    w.DownloadString(url)

let HTTPRequestUpload (url : string) verb value = 
    let port = 8080
    use w = new System.Net.WebClient () 
    w.UploadString(url,verb, value)

let rec mainLoop baseUrl =
    printfn "Please provide event name:"
    
    //http://localhost:8080/<process>/<session_id>/<event>/<attribute>
    let eventName = Console.ReadLine()
    let executed = HTTPRequestDownload (baseUrl + "/" + eventName + "/executed")
    let included = HTTPRequestDownload (baseUrl + "/" + eventName + "/included")
    let pending = HTTPRequestDownload (baseUrl + "/" + eventName + "/pending")

    //Write out status:
    printfn "Event status:"
    printfn "IsExecuted: %s" executed 
    printfn "IsIncluded: %s" included
    printfn "IsPending: %s" pending
    printfn "Actions:"
    printfn "1 -> Execute"
    printfn "2 -> Include"
    printfn "3 -> Pending"
    printfn "4 -> Lock"
    printfn "5 -> Exit event"
    printfn "6 -> Exit program"
    let action = Console.ReadLine()
    if (action = "1") then
        HTTPRequestUpload (baseUrl + "/" + eventName + "/executed") "PUT" "true" |> printfn "Result: %s"
    if (action = "2") then
        HTTPRequestUpload (baseUrl + "/" + eventName + "/included") "PUT" "true" |> printfn "Result: %s"
    if (action = "3") then
        HTTPRequestUpload (baseUrl + "/" + eventName + "/pending") "PUT" "true" |> printfn "Result: %s"
    if (action = "4") then
        HTTPRequestUpload (baseUrl + "/" + eventName + "/locked") "PUT" "true" |> printfn "Result: %s"         
    if not (action = "6") then
        mainLoop baseUrl     

let GetWorkFlowUrl url = 
    if (not (Uri.IsWellFormedUriString(url, UriKind.Absolute))) then
        None
    else
        match ((new Uri(url))) with
        | x when x.Segments.Length > 1 -> Some("http://"+ x.Host + "/" + (string (x.Segments.GetValue(1))))
        | _ -> None

let rec SelectBaseUrl n = 
    printfn "Please provide by url and press enter:"
    let baseUrl = GetWorkFlowUrl (Console.ReadLine())
    if (baseUrl.IsSome) then
        baseUrl.Value
    else
        printfn "Entered url was invalid, please try again."
        SelectBaseUrl n

[<EntryPoint>]
let main argv = 
    Console.Title <- "Workflow test client"
    let baseUrl = SelectBaseUrl 0
    mainLoop baseUrl
    0

        
