open System
open System.Net
open System.Text
open System.IO
 
let siteRoot = "~/code/fsharp"
let host = "http://localhost:8080/"
 
let listener (handler:(HttpListenerRequest->HttpListenerResponse->Async<unit>)) =
    let hl = new HttpListener()
    hl.Prefixes.Add host
    hl.Start()
    let task = Async.FromBeginEnd(hl.BeginGetContext, hl.EndGetContext)
    async {
        printfn "Listening..."
        while true do
            let! context = task
            Async.Start(handler context.Request context.Response)
    } |> Async.RunSynchronously
 
let output (req:HttpListenerRequest) =
    let file = Path.Combine(siteRoot,
                            Uri(host).MakeRelativeUri(req.Url).OriginalString)
    printfn "Requested : '%s'" file
    if (File.Exists file)
    then File.ReadAllText(file)
    else "File does not exist!"

[<EntryPoint>]
let main args =
    printfn "Starting..."
    listener (fun req resp ->
        async {
            let txt = Encoding.ASCII.GetBytes(output req)
            resp.ContentType <- "text/html"
            resp.OutputStream.Write(txt, 0, txt.Length)
            printfn "Sleeping at a request.... ( %s )" (string txt)
            do! Async.Sleep 10000 // Sleep for 10 seconds
            resp.OutputStream.Close()
            printfn "Finished sleeping! ( %s )" (string txt)
        }) //|> Async.RunSynchronously
    0