module PastryHTTP

// Source: http://www.fssnip.net/br
open System
open System.Net
open System.Text

let listener (handler:(HttpListenerRequest -> HttpListenerResponse -> Async<unit>)) =
    let hl = new HttpListener()
    hl.Prefixes.Add "http://*:8080/"
    hl.Start()
    let task = Async.FromBeginEnd(hl.BeginGetContext, hl.EndGetContext)
    async {
        while true do
            let! context = task
            Async.Start(handler context.Request context.Response)
    } |> Async.Start

let reply (response: HttpListenerResponse) answer status reason =
    printfn " --> %d %s [%s]" status reason answer
    // Set HTTP statuscode and reason (e.g., 200 OK)
    response.StatusCode <- status
    response.StatusDescription <- reason
    // Encode and write body.
    let buffer = System.Text.Encoding.UTF8.GetBytes(answer : string)
    response.ContentLength64 <- int64 buffer.Length;
    response.OutputStream.Write(buffer,0,buffer.Length);
    response.OutputStream.Close();

listener (fun request response ->
    async {
        let meth     = request.HttpMethod
        let args     = request.QueryString
        let path     = request.Url.AbsolutePath
        let rip      = request.RemoteEndPoint.Address
        let rport    = request.RemoteEndPoint.Port
        let body =
            use is = new System.IO.StreamReader(request.InputStream, request.ContentEncoding)
            is.ReadToEnd()

        reply response (sprintf "I got this:\n\n%s" path) 200 "Ok"
        printfn "Response handled, sleeping..."
        do! Async.Sleep 10000000 // Sleep for a while
    })

printfn "Press return to exit..."
Console.ReadLine()
    |> ignore