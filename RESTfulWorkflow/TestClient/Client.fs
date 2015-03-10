module Client

// --debois, Mar '15

let test r = 
    let port = 8080
    let url = sprintf "http://localhost:8080/resource/%s" r
    use w = new System.Net.WebClient () 

    w.UploadString(url, "CREATE", "value for A") |> printfn "CREATE resource/A --> %s"
    w.DownloadString(url)                        |> printfn "GET /resource/A --> %s" 
    w.UploadString(url, "PUT", "new value")      |> printfn "PUT /resource/A [new value] --> %s"
    w.DownloadString(url)                        |> printfn "GET /resource/A --> %s" 

[<EntryPoint>]
let main _ = 
    printfn "Waiting for server to come up ..."
    System.Threading.Thread.Sleep 2000
    test "A"
    test "B"
    0

