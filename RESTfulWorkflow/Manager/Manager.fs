
let test r = 
    let port = 8080
    let url = sprintf "http://localhost:8080/resource/%s" r
    use w = new System.Net.WebClient () 

    w.UploadString(url, "CREATE", "value for A") |> printfn "CREATE resource/A --> %s"
    w.DownloadString(url)                        |> printfn "GET /resource/A --> %s" 
    w.UploadString(url, "PUT", "new value")      |> printfn "PUT /resource/A [new value] --> %s"
    w.DownloadString(url)                        |> printfn "GET /resource/A --> %s"

[<EntryPoint>]
let main argv =
    test "A"
    test "B"
    0
