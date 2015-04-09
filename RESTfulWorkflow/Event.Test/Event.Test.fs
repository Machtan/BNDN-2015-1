module EventTest.``Testeing of the event code``

open NUnit.Framework
open System.IO

let mutable p = new System.Diagnostics.Process()

let download eventPath =
    use w = new System.Net.WebClient ()
    w.DownloadString(sprintf "http://localhost:8080/%s" eventPath)

let testHelpGET eventPath expected =
    let s = download (sprintf "Test/%s" eventPath)
    System.Console.WriteLine("/{0} --> {1}, Expected: {2}", eventPath, s, expected.ToString())
    Assert.AreEqual(expected.ToString(),s)

let testHelpPUT eventPath role =
    use w = new System.Net.WebClient ()
    try
        ignore <| w.UploadString(sprintf "http://localhost:8080/Test/%s" eventPath, "PUT" , role)
    with
        | x ->  System.Console.WriteLine("Exeption: {0}", x.Message)
                0 |> ignore

[<TestFixture>]
type public Test() =

    [<SetUp>]
    member public x.``run before each test``() =
        #if TARGET_MAC
        #else
        if File.Exists("Event.exe")
        then File.Delete("Event.exe")

        let src = @"..\..\..\Event\bin\Debug\Event.exe"
        let dst = "Event.exe"
        File.Copy(src, dst)

        //Starts the server form the .exe fil server from same placement as the program.
        p.StartInfo.FileName <- "Event.exe"
        p.StartInfo.Arguments <- ("Test 8080 ")
        p.Start() |> ignore
        #endif

        //System.Console.WriteLine("Start: {0}", startData)
        //System.Console.WriteLine("P: {0} HasExited {1}", p, p.HasExited)

        use w = new System.Net.WebClient ()

        // RESET!
        let testUpload (client: System.Net.WebClient) (url: string) (meth: string) (data: string) =
            System.Threading.Thread.Sleep(15);
            client.UploadString(url, meth, data) |> ignore

        testUpload w "http://localhost:8080/Test?action=reset" "PUT" "TestClient"

        testUpload w "http://localhost:8080/Test/Event1" "POST" "000 TestClient"
        testUpload w "http://localhost:8080/Test/Event2" "POST" "000 TestClient"
        testUpload w "http://localhost:8080/Test/Event3" "POST" "000 TestClient"
        testUpload w "http://localhost:8080/Test/Event4" "POST" "000 TestClient"
        testUpload w "http://localhost:8080/Test/Event5" "POST" "000 TestClient"

        testUpload w "http://localhost:8080/Test/Event1/exclusion" "POST" "Event2" // [1]->%[2]
        testUpload w "http://localhost:8080/Test/Event1/condition" "POST" "Event3" // [1]->o[3]
        testUpload w "http://localhost:8080/Test/Event1/response"  "POST" "Event5" // [1]o->[5]
        testUpload w "http://localhost:8080/Test/Event2/condition" "POST" "Event3" // [2]->o[3]
        testUpload w "http://localhost:8080/Test/Event4/inclusion" "POST" "Event2" // [4]->+[2]

        // [1]->%[2], [1]->o[3], [1]o->[5]
        // [2]->o[3]
        // [4]->+[2]
        //
        //  [2]-----[3]
        //   | \   /
        //   |  [1]
        //   |     \
        //  [4]     [5]

    [<TearDown>]
    member public x.``run after each test``() =
        #if TARGET_MAC
        #else
        p.Kill() |> ignore
        #endif
        System.Threading.Thread.Sleep(15);

    [<Test>]
    member public x.``Events start with the right settings`` () =
        testHelpGET "Event1/executed" false
        testHelpGET "Event1/included" true
        testHelpGET "Event1/pending" false
        testHelpGET "Event2/executed" false
        testHelpGET "Event2/included" true
        testHelpGET "Event2/pending" false
        testHelpGET "Event3/executed" false
        testHelpGET "Event3/included" true
        testHelpGET "Event3/pending" false
        testHelpGET "Event4/executed" false
        testHelpGET "Event4/included" true
        testHelpGET "Event4/pending" false
        testHelpGET "Event5/executed" false
        testHelpGET "Event5/included" true
        testHelpGET "Event5/pending" false

    [<Test>]
    member public x.``An event can be executed`` () =
        testHelpGET "Event1/executed" false
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event1/executed" true

    [<Test>]
    member public x.``Exclusion inhibits execution`` () =
        testHelpGET "Event2/executed" false
        testHelpPUT "Event1/executed" "TestClient"
        testHelpPUT "Event2/executed" "TestClient"
        testHelpGET "Event2/executed" false

    [<Test>]
    member public x.``Condition inhibits execution?`` () =
        testHelpGET "Event3/executed" false
        testHelpPUT "Event3/executed" "TestClient"
        testHelpGET "Event3/executed" false
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event3/executed" false
        testHelpPUT "Event3/executed" "TestClient"
        testHelpGET "Event3/executed" true

    [<Test>]
    member public x.``A response relation is set to the response value when an event is executed`` () =
        testHelpGET "Event5/pending" false
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event5/pending" true
        testHelpPUT "Event5/executed" "TestClient"
        testHelpGET "Event5/pending" false

    [<Test>]
    member public x.``A response relation is no more set to the response value even after the receving event has been executed`` () =
        testHelpGET "Event5/pending" false
        testHelpPUT "Event5/executed" "TestClient"
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event5/pending" true
        testHelpPUT "Event5/executed" "TestClient"
        testHelpGET "Event5/pending" false

    [<Test>]
    member public x.``An event cannot be pending after it has been executed`` () =
        testHelpGET "Event5/pending" false
        testHelpPUT "Event5/executed" "TestClient"
        testHelpGET "Event5/pending" false
        testHelpGET "Event5/executed" true
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event5/pending" true

    [<Test>]
    member public x.``An include relation includes the event`` () =
        testHelpGET "Event2/included" true
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event2/included" false
        testHelpPUT "Event4/executed" "TestClient"
        testHelpGET "Event2/included" true

    [<Test>]
    member public x.``Inclusion, exclusion and conditions work at the same time`` () =
        testHelpGET "Event3/executed" false
        testHelpGET "Event2/included" true
        testHelpPUT "Event3/executed" "TestClient"
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event3/executed" false
        testHelpGET "Event2/included" false
        testHelpPUT "Event4/executed" "TestClient"
        testHelpPUT "Event3/executed" "TestClient"
        testHelpGET "Event3/executed" false
        testHelpGET "Event2/included" true
        testHelpPUT "Event2/executed" "TestClient"
        testHelpPUT "Event3/executed" "TestClient"
        testHelpGET "Event3/executed" true
        testHelpGET "Event2/executed" true

    [<Test>]
    member public x.``An event cannot be executed without the correct role`` () =
        testHelpGET "Event1/executed" false
        testHelpPUT "Event1/executed" "WrongTestClient"
        testHelpGET "Event1/executed" false

    [<Test>]
    member public x.``Get with empty role doesn't fail`` () =
        download "Test?role=" |> ignore

    [<Test>]
    member public x.``Get with empty action doesn't fail`` () =
        download "Test?action=" |> ignore

    [<Test>]
    member public x.``Reset with role does not return `` () =
        let resp = download "Test?role=TEST&action=reset"
        Assert.AreEqual(resp, "Resetting...")

