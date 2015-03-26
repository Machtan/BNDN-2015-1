module EventTest.``Testeing of the event code``

open NUnit.Framework
open System.IO

let mutable p = new System.Diagnostics.Process()

let testHelpGET eventPath (expected : bool)=
    use w = new System.Net.WebClient ()
    let s = w.DownloadString(sprintf "http://localhost:8080/Test/%s" eventPath)
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
        if File.Exists("Event.exe")
        then File.Delete("Event.exe")

        #if TARGET_MAC
        let src = "../../Event/target/Event.exe"
        let wfsrc = "../../Event/target/Workflow.dll"
        let wfdst = "Workflow.dll"
        if File.Exists("Workflow.dll")
        then File.Delete("Workflow.dll")
        File.Copy(wfsrc, wfdst)
        #else
        let src = @"..\..\..\Event\bin\Debug\Event.exe"
        #endif

        let dst = "Event.exe"
        File.Copy(src, dst)

        //Starts the server form the .exe fil server from same placement as the program.
        p.StartInfo.FileName <- "Event.exe"
        p.StartInfo.Arguments <- ("Test 8080 ")
        let startData = p.Start()
        System.Console.WriteLine("Start: {0}", startData)

        use w = new System.Net.WebClient ()
        w.UploadString("http://localhost:8080/Test/Event1", "POST", "TestClient") |> ignore
        w.UploadString("http://localhost:8080/Test/Event2", "POST", "TestClient") |> ignore
        w.UploadString("http://localhost:8080/Test/Event3", "POST", "TestClient") |> ignore
        w.UploadString("http://localhost:8080/Test/Event4", "POST", "TestClient") |> ignore
        w.UploadString("http://localhost:8080/Test/Event5", "POST", "TestClient") |> ignore

        w.UploadString("http://localhost:8080/Test/Event1/exclusion", "POST", "Event2") |> ignore // [1]->%[2]
        w.UploadString("http://localhost:8080/Test/Event1/condition", "POST", "Event3") |> ignore // [1]->o[3]
        w.UploadString("http://localhost:8080/Test/Event1/response",  "POST", "Event5") |> ignore // [1]o->[5]
        w.UploadString("http://localhost:8080/Test/Event2/condition", "POST", "Event3") |> ignore // [2]->o[3]
        w.UploadString("http://localhost:8080/Test/Event4/include",   "POST", "Event2") |> ignore // [4]->+[2]

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
        p.CloseMainWindow() |> ignore
        System.Threading.Thread.Sleep(100);

    [<Test>]
    member public x.``Test's if a event start with the right settings`` () =
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
    member public x.``Test's if a event can be executed`` () =
        testHelpGET "Event1/executed" false
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event1/executed" true

    [<Test>]
    member public x.``Test's exclusion inhipets execution`` () =
        testHelpGET "Event2/executed" false
        testHelpPUT "Event1/executed" "TestClient"
        testHelpPUT "Event2/executed" "TestClient"
        testHelpGET "Event2/executed" false

    [<Test>]
    member public x.``Test's condition inhipets execution`` () =
        testHelpGET "Event3/executed" false
        testHelpPUT "Event3/executed" "TestClient"
        testHelpGET "Event3/executed" false
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event3/executed" false
        testHelpPUT "Event3/executed" "TestClient"
        testHelpGET "Event3/executed" true

    [<Test>]
    member public x.``Test's response relation set the response value when executed`` () =
        testHelpGET "Event5/pending" false
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event5/pending" true
        testHelpPUT "Event5/executed" "TestClient"
        testHelpGET "Event5/pending" false

    [<Test>]
    member public x.``Test's response relation set the response value even after the receving event have been executed`` () =
        testHelpGET "Event5/pending" false
        testHelpPUT "Event5/executed" "TestClient"
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event5/pending" true
        testHelpPUT "Event5/executed" "TestClient"
        testHelpGET "Event5/pending" false

    [<Test>]
    member public x.``Test's if a event can be pending after it's executed`` () =
        testHelpGET "Event5/pending" false
        testHelpPUT "Event5/executed" "TestClient"
        testHelpGET "Event5/pending" false
        testHelpGET "Event5/executed" true
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event5/pending" true

    [<Test>]
    member public x.``Test's include relation include a event`` () =
        testHelpGET "Event2/included" true
        testHelpPUT "Event1/executed" "TestClient"
        testHelpGET "Event2/included" false
        testHelpPUT "Event4/executed" "TestClient"
        testHelpGET "Event2/included" true

    [<Test>]
    member public x.``Test's include, exclusion and condition together`` () =
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
    member public x.``Test's thad you can't execute a event wihtout the needed role`` () =
        testHelpGET "Event1/executed" false
        testHelpPUT "Event1/executed" "WorngTestClient"
        testHelpGET "Event1/executed" false
