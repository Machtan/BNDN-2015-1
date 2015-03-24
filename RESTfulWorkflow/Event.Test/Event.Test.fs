module EventTest.``Testeing of the event code``

open NUnit.Framework
open System.IO

let mutable p = new System.Diagnostics.Process()

let testHelpGET eventPath =
    use w = new System.Net.WebClient ()
    let s = w.DownloadString(sprintf "http://localhost:8080/Test/%s" eventPath)
    System.Console.WriteLine("http://localhost:8080/Test/{0} --> {1}", eventPath, s)
    s

let testHelpPUT eventPath role =
    use w = new System.Net.WebClient ()
    try
        ignore <| w.UploadString(sprintf "http://localhost:8080/Test/%s" eventPath, "PUT" , role)
    with
            | x -> 0 |> ignore

[<TestFixture>]
type public Test() =

    [<SetUp>]
    member public x.``run before test``() =
        if File.Exists("event.exe")
        then File.Delete("event.exe")
        File.Copy(@"..\..\..\Event\bin\Debug\Event.exe",@"Event.exe")

        //Starts the server form the .exe fil server from same placement as the program.
        p.StartInfo.FileName <- "event.exe"
        p.StartInfo.Arguments <- ("Test 8080 ")
        p.Start() |> ignore

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
    member public x.``run after test``() =
        p.CloseMainWindow() |> ignore

    [<Test>]
    member public x.``Test's if a event can be executed`` () =
        // Arrange
        use w = new System.Net.WebClient ()

        // Act
        let result1 = testHelpGET "Event1/executed"
        testHelpPUT "Event1/executed" "TestClient"
        let result2 = testHelpGET "Event1/executed"

        // Assert
        Assert.AreEqual("False",result1)
        Assert.AreEqual("True",result2)

    [<Test>]
    member public x.``Test's exclusion inhipets execution`` () =
        // Arrange
        use w = new System.Net.WebClient ()

        // Act
        let result1 = testHelpGET "Event2/executed"
        testHelpPUT "Event1/executed" "TestClient"
        testHelpPUT "Event2/executed" "TestClient"
        let result2 = testHelpGET "Event2/executed"

        // Assert
        Assert.AreEqual("False",result1)
        Assert.AreEqual("False",result2)

    [<Test>]
    member public x.``Test's condition inhipets execution`` () =
        // Arrange
        use w = new System.Net.WebClient ()

        // Act
        let result1 = testHelpGET "Event3/executed"
        testHelpPUT "Event3/executed" "TestClient"
        let result2 = testHelpGET "Event3/executed"
        testHelpPUT "Event1/executed" "TestClient"
        let result3 = testHelpGET "Event3/executed"
        testHelpPUT "Event3/executed" "TestClient"
        let result4 = testHelpGET "Event3/executed"
        

        // Assert
        Assert.AreEqual("False",result1)
        Assert.AreEqual("False",result2)
        Assert.AreEqual("False",result3)
        Assert.AreEqual("True",result4)