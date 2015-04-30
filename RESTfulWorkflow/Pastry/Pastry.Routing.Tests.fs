module Pastry.Test

open NUnit.Framework

[<TestFixture>]
type public Test() =

    [<SetUp>]
    member public x.``run before each test``() =
        System.Console.WriteLine("Test start")

    [<TearDown>]
    member public x.``run after each test``() =
        System.Console.WriteLine("Test slut")

    [<Test>]
    member public x.``Test if all states are correct in newlig created event`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), (true, true, true)
        //Act
        let result = create_event name state
        let event = match result with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(event.name, name)
        Assert.AreEqual(event.executed, true)
        Assert.AreEqual(event.pending, true)
        Assert.AreEqual(event.included, true)
        Assert.AreEqual(event.toRelations, Set.empty)
        Assert.AreEqual(event.fromRelations, Set.empty)
        Assert.AreEqual(event.roles, Set.empty)

