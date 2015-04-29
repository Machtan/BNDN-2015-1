module EventTest.``Testing of the eventLogik code``

open NUnit.Framework
open System.IO
open EventLogic      // To be tested in this Unit test

[<TestFixture>]
type public Test() =

    [<SetUp>]
    member public x.``run before each test``() =
        System.Console.WriteLine("Test start")

    [<TearDown>]
    member public x.``run after each test``() =
        System.Console.WriteLine("Test slut")

//let create_event (name : EventName) (state : EventState) : ResultEvent =
    
    [<Test>]
    member public x.``Test if all states are correct in newlig created event`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), (true, true, true)
        //Act
        let event = create_event name state
        //Assert
        Assert.AreEqual(event.name, name)
        Assert.AreEqual(event.executed, true)
        Assert.AreEqual(event.pending, true)
        Assert.AreEqual(event.included, true)
        Assert.AreEqual(event.toRelations, Set.empty)
        Assert.AreEqual(event.fromRelations, Set.empty)
        Assert.AreEqual(event.roles, Set.empty)


//let get_event_state (event : Event) : (string*EventState) =
//let check_roles (event : Event) (roles : Roles) : bool =
//let execute (event : Event) (userName : UserName) : ResultEvent =
//let add_event_roles (event : Event) (roles : Roles) : ResultEvent =
//let add_relation_to (event : Event) (relations : RelationType) (eventName : EventName) : ResultEvent =
//let add_relation_from (event : Event) (relations : RelationType) (eventName : EventName) : ResultEvent =
//let remove_relation_to (event : Event) (relation : RelationType) (eventName : EventName) : ResultEvent =
//let remove_relation_from (event : Event) (relation : RelationType) (eventName : EventName) : ResultEvent =
//let delete_event (event : Event) : ResultEvent =
//let remove_event_roles (event : Event) (roles : Roles) : ResultEvent =
