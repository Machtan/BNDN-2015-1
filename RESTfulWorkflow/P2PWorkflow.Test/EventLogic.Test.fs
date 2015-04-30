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
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let result = create_event name state
        let event = match result with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(event.name, name)
        Assert.AreEqual(event.included, true)
        Assert.AreEqual(event.pending, true)
        Assert.AreEqual(event.executed, false)
        Assert.AreEqual(event.toRelations, Set.empty)
        Assert.AreEqual(event.fromRelations, Set.empty)
        Assert.AreEqual(event.roles, Set.empty)


//let get_event_state (event : Event) : (string*EventState) =

    [<Test>]
    member public x.``Test if the states are returns in the right order`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let result = create_event name state
        let eventName, state = match result with
            | Ok(x) -> get_event_state x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(eventName, "TestEvent")
        Assert.AreEqual(true, state.included)
        Assert.AreEqual(true, state.pending)
        Assert.AreEqual(false, state.executed)

//let check_roles (event : Event) (roles : Roles) : bool =

    [<Test>]
    member public x.``Test if a role can be fund in many`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_event_roles event (set ["Test"; "Test1"; "Test2"]) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(true, check_roles event (set ["Test"]))
        Assert.AreEqual(true, check_roles event (set ["Test"; "Test2"]))
        Assert.AreEqual(true, check_roles event (set ["Test1"; "WrongRole"]))

    [<Test>]
    member public x.``Test if false is returned when given wong roles`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_event_roles event (set ["Test"; "Test1"; "Test2"]) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(false, check_roles event (Set.empty))
        Assert.AreEqual(false, check_roles event (set ["WrongRole"]))
        Assert.AreEqual(false, check_roles event (set ["WrongRole1"; "WrongRole2"; "WrongRole3"]))

    [<Test>]
    member public x.``Test if true is returned when the event have no roles`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_event_roles event (Set.empty) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(true, check_roles event (Set.empty))
        Assert.AreEqual(true, check_roles event (set ["Test"]))
        Assert.AreEqual(true, check_roles event (set ["Test"; "Test2"]))
        Assert.AreEqual(true, check_roles event (set ["Test1"; "Test2"; "Test3"]))

//let execute (event : Event) (userName : UserName) : ResultEvent =

    [<Test>]
    member public x.``Test if a event kan be execurted`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match execute event "DummyTestUser" with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(true, event.included)
        Assert.AreEqual(false, event.pending)
        Assert.AreEqual(true, event.executed)

    [<Test>]
    member public x.``Test if a event stops being pending when executed`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match execute event "DummyTestUser" with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(true, event.included)
        Assert.AreEqual(false, event.pending)
        Assert.AreEqual(true, event.executed)

    [<Test>]
    member public x.``Test if a event can't be executed when not included`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = false; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let result = match execute event "DummyTestUser" with
            | x     -> x

        //Assert
        Assert.AreEqual(NotExecutable, result)

//let add_event_roles (event : Event) (roles : Roles) : ResultEvent =

    [<Test>]
    member public x.``Test if role can be added to an event`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_event_roles event (set ["Test"; "Test1"; "Test2"]) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(true, check_roles event (set ["Test"]))
        Assert.AreEqual(true, check_roles event (set ["Test2"]))
        Assert.AreEqual(true, check_roles event (set ["Test1"]))

    [<Test>]
    member public x.``Test if role can be added to an event twice`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_event_roles event (set ["Test"]) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_event_roles event (set ["Test"]) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(true, check_roles event (set ["Test"]))

    [<Test>]
    member public x.``Test if no role can be added to an event`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event2 = match add_event_roles event (Set.empty) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(event, event2)

//let add_relation_to (event : Event) (relations : RelationType) (eventName : EventName) : ResultEvent =

    [<Test>]
    member public x.``Test if a toRelation can be added to an event`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_to event Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(1, event.toRelations.Count)

    [<Test>]
    member public x.``Test that ther can't be multible copys of toRelations`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_to event Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_to event Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(1, event.toRelations.Count)

    [<Test>]
    member public x.``Test all relation types toRelations`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_to event Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_to event Exclusion ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_to event Response ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_to event Inclusion ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(4, event.toRelations.Count)

//let add_relation_from (event : Event) (relations : RelationType) (eventName : EventName) : ResultEvent =

    [<Test>]
    member public x.``Test if a fromRelation can be added to an event`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_from event Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(1, event.fromRelations.Count)

    [<Test>]
    member public x.``Test that ther can't be multible copys of fromRelations`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_from event Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_from event Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(1, event.fromRelations.Count)

    [<Test>]
    member public x.``Test all relation types fromRelations`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_from event Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_from event Exclusion ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_from event Response ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_relation_from event Inclusion ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(4, event.fromRelations.Count)

//let remove_relation_from (event : Event) (relation : Relation) : ResultEvent =

    [<Test>]
    member public x.``Test if a toRelation can be removed to an event`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event1 = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event2 = match add_relation_to event1 Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event3 = match remove_relation_to event2 (Dependent, ("TestWorkflow", "TestEvent")) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(0, event3.toRelations.Count)
        Assert.AreEqual(event1, event3)

//let remove_relation_from (event : Event) (relation : RelationType) (eventName : EventName) : ResultEvent =

    [<Test>]
    member public x.``Test if a fromRelation can be removed to an event`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event1 = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event2 = match add_relation_from event1 Dependent ("TestWorkflow", "TestEvent") with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event3 = match remove_relation_from event2 (Dependent, ("TestWorkflow", "TestEvent")) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        //Assert
        Assert.AreEqual(0, event3.fromRelations.Count)
        Assert.AreEqual(event1, event3)

//let delete_event (event : Event) : ResultEvent =
//let remove_event_roles (event : Event) (roles : Roles) : ResultEvent =

    [<Test>]
    member public x.``Test if a role can be removed from an event`` () =
        //Arrange
        let name, state = ("WorkflowName", "TestEvent"), {included = true; pending = true; executed = false;}
        //Act
        let event = match create_event name state with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match add_event_roles event (set ["Test"; "Test1"; "Test2"]) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)
        let event = match remove_event_roles event (set ["Test"; "Test1"]) with
            | Ok(x) -> x
            | x     -> failwith (sprintf "ERROR: %A" x)

        //Assert
        Assert.AreEqual(false, check_roles event (set ["Test"]))
        Assert.AreEqual(false, check_roles event (set ["Test1"]))
        Assert.AreEqual(true, check_roles event (set ["Test2"]))