module Rest

open Repository_types
open UserLogic
open EventLogic
open WorkflowLogic
open Pastry
open PastryTypes

// Splits a string into a list by a char
let split (str: string) (c: char) =
    List.ofArray (str.Split([|c|]))

// Attempts to get an event from the workflow
let getEvent workflowName eventName repository : (EventState option * string * int) =
        let event = (workflowName,eventName) : EventName
        let response = get_event_state event repository
        Some(response),"Ok", 200

// Gets the 'executed' state of an event
let getExecuted event workflowName repo : ResourceResponse<Repository> =
    let response = getEvent workflowName event repo
    let (eventState,message,statusCode) = response
    match (eventState) with
        | Some(evState) -> (repo,(string)evState.executed, statusCode)
        | None ->  (repo,"Event could not be received.", statusCode)

// Gets the 'included' state of an event
let getIncluded event workflowName repo : ResourceResponse<Repository>  =
    let response = getEvent workflowName event repo
    let (eventState,message,statusCode) = response
    match (eventState) with
        | Some(evState) -> (repo,(string)evState.included, statusCode)
        | None ->  (repo,"Event could not be received.", statusCode)

// Gets the 'executable' state of an event
let getExecutable event workflowName repo sendfunc  : ResourceResponse<Repository> =
    let response = getEvent workflowName event repo
    let (eventState,message,statusCode) = response
    match (eventState) with
        | Some(evState) ->  (repo,string (check_if_executeble (workflowName,event) sendfunc repo), statusCode)
        | None ->  (repo,"Event could not be received.", statusCode)

// Gets the 'pending' state of an event
let getPending event workflowName repo : ResourceResponse<Repository> =
    let response = getEvent workflowName event repo
    let (eventState,message,statusCode) = response
    match (eventState) with
        | Some(evState) -> (repo,(string)evState.pending, statusCode)
        | None ->  (repo,"Event could not be received.", statusCode)
        
// Attempts to execute the given event
let setExecuted workflowName eventName userName repo sendFunc :ResourceResponse<Repository>  =
    let event = (workflowName,eventName): EventName
    let response = execute event userName sendFunc repo
    match (response) with
            | Result.Ok(r) -> (r,"Executed", 201)
            | Result.Unauthorized -> (repo,"Unauthorized", 401)
            | Result.NotExecutable -> (repo,"The event is not executable.", 400)
            | Result.MissingRelation -> (repo,"The relation is missing.", 400)
            | Result.LockConflict -> (repo,"Encountered LockConflict.", 400)
            | Result.Error(s) -> (repo,s, 400)

// Attempts to create a new event
let deleteEvent eventName workflowName (repository : Repository) sendFunc : ResourceResponse<Repository> =    
        let event = (workflowName,eventName): EventName
        let response = delete_event event sendFunc repository
        match (response) with
            | Result.Ok(r) -> (r,"Deleted.", 200)
            | Result.Unauthorized -> (repository,"Unauthorized", 401)
            | Result.NotExecutable -> (repository,"The event is not executable.", 400)
            | Result.MissingRelation -> (repository,"The relation is missing.", 400)
            | Result.LockConflict -> (repository,"Encountered LockConflict.", 400)
            | Result.Error(s) -> (repository,s, 400)

let createEvent eventName workflowName attribute (repository : Repository) : ResourceResponse<Repository> =
    let args = split attribute ' '
    let str = List.head args
    let roles = List.tail args
    if not (str.Length = 3) then
        let msg = sprintf "Received invalid initial event state: %s" str
        printfn "%s" msg
        (repository,msg, 400)
    else        
        let initialState = {included = (str.[0] = '1'); pending = (str.[1] = '1'); executed = (str.[2] = '1')} : EventState
        let event = (workflowName,eventName): EventName
        let response = create_event event initialState false repository
        match (response) with
            | Result.Ok(r) -> (r,"Created.", 201)
            | Result.Unauthorized -> (repository,"Unauthorized", 401)
            | Result.NotExecutable -> (repository,"The event is not executable.", 400)
            | Result.MissingRelation -> (repository,"The relation is missing.", 400)
            | Result.LockConflict -> (repository,"Encountered LockConflict.", 400)
            | Result.Error(s) -> (repository,s, 400)

// Adds a new relation
let addRelation workflowName eventName attribute repo sendFunc : ResourceResponse<Repository>=
    let args = split attribute ' '
    let url = List.head args
    let eventTo = (workflowName,List.head (List.tail args))
    
    let urlParts = split url '/'
    let typ = Seq.last urlParts
    let rel =
        match typ with
        | "exclusion"   -> Some(Exclusion)
        | "condition"   -> Some(Condition)
        | "response"    -> Some(Response)
        | "inclusion"   -> Some(Inclusion)
        | _             -> None
    match (rel) with
        | Some(r) ->   
                        let eventFrom = (workflowName,eventName): EventName 
                        let response = add_relation_to eventFrom r eventTo sendFunc repo
                        match (response) with
                            | Result.Ok(r) -> (r,"Created.", 201)
                            | Result.Unauthorized -> (repo,"Unauthorized", 401)
                            | Result.NotExecutable -> (repo,"The event is not executable.", 400)
                            | Result.MissingRelation -> (repo,"The relation is missing.", 400)
                            | Result.LockConflict -> (repo,"Encountered LockConflict.", 400)
                            | Result.Error(s) -> (repo,s, 400)
        | None -> (repo,"Invalid relation type", 400)
    
let createWorkflow workflowName repo  : ResourceResponse<Repository> =
        let response = create_workflow workflowName repo
        match (response) with
            | Result.Ok(r) -> (r,"Created.", 201)
            | Result.Unauthorized -> (repo,"Unauthorized", 401)
            | Result.NotExecutable -> (repo,"The event is not executable.", 400)
            | Result.MissingWorkflow -> (repo,"Workflow is missing", 400)
            
let deleteWorkflow workflowName repo  : ResourceResponse<Repository> =
        let response = delete_workflow workflowName repo
        match (response) with
            | Result.Ok(r) -> (r,"Deleted.", 200)
            | Result.Unauthorized -> (repo,"Unauthorized", 401)
            | Result.NotExecutable -> (repo,"The event is not executable.", 400)
            | Result.MissingWorkflow -> (repo,"Workflow is missing", 400)


let createUser userName repo  : ResourceResponse<Repository> =
        let response = create_user userName repo
        match (response) with
            | ResultUser.Ok(r) -> (r,"Created.", 201)
            | ResultUser.Unauthorized -> (repo,"Unauthorized", 401)
            | ResultUser.NotExecutable -> (repo,"The event is not executable.", 400)
            | ResultUser.MissingUser -> (repo,"user is missing", 400)

let deleteUser userName repo  : ResourceResponse<Repository> = 
        let response = delete_user userName repo
        match (response) with
            | ResultUser.Ok(w) -> (repo,"Deleted.", 200)
            | ResultUser.Unauthorized -> (repo,"Unauthorized", 401)
            | ResultUser.NotExecutable -> (repo,"The event is not executable.", 400)
            | ResultUser.MissingUser -> (repo,"The relation is missing.", 400) 


let getWorkFlowEvents workflow repo  : ResourceResponse<Repository> = 
        let response = get_workflow_events workflow repo
        (repo,String.concat "," response, 200)

// Creates a new repository
let create_repository () : Repository =
    { events = Map.empty; users = Map.empty; workflows = Map.empty; logs = []; }

// Serializes the permissions of a given user for HTTP transfer
let serialize_user_permissions (user: User) : string =
    "Not Implemented"

// Matches the given user and
let handle_user (user_name: string) (meth: string) (repo: Repository) : ResourceResponse<Repository> =
    match meth with
    | "POST" -> // Create a new user
        createUser user_name repo
    | "DELETE" -> // Delete a user
        deleteUser user_name repo
    | "GET" -> // Get the workflow permissions of a user
        match Map.tryFind user_name repo.users with
        | None ->
            repo, "User not found", 404
        | Some(user) ->
            repo, serialize_user_permissions user, 200
    | _ ->
        repo, "Unsupported user operation", 400

// So amazing
let SEPARATOR = "\n"

// Handles requests for the givien workflow (getting the events in it)
let handle_workflow (wf: string) (meth: string) (repo: Repository) : ResourceResponse<Repository> =
    match meth with
    | "GET" ->
        getWorkFlowEvents wf repo
    | "POST" ->
        createWorkflow wf repo
    | "DELETE" ->
        deleteWorkflow wf repo
    | _ ->
        repo, "Unsupported workflow operation", 400

// Matches the given event and tries to handle the request
let handle_event (workflow_name: string) (event_name: string) (attribute: string)
        (meth: string) (message: string) (repo: Repository) (sendFunc : SendFunc<'a>) : ResourceResponse<Repository> =
       
    // Find the event in this repository if it exists
    let response =
        match meth, attribute with
        | "POST", "" ->
            createEvent event_name workflow_name message repo
        | "DELETE", "" ->
            deleteEvent event_name workflow_name repo sendFunc
        | "GET", "executed" ->
            getExecuted event_name workflow_name repo
        | "PUT", "executed" ->
            setExecuted workflow_name event_name message repo sendFunc
        | "GET", "pending" ->
            getPending event_name workflow_name repo
        | "GET", "included" ->
            getIncluded event_name workflow_name repo
        | "GET", "executable" ->
            getExecutable event_name workflow_name repo sendFunc
        | "POST", relation ->
            addRelation workflow_name event_name relation repo sendFunc
        | _ ->
            failwith "Not Implemented"//"Unsupported operation", 404, "Not found", initialState
    response

// The actual resource handling function
let resource_handler (path: string) (meth: string) (message: string) (send_func: SendFunc<Repository>)
        (initial_state: Repository) : ResourceResponse<Repository> =

    let parts = split path '/'
    let response =
        //printfn "Parts: %A" parts
        match parts with
        | [] ->
            initial_state, "Nothing asked, nothing found.",400
        | "user"::user::[] ->
            handle_user user meth initial_state

        | "workflow"::workflow::[] ->
            handle_workflow workflow meth initial_state

        | "workflow"::workflow::event::attribute::[] ->
            handle_event workflow event attribute meth message initial_state send_func
        | _ ->
            printfn "Invalid path gotten: %s" path
            initial_state, "Invalid path", 400
    response

let KonoTestoKawaii =
    printfn "Hello migrator"

    let user_list = [
        ("Bob", ("Bob", [("Hospital", Set.ofList ["Medic"; "Patient"; "Janitor"])]));
        ("Alice", ("Alice", [("Jail", Set.ofList ["Medic"; "Inmate"; "Head honcho"])]));
    ]
    let repo_list = [
        ("Hospital", ["Enter"; "Leave"; "Surgery"; "Terrible accident"; "Zombie outbreak"]);
        ("Kindergarten", ["Enter"; "Eat"; "Run with scissors"; "Play with mud"])
    ]
    let eve w n i p e r t =
        let event = {
            name = (w, n);
            included = i;
            pending = p;
            executed = e;
            toRelations = Set.ofList t;
            fromRelations = Set.empty;
            roles = Set.ofList r
        }
        (false, event)
    let event_list = [
        ("Hospital", Map.ofList [
            ("Enter", eve "Hospital" "Enter" true true false ["Patient"] [(Condition,("Hospital", "Leave")); (Condition, ("Hospital", "Surgery"))]);
            ("Surgery", eve "Hospital" "Surgery" false false true ["Medic"] [(Condition, ("Hospital", "Terrible accident"))]);
            ("Zombie outbreak", eve "Hospital" "Zombie outbreak" true false false ["Janitor"] []);
        ]);
    ]
    let test_repository = {
        events = Map.ofList event_list;
        users = Map.ofList user_list;
        workflows = Map.ofList repo_list;
        logs = [];
    }

    let resp = handle_user "Bob" "DELETE" test_repository
    let (repo,message,status) = resp
    let resp2 = handle_event "Hospital" "Enter" "excluded" "GET" "Bob"  repo (fun a b c d ->  (repo, "Medic,Patient,Janitor", 200))
    0