module EventLogic

open Repository_types

type UpdateMapResult =
| MapOk of Map<string, bool*Event>
| MapErr of Result

let update_inner_map (workflow : WorkflowName) (func : Map<string, bool*Event> -> UpdateMapResult) (repository : Repository) : Result =
    match Map.tryFind workflow repository.events with
    | Some(innerMap)   ->
        match func innerMap with
        | MapOk(newMap) -> Ok({repository with events = Map.add workflow newMap repository.events})
        | MapErr(errer) -> errer
    | None      -> MissingWorkflow

type UpdateEventResult =
| EventOk of Event
| EventErr of Result

let update_inner_event (eventName : EventName) (func : Event -> UpdateEventResult) (repository : Repository) : Result =
    let workflow, name = eventName
    let inner (eventMap : Map<string, bool*Event>) : UpdateMapResult = 
        match Map.tryFind name eventMap with
            | Some(lockEvent)  ->
                let lock, event = lockEvent
                match func event with
                | EventOk(newEvent) -> MapOk(Map.add name (lock, newEvent) eventMap)
                | EventErr(errer)   -> MapErr(errer)
            | None      -> MapErr(MissingEvent)

    update_inner_map workflow inner repository

let get_event (eventName : EventName) (repository : Repository) : Event =
    let workflow, name = eventName
    match repository.events.TryFind workflow with
        | Some(x) -> 
            match x.TryFind name with
            | Some(x) ->
                let _, event = x
                event
            | None -> failwith "Missing Event"
        | None -> failwith "Missing Workflow"

/// Creates and returns a new event from a name and a start state
let create_event (eventName : EventName) (state : EventState) (repository : Repository) : Result =
    let n = {
            name        = eventName;
            executed    = state.executed;
            included    = state.included;
            pending     = state.pending;
            toRelations = Set.empty;
            fromRelations = Set.empty;
            roles       = Set.empty;
        }
    let workflow, name = eventName

    //Det skal lige chekkes om workflow'et ekesitire
    update_inner_map workflow (fun x -> MapOk(Map.add name (false, n) x)) repository

/// Return name and state of given event
let get_event_state (eventName : EventName) (repository : Repository) : EventState =
    let _, name = eventName
    let event = get_event eventName repository
    {executed = event.executed; pending = event.pending; included = event.included;}

/// Check if given event have one of given roles
let check_roles (eventName : EventName) (roles : Roles) (repository : Repository) : bool =
    let event = get_event eventName repository
    if not (Set.isEmpty event.roles)
    then Set.exists (fun role -> Set.exists (fun event_role -> event_role = role) event.roles) roles
    else true

/// Checks if given event is executeble
let check_if_executeble (eventName : EventName) (repository : Repository) : bool =
    failwith "Not implemented yet"

/// Checks if given event is luck'et
let check_if_lucked (eventName : EventName) (repository : Repository) : bool =
    failwith "Not implemented yet"

/// Checks if given event is executed / excluded
let check_condition (eventName : EventName) (repository : Repository) : bool =
    failwith "Not implemented yet"

/// Executed and returns the given envent if the given user have the reqred role
let execute (eventName : EventName) (userName : UserName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    //Dummy
    printfn "Send: Find %s's roles" userName
    let usersRoles = set ["Test";"test"] // the users roles

    ///-----Midlertidig---- 
    //                   Updated state, response, status code
    // type ResourceResponse<'a> = 'a * string * int

    // A function for the resource request func to send requests through
    //               partial_resource_url, method, data, state -> response
    // type SendFunc<'a> = string -> string -> string -> 'a -> ResourceResponse<'a>

    //let _, usersRoles, _ = sendFunc (sprintf "user/%s" userName) "GET" "" repository

    let workflow, name = eventName
    let event = get_event eventName repository
    let inner (x : Map<string, bool*Event>) : UpdateMapResult = 
        match Map.tryFind name x with
            | Some(x') ->
                let lock, event = x'
                MapOk(Map.add name (lock, {event with executed = true; pending = false}) x)
            | None -> MapErr(MissingEvent)
    
    if not event.included
    then NotExecutable
    else
        //lock andre events
        printfn "Send: luck's to many event :P"
        //Check conditions
        printfn "Send: checks alle the contitions :P"
        if Set.isEmpty event.roles 
        then update_inner_map workflow inner repository
        //{event with executed = true; pending = false})
        else
            if check_roles eventName usersRoles repository
            then update_inner_map workflow inner repository
            else Unauthorized

/// Adds given roles to given event and returns the result
let add_event_roles (eventName : EventName) (roles : Roles) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with roles = Set.fold (fun x y -> x.Add y) event.roles roles})
    update_inner_event eventName inner repository

/// Adds given relationships (going from first given event) to given event and returns the result
let add_relation_to (fromEvent : EventName) (relations : RelationType) (toEventName : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    //Dummy
    printfn "Send: Add fromRelation (%A %A) to %A" fromEvent relations toEventName

    let thisWorkflow, thisEvent = fromEvent
    let toWorkflow, toEvent = toEventName
    let _, _, status = sendFunc (sprintf "%s/%s/%A" thisWorkflow thisEvent relations) "GET" (sprintf "%s, %s" toWorkflow toEvent) repository
    //test om svaret er rektigt

    let inner (event : Event) : UpdateEventResult =
        EventOk({event with toRelations = Set.add (relations, toEventName) event.toRelations})
    update_inner_event fromEvent inner repository

/// Adds given relationships (going to given event) to given event and returns the result
let add_relation_from (toEvent : EventName) (relations : RelationType) (fromEventName : EventName) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with fromRelations = Set.add (relations, fromEventName) event.fromRelations})
    update_inner_event toEvent inner repository

/// luck given event
let luck_event (eventName : EventName) (repository : Repository) : Result =
    failwith "Not implemented yet"
/// unluck given event
let unluck_event (eventName : EventName) (repository : Repository) : Result =
    failwith "Not implemented yet"

/// Removes given relation form given event and returns the result
let remove_relation_to (fromEvent : EventName) (relations : RelationType) (toEventName : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    //Dummy
    printfn "Send: remove fromRelation (%A %A) to %A" fromEvent relations toEventName

    let inner (event : Event) : UpdateEventResult =
        if Set.contains (relations, toEventName) event.toRelations
        then EventOk({event with toRelations = Set.remove (relations, toEventName) event.toRelations})
        else EventErr(MissingRelation)
    update_inner_event fromEvent inner repository

/// Removes given relation form given event and returns the result
let remove_relation_from (toEvent : EventName) (relations : RelationType) (fromEventName : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        if Set.contains (relations, fromEventName) event.toRelations
        then EventOk({event with toRelations = Set.remove (relations, fromEventName) event.toRelations})
        else EventErr(MissingRelation)
    update_inner_event toEvent inner repository

/// Deletes given event and returns it if its susesful
let delete_event (eventName : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    //Dummy
    let workflow, eName = eventName
    let event = get_event eventName repository
    printfn "Send: remove event: %s from wrokflow: %s" eName workflow
    Set.forall (fun x -> 
        let typ, event' = x
        printfn "Send: Remove fromRelation (%A,%A) to event: %A" typ eventName event'
        true ) event.toRelations
    Set.forall (fun x ->
        let typ, event' = x
        printfn "Send: Remove toRelation (%A,%A) to event: %A" typ eventName event'
        true ) event.fromRelations

    let workflow, name = eventName
    let inner (x : Map<string, bool*Event>) : UpdateMapResult = 
        match Map.tryFind name x with
            | Some(_) ->
                MapOk(Map.remove name x)
            | None -> MapErr(MissingEvent)

    update_inner_map workflow inner repository

/// Removes given roles form given event and returns the result
let remove_event_roles (eventName : EventName) (roles : Roles) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with roles = Set.fold (fun x y -> x.Remove y) event.roles roles})
    update_inner_event eventName inner repository

