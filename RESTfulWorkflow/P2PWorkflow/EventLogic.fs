module EventLogic

open Repository_types


let update_inner_map (workflow : WorkflowName) (func : Map<string, bool*Event> -> Map<string, bool*Event>) (repository : Repository) : Repository =
    match Map.tryFind workflow repository.events with
    | Some(x)   ->
        let newInnerMap = func x
        {repository with events = Map.add workflow newInnerMap repository.events}
    | None      -> failwith "Missing Workflow"

let update_inner_event (eventName : EventName) (func : Event -> Event) (repository : Repository) : Repository =
    let workflow, name = eventName
    let inner (eventMap : Map<string, bool*Event>) = 
        match Map.tryFind name eventMap with
            | Some(x')  ->
                let lock, event = x'
                Map.add name (lock, func event) eventMap
            | None      -> failwith "Missing Event"

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
    Ok(update_inner_map workflow (fun x -> Map.add name (false, n) x) repository)

/// Return name and state of given event
let get_event_state (eventName : EventName) (repository : Repository) : (string*EventState) =
    let _, name = eventName
    let event = get_event eventName repository
    (name, {executed = event.executed; pending = event.pending; included = event.included;})

/// Check if given event have one of given roles
let check_roles (eventName : EventName) (roles : Roles) (repository : Repository) : bool =
    let event = get_event eventName repository
    if not (Set.isEmpty event.roles)
    then Set.exists (fun role -> Set.exists (fun event_role -> event_role = role) event.roles) roles
    else true

/// Executed and returns the given envent if the given user have the reqred role
let execute (eventName : EventName) (userName : UserName) (sendFunc : SendFunc<'a>) (repository : Repository) : Result =
    //Dummy
    printfn "Send: Find %s's roles" userName
    let usersRoles = set ["Test";"test"] // the users roles

    let workflow, name = eventName
    let event = get_event eventName repository
    let inner (x : Map<string, bool*Event>) = 
        match Map.tryFind name x with
            | Some(x') ->
                let lock, event = x'
                Map.add name (lock, {event with executed = true; pending = false}) x
            | None -> failwith "Missing Event"
    
    if not event.included
    then NotExecutable
    else
        //lock andre events
        printfn "Send: luck's to many event :P"
        //Check conditions
        printfn "Send: checks alle the contitions :P"
        if Set.isEmpty event.roles 
        then Ok(update_inner_map workflow inner repository)
        //{event with executed = true; pending = false})
        else
            if check_roles eventName usersRoles repository
            then Ok(update_inner_map workflow inner repository)
            else Unauthorized

/// Adds given roles to given event and returns the result
let add_event_roles (eventName : EventName) (roles : Roles) (repository : Repository) : Result =
    let inner (event : Event) : Event =
        {event with roles = Set.fold (fun x y -> x.Add y) event.roles roles}
    Ok(update_inner_event eventName inner repository)

/// Adds given relationships (going from first given event) to given event and returns the result
let add_relation_to (fromEvent : EventName) (relations : RelationType) (toEventName : EventName) (sendFunc : SendFunc<'a>) (repository : Repository) : Result =
    //Dummy
    printfn "Send: Add fromRelation (%A %A) to %A" fromEvent relations toEventName

    let inner (event : Event) : Event =
        {event with toRelations = Set.add (relations, toEventName) event.toRelations}
    Ok(update_inner_event fromEvent inner repository)

/// Adds given relationships (going to given event) to given event and returns the result
let add_relation_from (toEvent : EventName) (relations : RelationType) (fromEventName : EventName) (repository : Repository) : Result =
    let inner (event : Event) : Event =
        {event with fromRelations = Set.add (relations, fromEventName) event.fromRelations}
    Ok(update_inner_event toEvent inner repository)

/// Removes given relation form given event and returns the result
let remove_relation_to (fromEvent : EventName) (relations : RelationType) (toEventName : EventName) (sendFunc : SendFunc<'a>) (repository : Repository) : Result =
    //Dummy
    printfn "Send: remove fromRelation (%A %A) to %A" fromEvent relations toEventName

    let inner (event : Event) : Event =
        if Set.contains (relations, toEventName) event.toRelations
        then {event with toRelations = Set.remove (relations, toEventName) event.toRelations}
        else failwith "Missing Relation"
    Ok(update_inner_event fromEvent inner repository)

/// Removes given relation form given event and returns the result
let remove_relation_from (toEvent : EventName) (relations : RelationType) (fromEventName : EventName) (sendFunc : SendFunc<'a>) (repository : Repository) : Result =
    let inner (event : Event) : Event =
        if Set.contains (relations, fromEventName) event.toRelations
        then {event with toRelations = Set.remove (relations, fromEventName) event.toRelations}
        else failwith "Missing Relation"
    Ok(update_inner_event toEvent inner repository)

/// Deletes given event and returns it if its susesful
let delete_event (eventName : EventName) (sendFunc : SendFunc<'a>) (repository : Repository) : Result =
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
    let inner (x : Map<string, bool*Event>) = 
        match Map.tryFind name x with
            | Some(_) ->
                Map.remove name x
            | None -> failwith "Missing Event"

    Ok(update_inner_map workflow inner repository)

/// Removes given roles form given event and returns the result
let remove_event_roles (eventName : EventName) (roles : Roles) (repository : Repository) : Result =
    let inner (event : Event) : Event =
        {event with roles = Set.fold (fun x y -> x.Remove y) event.roles roles}
    Ok(update_inner_event eventName inner repository)

