﻿module EventLogic

type Roles = Set<string>           //A list of roles
type RelationType =                 //The types of relations
    | Dependent
    | Exclusion
    | Response
    | Inclusion
type WorkflowName = string                  // The name of a workflow
type EventName = WorkflowName*string        // WorkflowName*EventName
type UserName = string                      // The name of a user

type Relation = RelationType*EventName      // A relation containd by a event
type ToRelations = Set<Relation>
type FromRelations = Set<Relation>

// We only really use this as the state singleton (which is nice, tho)
type Event = {
    name: EventName;
    included: bool;
    pending: bool;
    executed: bool;
    toRelations: ToRelations;
    fromRelations: FromRelations;
    roles: Roles;
}

// included, pending, executed
type EventState = {
    included: bool;
    pending: bool;
    executed: bool;
}

// posible results when working with events
type ResultEvent =
    | Ok of Event
    | Unauthorized
    | NotExecutable
    | MissingRelation
    | Error of string
    | LockConflict

/// Creates and returns a new event from a name and a start state
let create_event (name : EventName) (state : EventState) : ResultEvent =
    // Måske cheke om der er et event med dette navn
    let n = {
            name        = name;
            executed    = state.executed;
            included    = state.included;
            pending     = state.pending;
            toRelations = Set.empty;
            fromRelations = Set.empty;
            roles       = Set.empty;
        }
    Ok(n)

/// Return name and state of given event
let get_event_state (event : Event) : (string*EventState) =
    let workflow, name = event.name
    (name, {executed = event.executed; pending = event.pending; included = event.included;})

/// Check if given event have one of given roles
let check_roles (event : Event) (roles : Roles) : bool =
    if not (Set.isEmpty event.roles)
    then Set.exists (fun role -> Set.exists (fun event_role -> event_role = role) event.roles) roles
    else true

/// Executed and returns the given envent if the given user have the reqred role
let execute (event : Event) (userName : UserName) : ResultEvent =
    //Dummy
    printfn "Send: Find %s's roles" userName
    let usersRoles = set ["Test";"test"] // the users roles

    if not event.included
    then NotExecutable
    else
        if Set.isEmpty event.roles 
        then Ok({event with executed = true; pending = false})
        else
            if check_roles event usersRoles
            then Ok({event with executed = true})
            else Unauthorized

/// Adds given roles to given event and returns the result
let add_event_roles (event : Event) (roles : Roles) : ResultEvent =
    Ok({event with roles = Set.fold (fun x y -> x.Add y) event.roles roles})

/// Adds given relationships (going from first given event) to given event and returns the result
let add_relation_to (event : Event) (relations : RelationType) (eventName : EventName) : ResultEvent =
    //Dummy
    printfn "Send: Add fromRelation (%A %A) to %A" event.name relations eventName

    Ok({event with toRelations = event.toRelations.Add (relations, eventName)})

/// Adds given relationships (going to given event) to given event and returns the result
let add_relation_from (event : Event) (relations : RelationType) (eventName : EventName) : ResultEvent =
    Ok({event with fromRelations = event.fromRelations.Add (relations, eventName)})

/// Removes given relation form given event and returns the result
let remove_relation_to (event : Event) (relation : Relation) : ResultEvent =
    //Dummy
    let typ, toEvent = relation
    if event.toRelations.Contains (typ, toEvent)
    then
        let toRelation = event.toRelations.Remove (typ, toEvent)
        printfn "Send: Remove fromRelation (%A, %A) to event: %A" typ event.name toEvent
        Ok({event with toRelations = toRelation})
    else MissingRelation

/// Removes given relation form given event and returns the result
let remove_relation_from (event : Event) (relation : Relation) : ResultEvent =
    //Dummy
    let typ, fromEvent = relation
    if event.fromRelations.Contains (typ, fromEvent)
    then
        let fromRelation = event.fromRelations.Remove (typ, fromEvent)
        printfn "Send: Remove toRelation (%A, %A) to event: %A" typ event.name fromEvent
        Ok({event with fromRelations = fromRelation})
    else MissingRelation

/// Deletes given event and returns it if its susesful
let delete_event (event : Event) : ResultEvent =
    //Dummy
    let workflow, eventName = event.name
    printfn "Send: remove event: %s from wrokflow: %s" eventName workflow
    Set.forall (fun x -> 
        let typ, event' = x
        printfn "Send: Remove fromRelation (%A,%A) to event: %A" typ eventName event'
        true ) event.toRelations
    Set.forall (fun x ->
        let typ, event' = x
        printfn "Send: Remove toRelation (%A,%A) to event: %A" typ eventName event'
        true ) event.fromRelations

    Ok(event)

/// Removes given roles form given event and returns the result
let remove_event_roles (event : Event) (roles : Roles) : ResultEvent =
    Ok({event with roles = Set.fold (fun x y -> x.Remove y) event.roles roles})

