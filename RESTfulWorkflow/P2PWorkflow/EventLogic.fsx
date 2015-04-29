module EventLogic

type Roles = Set<string>           //A list of roles
type RelationType =                 //The types of relations
    | Dependent
    | Exclusion
    | Response
    | Inclusion
type WorkflowName = string                  // The name of a workflow
type EventName = WorkflowName*string        // WorkflowName*EventName
type EventState = bool*bool*bool            // Executed*Pending*Includet
type UserName = string                      // The name of a user

type Relation = RelationType*EventName      // A relation containd by a event
type ToRelations = Set<Relation>
type FromRelations = Set<Relation>

// We only really use this as the state singleton (which is nice, tho)
type Event = {
    name: EventName;
    executed: bool;
    pending: bool;
    included: bool;
    toRelations: ToRelations;
    fromRelations: FromRelations;
    roles: Roles;
}

// posible results when working with events
type ResultEvent =
    | Ok of Event
    | Unauthorized
    | NotExecutable
    | MissingEvent of string
    | LockConflict

/// Creates and returns a new event from a name and a start state
let create_event (name : EventName) (state : EventState) : ResultEvent =
    // Måske cheke om der er et event med dette navn
    let a,b,c = state
    let n = {
            name        = name;
            executed    = a;
            included    = b;
            pending     = c;
            toRelations = Set.empty;
            fromRelations = Set.empty;
            roles       = Set.empty;
        }
    Ok(n)

/// Return name and state of given event
let get_event_state (event : Event) : (string*EventState) =
    let workflow, name = event.name
    (name, (event.executed, event.pending, event.included))

/// Check if given event have one of given roles
let check_roles (event : Event) (roles : Roles) : bool =
    Set.exists (fun role -> Set.exists (fun event_role -> role = role) event.roles) roles

/// Executed and returns the given envent if the given user have the reqred role
let execute (event : Event) (userName : UserName) : ResultEvent =
    //Dummy
    printfn "Send: Find %s's roles" userName
    let usersRoles = set ["Test";"test"] // the users roles

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
let remove_relation_to (event : Event) (relation : RelationType) (eventName : EventName) : ResultEvent =
    //Dummy
    if event.toRelations.Contains (relation, eventName)
    then
        let toRelation = event.toRelations.Remove (relation, eventName)
        printfn "Send: Remove fromRelation (%A, %A) to event: %A" relation event.name eventName
        Ok({event with toRelations = toRelation})
    else MissingEvent(sprintf "ERROR: no (%A, %A) relation" relation eventName)

/// Removes given relation form given event and returns the result
let remove_relation_from (event : Event) (relation : RelationType) (eventName : EventName) : ResultEvent =
    //Dummy
    if event.fromRelations.Contains (relation, eventName)
    then
        let fromRelation = event.fromRelations.Remove (relation, eventName)
        printfn "Send: Remove toRelation (%A, %A) to event: %A" relation event.name eventName
        Ok({event with toRelations = fromRelation})
    else MissingEvent(sprintf "ERROR: no (%A, %A) relation" relation eventName)

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

