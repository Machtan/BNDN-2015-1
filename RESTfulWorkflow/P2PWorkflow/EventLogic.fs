﻿module EventLogic

open Pastry
open Repository_types
open Send

type UpdateMapResult =
| MapOk of Map<string, bool*Event>
| MapErr of Result

let update_inner_map (workflow : WorkflowName) (func : Map<string, bool*Event> -> UpdateMapResult) (repository : Repository) : Result =
    let inner_map =
        match Map.tryFind workflow repository.events with
        | Some(map) -> map
        | None -> Map.empty
    match func inner_map with
    | MapOk(newMap) -> Ok({repository with events = Map.add workflow newMap repository.events})
    | MapErr(error) -> error

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
let create_event (eventName : EventName) (state : EventState) (lock : bool) (repository : Repository) : Result =
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

    // TODO Maybe check whether the workflow exists somewhere
    // before adding events

    update_inner_map workflow (fun x -> MapOk(Map.add name (lock, n) x)) repository

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
let check_if_executeble (eventName : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : bool =
    let event = get_event eventName repository

    let onlyConditions x =
        let typ, _ = x
        typ = Condition

    let checkConditions x =
        let typ, fromEventName = x
        let fromWorkflow, fromName = fromEventName
        let (_, resp, status) = send (GetIfCondition(eventName)) sendFunc repository
        check_if_positive_bool resp status

    if event.included = true
    then
        let fromRelations = Set.filter onlyConditions event.fromRelations
        Set.forall checkConditions fromRelations
    else false


/// Checks if given event is lock'et
let check_if_locked (eventName : EventName) (repository : Repository) : bool =
    let workflow, name = eventName
    match Map.tryFind workflow repository.events with
    | Some(innerMap)   ->
        match Map.tryFind name innerMap with
            | Some(lockEvent)  ->
                let lock, _ = lockEvent
                lock
            | None -> failwith "Missing Event"
    | None -> failwith "Missing Workflow"

/// Checks if given event is executed / excluded
let check_condition (eventName : EventName) (repository : Repository) : bool =
    let event = get_event eventName repository
    if event.included = true
    then
        if event.executed = true
        then true
        else false
    else true

/// Executes and returns the given event if the given user has the required role
let execute (eventName : EventName) (userName : UserName)
        (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    if check_if_executeble eventName sendFunc repository
    then
        let workflow, _ = eventName
        let _, answer, _ = send (GetUserRoles(userName, workflow)) sendFunc repository
        let usersRoles = Set.ofArray (answer.Split ',')
        if check_roles eventName usersRoles repository
        then
            let event = get_event eventName repository
            // Find out which events need to be locked before executing this one
            let onlyNecessary x =
              let typ, _ = x
              typ = Condition || typ = Exclusion
            let setSplit acc x =
                let _, event = x
                Set.add event acc
            let necessary_from_relations = Set.filter onlyNecessary event.fromRelations
            let necessary_relations = Set.union necessary_from_relations event.toRelations
            let lockSet = Set.fold setSplit Set.empty necessary_relations

            // Try to lock them all
            let lockMany (x : Set<EventName>) (repo: Repository) : bool * Repository =
                let inner event (can_continue, locked_events, locked_state) =
                    if can_continue then
                        let (state, resp, status) = send (Lock(event)) sendFunc locked_state
                        if check_if_positive status
                        then (status, Set.add x acc)
                        else (false, acc)
                    else s

                let (status, unlockSet, updated_state) = Set.foldBack inner lockSet (true, Set.empty, repo)
                if status
                then true
                else
                    let lock_succesful event =
                        let (state, resp, status) = send (Lock(event)) sendFunc repository
                        check_if_positive status
                    if Set.forall unlockSet
                    then false
                    else failwith "ERROR: Could not remove locks after failed execution"

            let (succesfully_locked, locked_state) = lockMany lockSet
            if succesfully_locked
            then
                let updateState x y =
                    let stat, repo = y
                    if stat
                    then
                        let typ, event = x
                        match typ with
                        | Condition -> y
                        | Exclusion ->
                            let RR = send (SetIncluded(event, false)) sendFunc repo
                            if check_if_positive RR
                            then
                                let newRepository, _, _ = RR
                                (true, newRepository)
                            else (false, repo)
                        | Response  ->
                            let RR = send (SetPending(event, true)) sendFunc repo
                            if check_if_positive RR
                            then
                                let newRepository, _, _ = RR
                                (true, newRepository)
                            else (false, repo)
                        | Inclusion ->
                            let RR = send (SetIncluded(event, true)) sendFunc repo
                            if check_if_positive RR
                            then
                                let newRepository, _, _ = RR
                                (true, newRepository)
                            else (false, repo)
                    else y

                let state, repository = Set.foldBack updateState event.toRelations (true, repository)
                if state
                then
                    if Set.forall (fun x -> check_if_positive (send (Unlock(x)) sendFunc repository)) lockSet
                    then
                        let inner (event : Event) : UpdateEventResult =
                            EventOk({event with executed = true; pending = false})
                        update_inner_event eventName inner repository
                    else Error("ERROR: It's not possible to unlock all events!!!")
                else Error("ERROR: It's not possible to chenge all the states!!!")
            else LockConflict
        else Unauthorized
    else NotExecutable

/// Adds given roles to given event and returns the result
let add_event_roles (eventName : EventName) (roles : Roles) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with roles = Set.fold (fun x y -> x.Add y) event.roles roles})
    update_inner_event eventName inner repository

/// Adds given relationships (going from first given event) to given event and returns the result
let add_relation_to (fromEvent : EventName) (relations : RelationType) (toEvent : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    let (updated_state, msg, status) = send (AddFromRelation(fromEvent, relations, toEvent)) sendFunc repository
    if check_if_positive (status)
    then
        let inner (event : Event) : UpdateEventResult =
            EventOk({event with toRelations = Set.add (relations, toEvent) event.toRelations})
        update_inner_event fromEvent inner updated_state
    else LockConflict

/// Adds given relationships (going to given event) to given event and returns the result
let add_relation_from (fromEvent : EventName) (relations : RelationType) (toEventName : EventName) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with fromRelations = Set.add (relations, fromEvent) event.fromRelations})
    update_inner_event toEventName inner repository

/// Chance state
let set_included (eventName : EventName) (newState : bool) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with included = newState})
    update_inner_event eventName inner repository

let set_pending (eventName : EventName) (newState : bool) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with pending = newState})
    update_inner_event eventName inner repository

let set_executed (eventName : EventName) (newState : bool) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with executed = newState})
    update_inner_event eventName inner repository

/// lock given event
let lock_event (eventName : EventName) (repository : Repository) : Result =
    let workflow, name = eventName
    let inner (x : Map<string, bool*Event>) : UpdateMapResult =
        match Map.tryFind name x with
            | Some(x') ->
                let lock, event = x'
                if not lock
                then MapOk(Map.add name (true, event) x)
                else MapErr(LockConflict)
            | None -> MapErr(MissingEvent)

    update_inner_map workflow inner repository

/// unlock given event
let unlock_event (eventName : EventName) (repository : Repository) : Result =
    let workflow, name = eventName
    let inner (x : Map<string, bool*Event>) : UpdateMapResult =
        match Map.tryFind name x with
            | Some(x') ->
                let lock, event = x'
                if lock
                then MapOk(Map.add name (false, event) x)
                else MapErr(LockConflict)
            | None -> MapErr(MissingEvent)

    update_inner_map workflow inner repository

/// Removes given relation form given event and returns the result
let remove_relation_to (fromEvent : EventName) (relations : RelationType) (toEventName : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    if check_if_positive (send (RemoveFromRelation(toEventName, relations, fromEvent)) sendFunc repository)
    then
        let inner (event : Event) : UpdateEventResult =
            if Set.contains (relations, toEventName) event.toRelations
            then EventOk({event with toRelations = Set.remove (relations, toEventName) event.toRelations})
            else EventErr(MissingRelation)
        update_inner_event fromEvent inner repository
    else LockConflict

/// Removes given relation form given event and returns the result
let remove_relation_from (fromEvent : EventName) (relations : RelationType) (toEvent : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        if Set.contains (relations, fromEvent) event.toRelations
        then EventOk({event with toRelations = Set.remove (relations, fromEvent) event.toRelations})
        else EventErr(MissingRelation)
    update_inner_event toEvent inner repository

/// Deletes given event and returns it if its susesful
let delete_event (eventName : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    //Dummy
    let workflow, eName = eventName
    let event = get_event eventName repository
    printfn "Send: remove event: %s from wrokflow: %s" eName workflow
    if Set.forall (fun x ->
        let typ, event' = x
        check_if_positive (send (RemoveToRelation(eventName, typ, event')) sendFunc repository)) event.toRelations
    then
        if Set.forall (fun x ->
            let typ, event' = x
            check_if_positive (send (RemoveFromRelation(eventName, typ, event')) sendFunc repository)) event.fromRelations
        then
            let workflow, name = eventName
            let inner (x : Map<string, bool*Event>) : UpdateMapResult =
                match Map.tryFind name x with
                    | Some(_) ->
                        MapOk(Map.remove name x)
                    | None -> MapErr(MissingEvent)

            update_inner_map workflow inner repository
        else LockConflict
    else LockConflict

/// Removes given roles form given event and returns the result
let remove_event_roles (eventName : EventName) (roles : Roles) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with roles = Set.fold (fun x y -> x.Remove y) event.roles roles})
    update_inner_event eventName inner repository

