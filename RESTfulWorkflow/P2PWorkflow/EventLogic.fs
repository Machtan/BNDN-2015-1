module EventLogic

open System
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

// Attempts to get an event in the repository
let get_event (eventName : EventName) (repository : Repository) : ReadResult<Event * bool> =
    let workflow, name = eventName
    match repository.events.TryFind workflow with
        | Some(x) ->
            match x.TryFind name with
            | Some((locked, event)) ->
                ReadResult.Ok((event, locked))
            | None ->
                NotFound(NotFoundError.Event)
        | None ->
            NotFound(NotFoundError.Workflow)

/// Creates and returns a new event from a name and a start state
let create_event (eventName : EventName) (state : EventState)
        (roles: string list) (locked : bool) (repository : Repository) : Result =
    let event: Event = {
        name        = eventName;
        executed    = state.executed;
        included    = state.included;
        pending     = state.pending;
        toRelations = Set.empty;
        fromRelations = Set.empty;
        roles       = Set.ofList roles;
    }
    let workflow, name = eventName

    // TODO Maybe check whether the workflow exists somewhere
    // before adding events

    update_inner_map workflow (fun x -> MapOk(Map.add name (locked, event) x)) repository

/// Check if given event have one of given roles
let check_roles (eventName : EventName) (user_roles : Roles) (repository : Repository) : ReadResult<bool> =
    match get_event eventName repository with
    | ReadResult.Ok(event, _) ->
        if not (Set.isEmpty event.roles) then
            let has_common_role = not (Set.isEmpty (Set.intersect event.roles user_roles))
            ReadResult.Ok(has_common_role)
        else
            ReadResult.Ok(true)
    | NotFound(error) ->
        NotFound(error)

/// Checks if given event is executeble
let check_if_executable (eventName : EventName) (user: string)
        (sendFunc : SendFunc<Repository>) (repository : Repository)
        : ReadResult<ExecutableState> =
    let workflow_name, _ = eventName
    //printfn "EXECHECK: Checking if %A is executable (wf: %s)" eventName workflow_name
    // Check if the user can actually execute this
    let (_, answer, status) = send (GetUserRoles(user, workflow_name)) sendFunc repository
    let user_found = check_if_positive status
    let roles =
        if user_found then
            Set.ofArray (answer.Split ',')
        else
            printfn "EXECHECK: User '%s' not found: '%s'" user answer
            Set.empty
    let event_result = get_event eventName repository
    match event_result with
    | ReadResult.Ok((event, false)) ->

        let onlyConditions x =
            let typ, _ = x
            typ = Condition

        let checkConditions x =
            let typ, fromEventName = x
            let fromWorkflow, fromName = fromEventName
            let (_, resp, status) = send (GetIfCondition(eventName)) sendFunc repository
            check_if_positive_bool resp status

        if event.included then
            let fromRelations = Set.filter onlyConditions event.fromRelations
            if Set.forall checkConditions fromRelations then
                ReadResult.Ok(ExecutableState.Executable)
            else
                ReadResult.Ok(ExecutableState.NotExecutable)
        else
            ReadResult.Ok(ExecutableState.NotExecutable)

    | ReadResult.Ok((_, true)) ->
        ReadResult.Ok(ExecutableState.Locked)

    | NotFound(err) ->
        NotFound(err)

/// Checks if given event is lock'et
let check_if_locked (eventName : EventName) (repository : Repository) : ReadResult<bool> =
    match get_event eventName repository with
    | ReadResult.Ok((_, locked)) ->
        ReadResult.Ok(locked)
    | NotFound(error) ->
        NotFound(error)

/// Checks if given event is executed / excluded
// (whether the condition is fulfilled)
let check_condition (eventName : EventName) (repository : Repository) : ReadResult<bool> =
    match get_event eventName repository with
    | ReadResult.Ok((event, _)) ->
        ReadResult.Ok(event.executed || (not event.included))
    | NotFound(error) ->
        NotFound(error)

// Try to lock them all
let lock_all (events_to_lock: Set<EventName>) (sendFunc: SendFunc<Repository>)
        (repo: Repository) : bool * Repository =
    let inner event (can_continue, locked_events, locked_state) =
        if can_continue then
            let (state, resp, status) = send (Lock(event)) sendFunc locked_state
            if check_if_positive status
            then (true, Set.add event locked_events, state)
            else (false, locked_events, locked_state)
        else (can_continue, locked_events, locked_state)

    // TODO: Send this status further
    let (status, unlockSet, updated_state) = Set.foldBack inner events_to_lock (true, Set.empty, repo)
    if status then
        true, updated_state
    else
        let lock_succesful event =
            let (state, resp, status) = send (Lock(event)) sendFunc repo
            check_if_positive status
        if Set.forall lock_succesful unlockSet then
            false, repo // Give the old back
        else failwith "ERROR: Could not remove locks after failed execution"

// Updates the state of the given event. Used for a folding loop in execute
let update_all (relations: Set<Relation>) (send_func: SendFunc<Repository>)
        (state: Repository) : bool * Repository=
    let update (reltype, event) (can_continue, updated_repo) : bool * Repository =
        printfn "EXECUTE: Updating %A to %A (continue: %A)..." reltype event can_continue
        if can_continue then
            match reltype with
            | Condition -> // We check backwards, so nothing here
                (can_continue, updated_repo)
            | Exclusion ->
                let (new_repo, resp, status) = send (SetIncluded(event, false)) send_func updated_repo
                if check_if_positive status
                then (true, new_repo)
                else
                    printfn "EXECUTE ERROR: Could not exclude '%A' | %d | '%s'!" event status resp
                    (false, updated_repo)
            | Response  ->
                let (new_repo, resp, status) = send (SetPending(event, true)) send_func updated_repo
                if check_if_positive status
                then (true, new_repo)
                else
                    printfn "EXECUTE ERROR: Could not notify '%A' | %d | '%s'!" event status resp
                    (false, updated_repo)
            | Inclusion ->
                let (new_repo, resp, status) = send (SetIncluded(event, true)) send_func updated_repo
                if check_if_positive status
                then (true, new_repo)
                else
                    printfn "EXECUTE ERROR: Could not include '%A' | %d | '%s'!" event status resp
                    (false, updated_repo)
        else
            (can_continue, updated_repo)

    Set.foldBack update relations (true, state)

// Attempts to unlock all the given events
let unlock_all (events: Set<EventName>) (send_func: SendFunc<Repository>)
        (state: Repository) : bool * Repository=
    // Try to unlock as much as possible, even if it fails
    let unlock event (succesful, unlocked_repo) =
        let (new_repo, resp, status) = send (Unlock(event)) send_func unlocked_repo
        if succesful && (check_if_positive status) then
            (true, new_repo)
        else
            (false, new_repo)

    Set.foldBack unlock events (true, state)

/// Executes and returns the given event if the given user has the required role
let execute (eventName : EventName) (userName : UserName)
        (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    printfn "EXECUTE: Executing %A!" eventName
    match check_if_executable eventName userName sendFunc repository with
    | ReadResult.Ok(executable_state) ->
        let event_result = get_event eventName repository
        match event_result, executable_state with
        | ReadResult.Ok(event, _), ExecutableState.Executable ->
            // Find out which events need to be locked before executing this one
            printfn "EXECUTE: ToRelations: %A" event.toRelations
            printfn "EXECUTE: FromRelations: %A" event.fromRelations
            let onlyNecessary x =
              let typ, _ = x
              typ = Condition || typ = Exclusion
            let setSplit acc x =
                let _, event = x
                Set.add event acc
            let necessary_from_relations = Set.filter onlyNecessary event.fromRelations
            let necessary_relations = Set.union necessary_from_relations event.toRelations
            let all_events_to_lock = Set.fold setSplit Set.empty necessary_relations
            // NOTE: Remove this event from the lists :p
            let events_to_lock = Set.remove eventName all_events_to_lock
            let (succesfully_locked, locked_state) = lock_all events_to_lock sendFunc repository
            if succesfully_locked then
                let (succesfully_updated, updated_repo) =
                    update_all event.toRelations sendFunc locked_state
                if succesfully_updated then
                    let (succesfully_unlocked, unlocked_repo) =
                        unlock_all events_to_lock sendFunc updated_repo
                    if succesfully_unlocked then
                        // Log this shit!
                        let (logged_repo, resp, status) =
                            send (Log(eventName, DateTime.Now, userName)) sendFunc unlocked_repo
                        let inner (event : Event) : UpdateEventResult =
                            EventOk({event with executed = true; pending = false})
                        update_inner_event eventName inner logged_repo
                    else Error("ERROR: It's not possible to unlock all events!!!")
                else Error("ERROR: It's not possible to change all the states!!!")
            else LockConflict

        | ReadResult.NotFound(error), _ ->
            match error with
            | NotFoundError.Workflow ->
                Result.MissingWorkflow
            | NotFoundError.Event ->
                Result.MissingEvent

        | _, ExecutableState.NotExecutable ->
            Result.NotExecutable

        | _, ExecutableState.Unauthorized ->
            Result.Unauthorized

        | _, ExecutableState.Locked ->
            Result.LockConflict

    | NotFound(error) ->
        match error with
        | NotFoundError.Workflow ->
            Result.MissingWorkflow
        | NotFoundError.Event ->
            Result.MissingEvent

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
    let (state, resp, status) = send (RemoveFromRelation(toEventName, relations, fromEvent)) sendFunc repository
    if check_if_positive status then
        let inner (event : Event) : UpdateEventResult =
            if Set.contains (relations, toEventName) event.toRelations
            then EventOk({event with toRelations = Set.remove (relations, toEventName) event.toRelations})
            else EventErr(MissingRelation)
        update_inner_event fromEvent inner state
    else LockConflict

/// Removes given relation form given event and returns the result
let remove_relation_from (fromEvent : EventName) (relations : RelationType) (toEvent : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        if Set.contains (relations, fromEvent) event.toRelations
        then EventOk({event with toRelations = Set.remove (relations, fromEvent) event.toRelations})
        else EventErr(MissingRelation)
    update_inner_event toEvent inner repository

/// Attempts to delete the given event and returns whether it was succesful
let delete_event (eventName : EventName) (sendFunc : SendFunc<Repository>) (repository : Repository) : Result =
    //Dummy
    let workflow, eName = eventName
    match get_event eventName repository with
    | ReadResult.Ok((event, false)) ->
        printfn "Removing event '%s' from workflow '%s'..." eName workflow

        let remove_to_relation (reltype, target) (succesful, removed_state) =
            let (new_state, resp, status) = send (RemoveToRelation(eventName, reltype, target)) sendFunc removed_state
            if succesful && (check_if_positive status) then (true, new_state)
            else (false, new_state)

        let (succesfully_removed_to_relations, removed_to_state) =
            Set.foldBack remove_to_relation event.toRelations (true, repository)

        if succesfully_removed_to_relations then

            let remove_from_relation (reltype, target) (succesful, removed_state) =
                let (new_state, resp, status) = send (RemoveFromRelation(eventName, reltype, target)) sendFunc removed_state
                if succesful && (check_if_positive status) then (true, new_state)
                else (false, new_state)

            let (succesfully_removed_from_relations, removed_state) =
                Set.foldBack remove_from_relation event.fromRelations (true, removed_to_state)

            if succesfully_removed_from_relations then
                let workflow, name = eventName

                let inner (x : Map<string, bool*Event>) : UpdateMapResult =
                    match Map.tryFind name x with
                    | Some(_)   -> MapOk(Map.remove name x)
                    | None      -> MapErr(MissingEvent)

                update_inner_map workflow inner removed_state
            else LockConflict
        else LockConflict

    | ReadResult.Ok((event, true)) ->
        LockConflict

    | NotFound(error) ->
        match error with
        | NotFoundError.Workflow ->
            Result.MissingWorkflow
        | NotFoundError.Event ->
            Result.MissingEvent


/// Removes given roles form given event and returns the result
let remove_event_roles (eventName : EventName) (roles : Roles) (repository : Repository) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with roles = Set.fold (fun x y -> x.Remove y) event.roles roles})
    update_inner_event eventName inner repository

