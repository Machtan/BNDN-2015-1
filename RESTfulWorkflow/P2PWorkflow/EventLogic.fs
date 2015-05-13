module EventLogic

open System
open Pastry
open Repository_types
open Send

type UpdateMapResult =
| MapOk of Map<string, bool*Event>
| MapErr of Result

// SendResult constructor
let send_result<'a> (result: 'a) (state: PastryState<Repository>) =
    { result = result; state = state; }

let update_inner_map (workflow : WorkflowName)
        (func : Map<string, bool*Event> -> UpdateMapResult)
        (state : PastryState<Repository>) : Result =
    let inner_map =
        match Map.tryFind workflow state.data.events with
        | Some(map) ->
            map
        | None ->
            Map.empty
    match func inner_map with
    | MapOk(newMap) ->
        let new_repo = { state.data with events = Map.add workflow newMap state.data.events}
        Ok({state with data = new_repo;})
    | MapErr(error) ->
        error

type UpdateEventResult =
| EventOk of Event
| EventErr of Result

let update_inner_event (eventName: EventName) (func: Event -> UpdateEventResult)
        (state: PastryState<Repository>): Result =
    let workflow, name = eventName
    let inner (eventMap : Map<string, bool*Event>) : UpdateMapResult =
        match Map.tryFind name eventMap with
        | Some(lockEvent)  ->
            let lock, event = lockEvent
            match func event with
            | EventOk(newEvent) ->
                MapOk(Map.add name (lock, newEvent) eventMap)
            | EventErr(errer) ->
                MapErr(errer)
        | None ->
            MapErr(MissingEvent(state))

    update_inner_map workflow inner state

// Attempts to get an event in the repository
let get_event (eventName: EventName) (state: PastryState<Repository>)
        : ReadResult<Event * bool> =
    let workflow, name = eventName
    match state.data.events.TryFind workflow with
        | Some(x) ->
            match x.TryFind name with
            | Some((locked, event)) ->
                ReadResult.Ok(event, locked)
            | None ->
                NotFound(NotFoundError.Event)
        | None ->
            NotFound(NotFoundError.Workflow)

/// Creates and returns a new event from a name and a start state
let create_event (eventName : EventName) (event_state : EventState)
        (roles: string list) (locked : bool) (state : PastryState<Repository>) : Result =
    let event: Event = {
        name        = eventName;
        executed    = event_state.executed;
        included    = event_state.included;
        pending     = event_state.pending;
        toRelations = Set.empty;
        fromRelations = Set.empty;
        roles       = Set.ofList roles;
    }
    let workflow, name = eventName

    // TODO Maybe check whether the workflow exists somewhere
    // before adding events
    let modify x =
        MapOk(Map.add name (locked, event) x)

    update_inner_map workflow modify state

/// Check if given event have one of given roles
let check_roles (eventName : EventName) (user_roles : Roles)
        (state: PastryState<Repository>) : ReadResult<bool> =
    match get_event eventName state with
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
        (sendFunc : SendFunc<Repository>) (state: PastryState<Repository>)
        : SendResult<ReadResult<ExecutableState>> =
    let workflow_name, _ = eventName
    //printfn "EXECHECK: Checking if %A is executable (wf: %s)" eventName workflow_name
    // Check if the user can actually execute this
    let result = send (GetUserRoles(user, workflow_name)) sendFunc state
    let user_found = check_if_positive result.status
    let roles =
        if user_found then
            Set.ofArray (result.message.Split ',')
        else
            printfn "EXECHECK: User '%s' not found: '%s'" user result.message
            Set.empty
    let event_result = get_event eventName result.state
    match event_result with
    | ReadResult.Ok((event, false)) ->
        let user_has_permission = not (Set.isEmpty <| Set.intersect roles event.roles)

        match event.included, user_has_permission with
        | true, true ->
            let is_condition (typ, _) =
                typ = Condition

            let check_condition (typ, from_event) (succesful, new_state) =
                let cond_resp = send (GetIfCondition(from_event)) sendFunc new_state
                let condition_fulfilled = check_if_positive_bool cond_resp.message cond_resp.status
                if succesful && condition_fulfilled then
                    (true, cond_resp.state)
                else
                    (false, cond_resp.state)

            let fromRelations = Set.filter is_condition event.fromRelations
            let conditions_fulfilled, new_state =
                Set.foldBack check_condition fromRelations (true, result.state)
            if conditions_fulfilled then
                send_result <| ReadResult.Ok(ExecutableState.Executable) <| new_state
            else
                send_result <| ReadResult.Ok(ExecutableState.NotExecutable) <| new_state
        | true, false ->
            send_result <| ReadResult.Ok(ExecutableState.Unauthorized) <| result.state
        | false, _ ->
            send_result <| ReadResult.Ok(ExecutableState.NotExecutable) <| result.state

    | ReadResult.Ok((_, true)) ->
        send_result <| ReadResult.Ok(ExecutableState.Locked) <| result.state

    | NotFound(err) ->
        send_result <| NotFound(err) <| result.state

/// Checks if given event is lock'et
let check_if_locked (eventName : EventName) (state : PastryState<Repository>)
        : ReadResult<bool> =
    match get_event eventName state with
    | ReadResult.Ok((_, locked)) ->
        ReadResult.Ok(locked)
    | NotFound(error) ->
        NotFound(error)

/// Checks if given event is executed / excluded
// (whether the condition is fulfilled)
let check_condition (eventName : EventName) (state : PastryState<Repository>)
        : ReadResult<bool> =
    match get_event eventName state with
    | ReadResult.Ok((event, _)) ->
        ReadResult.Ok(event.executed || (not event.included))
    | NotFound(error) ->
        NotFound(error)

// Prints the lock state of the given repository
let print_lock_state (state: PastryState<Repository>) =
    let check_folder workflow map statelist =
        let inner_folder event (locked, _) statelist =
            if locked then (workflow, event)::statelist
            else statelist
        Map.foldBack inner_folder map statelist
    let event_state = Map.foldBack check_folder state.data.events []
    printfn "LOCK STATE: %A" event_state

// Try to lock them all
let lock_all (events_to_lock: Set<EventName>) (sendFunc: SendFunc<Repository>)
        (state: PastryState<Repository>) : bool * PastryState<Repository> =
    let inner event (can_continue, locked_events, locked_state) =
        if can_continue then
            printfn "Lock before send:"
            print_lock_state locked_state
            let result = send (Lock(event)) sendFunc locked_state
            printfn "Lock after send:"
            print_lock_state result.state
            if check_if_positive result.status
            then
                (true, Set.add event locked_events, result.state)
            else
                printfn "LOCK ERROR: Could not lock '%A' | %d | '%s'!" event result.status result.message
                (false, locked_events, locked_state)
        else
            (can_continue, locked_events, locked_state)

    // TODO: Send this status further
    let (succesfully_locked, unlockSet, updated_state) =
        Set.foldBack inner events_to_lock (true, Set.empty, state)
    printfn "LOCK ALL RESULT: %A -> %A" succesfully_locked unlockSet
    print_lock_state updated_state
    if succesfully_locked then
        true, updated_state
    else
        let unlock event (succesful, unlocked_state) =
            let result = send (Unlock(event)) sendFunc unlocked_state
            if succesful && (check_if_positive result.status) then
                true, result.state
            else
                false, result.state
        let (unlock_succesful, unlocked_state) =
            Set.foldBack unlock unlockSet (true, updated_state)
        if unlock_succesful then
            false, unlocked_state
        else
            printfn "LOCK ERROR: Could not remove all locks after failed execution"
            false, unlocked_state

// Updates the state of the given event. Used for a folding loop in execute
let update_all (relations: Set<Relation>) (send_func: SendFunc<Repository>)
        (state: PastryState<Repository>) : bool * PastryState<Repository> =

    let update (reltype, event) (can_continue, new_state)
            : bool * PastryState<Repository> =
        printfn "EXECUTE: Updating %A to %A (continue: %A)..." reltype event can_continue
        if can_continue then
            match reltype with
            | Condition -> // We check backwards, so nothing here
                (can_continue, new_state)
            | Exclusion ->
                let result = send (SetIncluded(event, false)) send_func new_state
                if check_if_positive result.status
                then (true, result.state)
                else
                    printfn "EXECUTE ERROR: Could not exclude '%A' | %d | '%s'!" event result.status result.message
                    (false, result.state)
            | Response  ->
                let result = send (SetPending(event, true)) send_func new_state
                if check_if_positive result.status
                then (true, result.state)
                else
                    printfn "EXECUTE ERROR: Could not notify '%A' | %d | '%s'!" event result.status result.message
                    (false, result.state)
            | Inclusion ->
                let result = send (SetIncluded(event, true)) send_func new_state
                if check_if_positive result.status
                then (true, result.state)
                else
                    printfn "EXECUTE ERROR: Could not include '%A' | %d | '%s'!" event result.status result.message
                    (false, result.state)
        else
            (can_continue, new_state)

    Set.foldBack update relations (true, state)

// Attempts to unlock all the given events
let unlock_all (events: Set<EventName>) (send_func: SendFunc<Repository>)
        (state: PastryState<Repository>) : bool * PastryState<Repository> =
    // Try to unlock as much as possible, even if it fails
    let unlock event (succesful, new_state) =
        let result = send (Unlock(event)) send_func new_state
        if succesful && (check_if_positive result.status) then
            (true, result.state)
        else
            printfn "UNLOCK ERROR: Could not unlock '%A' | %d | '%s'" event result.status result.message
            (false, result.state)

    Set.foldBack unlock events (true, state)

/// Executes and returns the given event if the given user has the required role
let execute (eventName : EventName) (userName : UserName)
        (sendFunc : SendFunc<Repository>) (state : PastryState<Repository>)
        : Result =
    printfn ""
    printfn "%s" (String.replicate 50 "=")
    printfn "EXECUTE: Executing %A!" eventName
    printfn "%s" (String.replicate 50 "=")
    printfn ""
    let result = check_if_executable eventName userName sendFunc state
    match result.result with
    | ReadResult.Ok(executable_state) ->
        let event_result = get_event eventName result.state
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
            let (succesfully_locked, locked_state) =
                lock_all events_to_lock sendFunc result.state
            if succesfully_locked then
                let (succesfully_updated, updated_state) =
                    update_all event.toRelations sendFunc locked_state
                if succesfully_updated then
                    let (succesfully_unlocked, unlocked_state) =
                        unlock_all events_to_lock sendFunc updated_state
                    if succesfully_unlocked then
                        // Log this shit!
                        let end_result = send (Log(eventName, DateTime.Now, userName)) sendFunc unlocked_state
                        printfn ""
                        printfn "%s" (String.replicate 50 "=")
                        printfn "EXECUTE: Execution finished!"
                        printfn "%s" (String.replicate 50 "=")
                        printfn ""
                        let inner (event : Event) : UpdateEventResult =
                            EventOk({event with executed = true; pending = false})
                        update_inner_event eventName inner end_result.state
                    else
                        Error("ERROR: It's not possible to unlock all events!!!", unlocked_state)
                else
                    Error("ERROR: It's not possible to change all the states!!!", updated_state)
            else
                LockConflict(locked_state)

        | ReadResult.NotFound(error), _ ->
            match error with
            | NotFoundError.Workflow ->
                Result.MissingWorkflow(result.state)
            | NotFoundError.Event ->
                Result.MissingEvent(result.state)

        | _, ExecutableState.NotExecutable ->
            Result.NotExecutable(result.state)

        | _, ExecutableState.Unauthorized ->
            Result.Unauthorized(result.state)

        | _, ExecutableState.Locked ->
            Result.LockConflict(result.state)

    | NotFound(error) ->
        match error with
        | NotFoundError.Workflow ->
            Result.MissingWorkflow(result.state)
        | NotFoundError.Event ->
            Result.MissingEvent(result.state)

/// Adds given roles to given event and returns the result
let add_event_roles (eventName : EventName) (roles : Roles)
        (state : PastryState<Repository>) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with roles = Set.fold (fun x y -> x.Add y) event.roles roles})
    update_inner_event eventName inner state

/// Adds given relationships (going from first given event) to given event and returns the result
let add_relation_to (fromEvent : EventName) (relations : RelationType)
        (toEvent : EventName) (sendFunc : SendFunc<Repository>)
        (state : PastryState<Repository>) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with toRelations = Set.add (relations, toEvent) event.toRelations})
    update_inner_event fromEvent inner state

/// Adds given relationships (going to given event) to given event and returns the result
let add_relation_from (fromEvent : EventName) (relations : RelationType)
        (toEventName : EventName) (state : PastryState<Repository>) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with fromRelations = Set.add (relations, fromEvent) event.fromRelations})
    update_inner_event toEventName inner state

/// Chance state
let set_included (eventName : EventName) (newState : bool)
        (state : PastryState<Repository>) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with included = newState})
    update_inner_event eventName inner state

let set_pending (eventName : EventName) (newState : bool)
        (state : PastryState<Repository>) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with pending = newState})
    update_inner_event eventName inner state

let set_executed (eventName : EventName) (newState : bool)
        (state : PastryState<Repository>) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with executed = newState})
    update_inner_event eventName inner state

/// lock given event
let lock_event (eventName: EventName) (state: PastryState<Repository>)
        : Result =
    let workflow, name = eventName
    let inner (x : Map<string, bool*Event>) : UpdateMapResult =
        match Map.tryFind name x with
        | Some((locked, event)) ->
            if not locked then
                MapOk(Map.add name (true, event) x)
            else
                MapErr(LockConflict(state))
        | None ->
            MapErr(MissingEvent(state))

    update_inner_map workflow inner state

/// unlock given event
let unlock_event (eventName : EventName) (state : PastryState<Repository>)
        : Result =
    let workflow, name = eventName
    let inner (x : Map<string, bool*Event>) : UpdateMapResult =
        match Map.tryFind name x with
        | Some((locked, event)) ->
            if locked then
                MapOk(Map.add name (false, event) x)
            else
                MapErr(LockConflict(state))
        | None ->
            MapErr(MissingEvent(state))

    update_inner_map workflow inner state

/// Removes given relation form given event and returns the result
let remove_relation_to (fromEvent : EventName) (relations : RelationType)
        (toEvent : EventName) (sendFunc : SendFunc<Repository>)
        (state: PastryState<Repository>) : Result =
    let inner (event : Event) : UpdateEventResult =
        if Set.contains (relations, toEvent) event.toRelations then
            let to_relations = Set.remove (relations, toEvent) event.toRelations
            EventOk({event with toRelations = to_relations; })
        else
            EventErr(MissingRelation(state))
    update_inner_event fromEvent inner state

/// Removes given relation form given event and returns the result
let remove_relation_from (fromEvent : EventName) (relations : RelationType)
        (toEvent : EventName) (sendFunc : SendFunc<Repository>)
        (state : PastryState<Repository>) : Result =
    let inner (event : Event) : UpdateEventResult =
        if Set.contains (relations, fromEvent) event.toRelations
        then
            let from_relations = Set.remove (relations, fromEvent) event.fromRelations
            EventOk({event with fromRelations = from_relations; })
        else
            EventErr(MissingRelation(state))
    update_inner_event toEvent inner state

/// Attempts to delete the given event and returns whether it was succesful
let delete_event (eventName : EventName) (sendFunc : SendFunc<Repository>)
        (state : PastryState<Repository>) : Result =
    //Dummy
    let workflow, eName = eventName
    match get_event eventName state with
    | ReadResult.Ok((event, false)) ->
        printfn "Removing event '%s' from workflow '%s'..." eName workflow

        // Continues despite failing: TODO: change
        let remove_to_relation (reltype, target) (succesful, new_state) =
            let cmd = RemoveFromRelation(eventName, reltype, target)
            let result = send cmd sendFunc new_state
            if succesful && (check_if_positive result.status) then
                (true, result.state)
            else
                (false, result.state)

        let (succesfully_removed_to_relations, removed_to_state) =
            Set.foldBack remove_to_relation event.toRelations (true, state)

        if succesfully_removed_to_relations then

            // Continues despite failing: TODO: change
            let remove_from_relation (reltype, target) (succesful, new_state) =
                let cmd = RemoveToRelation(target, reltype, eventName)
                let result = send cmd sendFunc new_state
                if succesful && (check_if_positive result.status) then
                    (true, result.state)
                else
                    (false, result.state)

            let (succesfully_removed_from_relations, removed_state) =
                Set.foldBack remove_from_relation event.fromRelations (true, removed_to_state)

            if succesfully_removed_from_relations then
                let workflow, name = eventName

                let inner (x : Map<string, bool*Event>) : UpdateMapResult =
                    match Map.tryFind name x with
                    | Some(_) ->
                        MapOk(Map.remove name x)
                    | None ->
                        MapErr(MissingEvent(removed_state))

                update_inner_map workflow inner removed_state
            else
                LockConflict(removed_state)
        else
            LockConflict(removed_to_state)

    | ReadResult.Ok((event, true)) ->
        LockConflict(state)

    | NotFound(error) ->
        match error with
        | NotFoundError.Workflow ->
            Result.MissingWorkflow(state)
        | NotFoundError.Event ->
            Result.MissingEvent(state)

/// Removes given roles form given event and returns the result
let remove_event_roles (eventName : EventName) (roles : Roles)
        (state : PastryState<Repository>) : Result =
    let inner (event : Event) : UpdateEventResult =
        EventOk({event with roles = Set.fold (fun x y -> x.Remove y) event.roles roles})
    update_inner_event eventName inner state
