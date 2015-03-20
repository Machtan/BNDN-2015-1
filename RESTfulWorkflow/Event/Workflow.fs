module Workflow

// The types of relations the events can know of
type Relation =             // The internal string is a http address
    | Dependent of string   // What is enabled once a event is executed
    | Exclusion of string   // What becomes excluded once this event is executed
    | Response of string    // What becomes pending once a event is executed
    | Inclusion of string   // What gets executable once this event is executed

// The record type for an event event.
// We only really use this as the state singleton (which is nice, tho)
type Event = {
    name: string;
    executed: bool;
    excluded: bool;
    executable: bool;
    pending: bool;
    relations: Set<Relation>;
    conditions: Map<string, bool>; // Whether each condition is fulfilled
    // Ex: {"register": false}
}

// The types of messages that the events can send
type Message =
    // Internal messages
    | Notify of string          // The target is notified that this is executed
                                // Argument is(The name of this event)
    | SetExcluded               // The target event becomes excluded
    | SetIncluded               // The target event becomes excluded
    | SetPending                // The target event becomes pending
    | AddCondition of string    // The target event is not executable before this one
    | RemoveCondition of string // This event is excluded, so a condition is voided

    // External messages
    | Execute                   // *new* tries to execute a event
    | Create                    // *new* creates a event
    | AddRelation of Relation   // *new* adds a relation

type Command = string * Message
type Workflow = Map<string, Event>

// Creates a new event
let createEvent event state cmds =
    if Map.containsKey event state
    then
        None
    else
        printfn "Creating '%s'..." event
        let n = {
            name = event;
            executed = false;
            excluded = false;
            executable = true;
            pending = false;
            relations = Set.empty;
            conditions = Map.empty;
        }
        Some(Map.add event n state, cmds)

// Adds a relation to a event
let addRelation event relation state cmds =
    printfn "Adding relation '%A' to '%s'..." relation event.name
    let n = { event with relations = Set.add relation event.relations; }
    let state' = Map.add event.name n state
    // Notify dependents
    let cmds' =
        match relation with
        | Dependent dep -> (dep, AddCondition event.name)::cmds
        | _ -> cmds
    state', cmds'

// Adds a condition to a event
let addCondition event condition state =
    printfn "Adding condition '%A' to '%s'..." condition event.name
    let cons = Map.add condition false event.conditions
    let n = { event with conditions = cons; executable = false; }
    (Map.add event.name n state)

// Sets the event to be pending
let setPending event state =
    printfn "Setting '%s' to pending..." event.name
    Map.add event.name { event with pending = true; } state

// Tells a event that a dependency has been executed
let notifyDependent event executed state =
    printfn "Updating dependency '%s' of '%s'..." executed event.name
    let cons = Map.add executed true event.conditions
    let inc = (not event.excluded) && Map.forall (fun _ exec -> exec) cons
    Map.add event.name { event with conditions = cons; executable = inc; } state

// Removes a condition of a event
let removeCondition event con state =
    printfn "Excluding dependency '%s' of '%s'..." con event.name
    let cons = Map.remove con event.conditions
    let inc = (not event.excluded) && Map.forall (fun _ exec -> exec) cons
    Map.add event.name { event with conditions = cons; executable = inc; } state

// Tries to execute a event
let tryExecuteInternal event state cmds =
    printfn "Executing '%s'..." event.name
    if not event.executable
    then
        printfn "The event '%s' was attempted executed, but is not executable!" event.name
        None
    else
        let event' = { event with executed = true; pending = false; }
        let state' = Map.add event.name event' state
        let notify relations commands =
            Set.foldBack (
                fun rel cmds ->
                    let cmd =
                        match rel with
                        | Dependent dst -> (dst, Notify event.name)
                        | Exclusion dst -> (dst, SetExcluded)
                        | Response  dst -> (dst, SetPending)
                        | Inclusion dst -> (dst, SetIncluded)
                    cmd::cmds
            ) relations commands

        Some (state', notify event.relations cmds)

// Sets the event to be excluded
let setExcluded event state cmds =
    // Don't do anything if it's already excluded
    if event.excluded then
        (state, cmds)
    else
        // Update the event
        let event' = { event with executable = false; excluded = true;}
        let state' = Map.add event.name event' state

        // Notify all relations of the change
        let notify relations commands =
            Set.foldBack (
                fun rel cmds ->
                    match rel with
                    | Dependent dst -> (dst, RemoveCondition event.name)::cmds
                    | _ -> cmds
            ) relations commands

        (state', notify event.relations cmds)

// Sets the event to be executable
let setIncluded event state (cmds: Command list) =
    // Don't do anything if it's NOT already excluded
    if not event.excluded then
        (state, cmds)
    else
        let rec notify executed relations commands =
            Set.foldBack (
                fun rel cmds ->
                    match rel with
                    | Dependent dst ->
                        let coms =
                            if executed
                            then (dst, Notify event.name)::cmds
                            else cmds
                        (dst, AddCondition event.name)::coms
                    | _ -> cmds
            ) relations commands

        let inc = Map.forall (fun _ exec -> exec) event.conditions
        let event' = { event with executable = inc; excluded = false; }
        let state' = Map.add event.name event' state
        (state', notify event.executed event.relations cmds)

// Sends a message to the simulation state
let sendMessage (state: Workflow) (cmds: Command list) =
    let rec trySend state cmds =
        match cmds with
        | [] -> Some state
        | (eventname, msg)::remainder ->
            let res =
                match msg with
                | Create -> createEvent eventname state remainder

                // Check whether the event exists before proceeding
                | _ ->
                    match Map.tryFind eventname state with
                    | Some event ->
                        match msg with
                        | Notify executed ->
                            Some ((notifyDependent event executed state), remainder)
                        | SetExcluded ->
                            Some (setExcluded event state remainder)
                        | SetIncluded ->
                            Some (setIncluded event state remainder)
                        | SetPending ->
                            Some ((setPending event state), remainder)
                        | AddCondition con ->
                            Some ((addCondition event con state), remainder)
                        | RemoveCondition con ->
                            Some ((removeCondition event con state), remainder)
                        | Execute ->
                            (tryExecuteInternal event state remainder) // This can fail
                        | AddRelation rel ->
                            Some (addRelation event rel state remainder)
                        | Create ->
                            printfn "HOW DID THIS HAPPEN!?"
                            createEvent eventname state remainder
                    | None ->
                        printfn "The event at '%s' does not exist! Aborting!" eventname
                        None
            match res with
            | Some (state', cmds') -> trySend state' cmds'
            | None -> None

    trySend state cmds


// Interface: These functions below, and the 'Relation' type
// Prints the status of all events in the state (sorta)
let showWorkflow (state: Workflow) =
    printfn "============= Status =============="
    Map.iter (
        fun k v ->
            printfn "%s %s %s %s" (if v.executable then "->" else "| ") (if v.executed then "x" else " ") k (if v.pending then "!" else "")
    ) state

// Creates a new event
let create (event: string) (state: Workflow) =
    sendMessage state [event, Create]

// Adds a relation to an event
let tryAdd (event: string) (relation: Relation) (state: Workflow) =
    sendMessage state [event, AddRelation relation]

// Attempts to execute an event
let tryExecute (event: string) (state: Workflow) =
    sendMessage state [event, Execute]

// Attempts to get information about an event
let tryGet (event: string) (state: Workflow) =
    match Map.tryFind event state with
    | Some event -> Some (event.executed, event.executable, event.pending)
    | None -> None

// Returns the status of the nodes as a string
let getNodes (state: Workflow) =
    Map.fold (fun keys key _ -> key::keys) [] state