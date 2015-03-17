module Workflow
// The types of relations the events can know of
type Relation =             // The internal string is a http address
    | Dependent of string   // What is enabled once a event is executed
    | Exclusion of string   // What becomes excluded once this event is executed
    | Response of string    // What becomes pending once a event is executed
    | Inclusion of string   // What gets included once this event is executed

// The record type for an event event.
// We only really use this as the state singleton (which is nice, tho)
type Event = {
    name: string;
    executed: bool;
    excluded: bool;
    included: bool;
    pending: bool;
    relations: Relation list;
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
    | AddCondition of string    // The target event is not included before this one
    | RemoveCondition of string // This event is excluded, so a condition is voided

    // External messages
    | Execute                   // *new* tries to execute a event
    | Create                    // *new* creates a event
    | AddRelation of Relation   // *new* adds a relation

type Command = string * Message
type Process = Map<string, Event>

// Creates a new event
let createEvent event state =
    printfn "Creating '%s'..." event
    let n = {
        name = event;
        executed = false;
        excluded = false;
        included = true;
        pending = false;
        relations = [];
        conditions = Map.empty;
    }
    (Map.add event n state)

// Adds a relation to a event
let addRelation event relation state cmds =
    printfn "Adding relation '%A' to '%s'..." relation event.name
    let n = { event with relations = relation::event.relations; }
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
    let n = { event with conditions = cons; included = false; }
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
    Map.add event.name { event with conditions = cons; included = inc; } state

// Removes a condition of a event
let removeCondition event con state =
    printfn "Excluding dependency '%s' of '%s'..." con event.name
    let cons = Map.remove con event.conditions
    let inc = (not event.excluded) && Map.forall (fun _ exec -> exec) cons
    Map.add event.name { event with conditions = cons; included = inc; } state

// Tries to execute a event
let tryExecuteInternal event state cmds =
    printfn "Executing '%s'..." event.name
    if not event.included
    then
        printfn "The event '%s' was attempted executed, but is not included!" event.name
        None
    else
        let event' = { event with executed = true; pending = false; }
        let state' = Map.add event.name event' state
        let rec notify relations commands =
            match relations with
            | [] -> commands
            | rel::remainder ->
                let commands' =
                    match rel with
                    | Dependent dst -> (dst, Notify event.name)::commands
                    | Exclusion dst -> (dst, SetExcluded)::commands
                    | Response  dst -> (dst, SetPending)::commands
                    | Inclusion dst -> (dst, SetIncluded)::commands
                notify remainder commands'

        Some (state', notify event.relations cmds)

// Sets the event to be excluded
let setExcluded event state cmds =
    // Don't do anything if it's already excluded
    if event.excluded then
        (state, cmds)
    else
        // Update the event
        let event' = { event with included = false; excluded = true;}
        let state' = Map.add event.name event' state

        // Notify all relations of the change
        let rec notify relations commands =
            match relations with
            | [] -> commands
            | rel::remainder ->
                let commands' =
                    match rel with
                    | Dependent dst -> (dst, RemoveCondition event.name)::commands
                    | _ -> commands
                notify remainder commands'
        (state', notify event.relations cmds)

// Sets the event to be included
let setIncluded event state (cmds: Command list) =
    // Don't do anything if it's NOT already excluded
    if not event.excluded then
        (state, cmds)
    else
        let rec notify executed relations commands =
            match relations with
            | [] -> commands
            | rel::remainder ->
                let commands' =
                    match rel with
                    | Dependent dst ->
                        // If this command was previously executed,
                        // then update to reflect this
                        let coms =
                            if executed then
                                (dst, Notify event.name)::commands
                            else
                                commands
                        (dst, AddCondition event.name)::coms
                    | _ -> commands
                notify executed remainder commands'

        let inc = Map.forall (fun _ exec -> exec) event.conditions
        let event' = { event with included = inc; excluded = false; }
        let state' = Map.add event.name event' state
        (state', notify event.executed event.relations cmds)

// Sends a message to the simulation state
let sendMessage (state: Process) (cmds: Command list) =
    let rec trySend state cmds =
        match cmds with
        | [] -> Some state
        | (event, msg)::remainder ->
            let res =
                match msg with
                | Create -> Some (createEvent event state, remainder)

                // Check whether the event exists before proceeding
                | _ ->
                    match Map.tryFind event state with
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
                        | Create -> failwith "HOW DID THIS HAPPEN!?"
                    | None ->
                        printfn "The event at '%s' does not exist! Aborting!" event
                        None
            match res with
            | Some (state', cmds') -> trySend state' cmds'
            | None -> None

    trySend state cmds


// Interface: These functions below, and the 'Relation' type
// Prints the status of all events in the state (sorta)
let showProcess (state: Process) =
    printfn "============= Status =============="
    Map.iter (
        fun k v ->
            printfn "%s %s %s %s" (if v.included then "->" else "| ") (if v.executed then "x" else " ") k (if v.pending then "!" else "")
    ) state

// Creates a new event
let create (event: string) (state: Process) =
    match sendMessage state [event, Create] with
    | Some state' -> state'
    | None -> failwith "Error while adding relation (this should not happen)"

// Adds a relation to an event
let tryAdd (event: string) (relation: Relation) (state: Process) =
    sendMessage state [event, AddRelation relation]

// Attempts to execute an event
let tryExecute (event: string) (state: Process) =
    sendMessage state [event, Execute]

// Attempts to get information about an event
let tryGet (event: string) (state: Process) =
    match Map.tryFind event state with
    | Some event -> Some (event.executed, event.included, event.pending)
    | None -> None