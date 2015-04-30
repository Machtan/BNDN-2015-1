module WorkflowOld
open System

// The types of messages that the events can send
type Message =
    // Internal messages
    | Notify of string          // The target is notified that this is executed
                                // Argument is(The name of this event)
    | SetExcluded               // The target event becomes excluded
    | SetIncluded               // The target event becomes included
    | SetPending                // The target event becomes pending
    | AddCondition of string    // The target event is not executable before this one
    | RemoveCondition of string // This event is excluded, so a condition is voided

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
    roles: Set<string>;
    executed: DateTime option;
    included: bool;
    executable: bool;
    pending: bool;
    relations: Set<Relation>;
    conditions: Map<string, bool>; // Whether each condition is fulfilled
    // Ex: {"register": false}
}

type Command = string * Message
type Workflow = Map<string, Event>

// A type indicating the result of an execution attempt
type ExecutionResult =
    | Ok of Workflow
    | Unauthorized
    | NotExecutable
    | MissingEvent of string

// A type indicating the result of an update
type UpdateResult =
    | Ok of Workflow
    | MissingEvent of string

// A type for the result of a get result
type GetResult =
    | Ok of (DateTime option * bool * bool * bool)
    | MissingEvent of string

// Creates a new event
let createEvent eventname roles initialState state =
    if Map.containsKey eventname state
    then
        state
    else
        let (excluded, pending, executed) = initialState
        printfn "CREATED: '%s' with state %A and roles %A." eventname initialState roles
        let n = {
            name        = eventname;
            roles       = Set.ofList roles;
            executed    = if executed then Some DateTime.Now else None;
            included    = not excluded;
            pending     = if (pending && not executed) then true else false;
            executable  = not excluded;
            relations   = Set.empty;
            conditions  = Map.empty;
        }
        Map.add eventname n state

// Adds a relation to a event
let addRelation event relation state =
    printfn "Adding relation '%A' to '%s'..." relation event.name
    let n = { event with relations = Set.add relation event.relations; }
    // Notify dependents
    let cmd =
        match relation with
        | Dependent dep -> Some(dep, AddCondition event.name)
        | _ -> None
    Map.add event.name n state, cmd

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
    let inc = (event.included) && Map.forall (fun _ exec -> exec) cons
    Map.add event.name { event with conditions = cons; executable = inc; } state

// Removes a condition of a event
let removeCondition event con state =
    printfn "Excluding dependency '%s' of '%s'..." con event.name
    let cons = Map.remove con event.conditions
    let inc = (event.included) && Map.forall (fun _ exec -> exec) cons
    Map.add event.name { event with conditions = cons; executable = inc; } state

// Tries to execute a event
let tryExecuteInternal event role state : (ExecutionResult * Command list) =
    printfn "Executing '%s'..." event.name
    if (event.roles = Set.empty) || (Set.contains role event.roles)
    then
        if (event.executable && event.included)
        then
            let event' = {
                event with
                    executed = Some DateTime.Now;
                    pending = false;
            }
            let state' = Map.add event.name event' state
            let getNotificationCommands relations commands =
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
            ExecutionResult.Ok state', getNotificationCommands event.relations []
        else
            NotExecutable, []
    else
        printfn "The role '%s' does not have permission to execute '%s'" role event.name
        Unauthorized, []

// Sets the event to be excluded
let setExcluded event state cmds =
    // Don't do anything if it's already included
    if  not event.included then
        (state, cmds)
    else
        // Update the event
        let event' = { event with executable = false; included = false;}
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
    // Don't do anything if it's already included
    if event.included then
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
        let event' = { event with executable = inc; included = true; }
        let state' = Map.add event.name event' state
        (state', notify event.executed.IsSome event.relations cmds)

// Sends a message to the simulation state
let rec tryUpdate (state: Workflow) (cmds: Command list) : UpdateResult =
    match cmds with
    | [] -> UpdateResult.Ok state
    | (eventname, msg)::remainder ->
        match Map.tryFind eventname state with
        | Some event ->
            let state', cmds' =
                match msg with
                | Notify executed ->
                    notifyDependent event executed state, remainder
                | SetExcluded ->
                    setExcluded event state remainder
                | SetIncluded ->
                    setIncluded event state remainder
                | SetPending ->
                    setPending event state, remainder
                | AddCondition con ->
                    addCondition event con state, remainder
                | RemoveCondition con ->
                    removeCondition event con state, remainder
            tryUpdate state' cmds'
        | None ->
            printfn "Event not found while sending message: '%s'" eventname
            UpdateResult.MissingEvent eventname


// Interface: These functions below, and the 'Relation' type
// Prints the status of all events in the state (sorta)
let showWorkflow (state: Workflow) =
    printfn "============= Status =============="
    Map.iter (
        fun k v ->
            printfn "%s %s %s %s" (if v.executable then "->" else "| ") (if v.executed.IsSome then "x" else " ") k (if v.pending then "!" else "")
    ) state

// Creates a new event
let create (eventname: string) (roles: string list) (initialState: bool*bool*bool) ( state: Workflow) =
    createEvent eventname roles initialState state

// Adds a relation to an event
let tryAdd (eventname: string) (relation: Relation) (state: Workflow) =
    match Map.tryFind eventname state with
    | Some event ->
        match addRelation event relation state with
        | state', Some cmd -> tryUpdate state' [cmd]
        | state', None -> UpdateResult.Ok state'
    | None -> UpdateResult.MissingEvent eventname

// Attempts to execute an event
let tryExecute (eventname: string) (role: string) (state: Workflow) =
    match Map.tryFind eventname state with
    | Some(event) ->
        match tryExecuteInternal event role state with
        | ExecutionResult.Ok state', cmds ->
            match tryUpdate state' cmds with
            | UpdateResult.Ok updatedState -> ExecutionResult.Ok updatedState
            | UpdateResult.MissingEvent e -> ExecutionResult.MissingEvent e
        | err, _ -> err
    | None -> ExecutionResult.MissingEvent eventname

// Attempts to get information about an event
let tryGet (eventname: string) (state: Workflow) =
    match Map.tryFind eventname state with
    | Some event ->
        GetResult.Ok (event.executed, event.included, event.executable, event.pending)
    | None ->
        GetResult.MissingEvent eventname

// Returns the names of the nodes as a string
let getEventNames (role: string) (state: Workflow) =
    let events = Map.filter (fun _ v -> (v.roles = Set.empty) || (Set.contains role v.roles)) state
    Map.fold (fun keys key _ -> key::keys) [] events
