module Workflow
// The types of relations the nodes can know of
type Relation =             // The internal string is a http address
    | Dependent of string   // What is enabled once a node is executed
    | Exclusion of string   // What becomes excluded once this node is executed
    | Response of string    // What becomes pending once a node is executed

// The record type for an event node.
// We only really use this as the state singleton (which is nice, tho)
type Event = {
    name: string;
    executed: bool;
    included: bool;
    pending: bool;
    relations: Relation list;
    conditions: Map<string, bool>; // Whether each condition is fulfilled
    // Ex: {"register": false}
}

// The types of messages that the nodes can send
type Message =
    // Internal messages
    | Notify of string          // The target is notified that this is executed
                                // Argument is(The name of this node)
    | SetExcluded               // The target node becomes excluded
    | SetPending                // The target node becomes pending
    | AddCondition of string    // The target node is not included before this one
    | RemoveCondition of string // This node is excluded, so a condition is voided

    // External messages
    | Execute                   // *new* tries to execute a node
    | Create                    // *new* creates a node
    | AddRelation of Relation   // *new* adds a relation

type Command = string * Message
type Process = Map<string, Event>

// Creates a new node
let createEvent event state =
    printfn "Creating '%s'..." event
    let n = {
        name = event;
        executed = false;
        included = true;
        pending = false;
        relations = [];
        conditions = Map.empty;
    }
    (Map.add event n state)

// Adds a relation to a node
let addRelation node relation state cmds =
    printfn "Adding relation '%A' to '%s'..." relation node.name
    let n = { node with relations = relation::node.relations; }
    let state' = Map.add node.name n state
    // Notify dependents
    let cmds' =
        match relation with
        | Dependent dep -> (dep, AddCondition node.name)::cmds
        | _ -> cmds
    state', cmds'

// Adds a condition to a node
let addCondition node condition state =
    printfn "Adding condition '%A' to '%s'..." condition node.name
    let cons = Map.add condition false node.conditions
    let n = { node with conditions = cons; included = false; }
    (Map.add node.name n state)

// Sets the node to be pending
let setPending node state =
    printfn "Setting '%s' to pending..." node.name
    Map.add node.name { node with pending = true; } state

// Tells a node that a dependency has been executed
let notifyDependent node executed state =
    printfn "Updating dependency '%s' of '%s'..." executed node.name
    let cons = Map.add executed true node.conditions
    let inc = Map.forall (fun _ exec -> exec) cons
    Map.add node.name { node with conditions = cons; included = inc; } state

// Removes a condition of a node
let removeCondition node con state =
    printfn "Excluding dependency '%s' of '%s'..." con node.name
    let cons = Map.remove con node.conditions
    let inc = Map.forall (fun _ exec -> exec) cons
    Map.add node.name { node with conditions = cons; included = inc; } state

// Tries to execute a node
let tryExecuteInternal node state cmds =
    printfn "Executing '%s'..." node.name
    if not node.included
    then
        printfn "The node '%s' was attempted executed, but is not included!" node.name
        None
    else
        let node' = { node with executed = true; pending = false; }
        let state' = Map.add node.name node' state
        let rec notify relations commands =
            match relations with
            | [] -> commands
            | rel::remainder ->
                let commands' =
                    match rel with
                    | Dependent dst -> (dst, Notify node.name)::commands
                    | Exclusion dst -> (dst, SetExcluded)::commands
                    | Response dst -> (dst, SetPending)::commands
                notify remainder commands'

        Some (state', notify node.relations cmds)

// Sets the node to be excluded
let setExcluded node state cmds =
    let state' = Map.add node.name { node with included = false; } state // TODO more?
    let rec notify relations commands =
        match relations with
        | [] -> commands
        | rel::remainder ->
            let commands' =
                match rel with
                | Dependent dst -> (dst, RemoveCondition node.name)::commands
                | _ -> commands
            notify remainder commands'
    (state', notify node.relations cmds)

// Sends a message to the simulation state
let sendMessage (state: Process) (cmds: Command list) =
    let rec trySend state cmds =
        match cmds with
        | [] -> Some state
        | (event, msg)::remainder ->
            let res =
                match msg with
                | Create -> Some (createEvent event state, remainder)

                // Check whether the node exists before proceeding
                | _ ->
                    match Map.tryFind event state with
                    | Some node ->
                        match msg with
                        | Notify executed ->
                            Some ((notifyDependent node executed state), remainder)
                        | SetExcluded ->
                            Some (setExcluded node state remainder)
                        | SetPending ->
                            Some ((setPending node state), remainder)
                        | AddCondition con ->
                            Some ((addCondition node con state), remainder)
                        | RemoveCondition con ->
                            Some ((removeCondition node con state), remainder)
                        | Execute ->
                            (tryExecuteInternal node state remainder) // This can fail
                        | AddRelation rel ->
                            Some (addRelation node rel state remainder)
                        | Create -> failwith "HOW DID THIS HAPPEN!?"
                    | None ->
                        printfn "The node at '%s' does not exist! Aborting!" event
                        None
            match res with
            | Some (state', cmds') -> trySend state' cmds'
            | None -> None

    trySend state cmds


// Interface: These functions below, and the 'Relation' type
// Prints the status of all nodes in the state (sorta)
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
    | Some node -> Some (node.executed, node.included, node.pending)
    | None -> None