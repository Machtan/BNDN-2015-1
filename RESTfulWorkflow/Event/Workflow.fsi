module Workflow
open System

type Event
type Relation =             // The internal string is a http address
    | Dependent of string   // What is enabled once a node is executed
    | Exclusion of string   // What becomes excluded once this node is executed
    | Response of string    // What becomes pending once a node is executed
    | Inclusion of string   // What gets included once this event is executed

type Workflow = Map<string, Event>

type ExecutionResult =
    | Ok of Workflow
    | Unauthorized
    | NotExecutable
    | MissingEvent of string

type UpdateResult =
    | Ok of Workflow
    | MissingEvent of string

type GetResult =
    | Ok of (DateTime option * bool * bool * bool)
    | MissingEvent of string

val create:         string   -> string    -> Workflow  -> Workflow
val tryAdd:         string   -> Relation  -> Workflow  -> UpdateResult
val tryExecute:     string   -> string    -> Workflow  -> ExecutionResult
val tryGet:         string   -> Workflow  -> GetResult
val getEventNames:  string   -> Workflow  -> string list
val showWorkflow:   Workflow -> unit