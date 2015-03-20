module Workflow

type Event
type Relation =             // The internal string is a http address
    | Dependent of string   // What is enabled once a node is executed
    | Exclusion of string   // What becomes excluded once this node is executed
    | Response of string    // What becomes pending once a node is executed
    | Inclusion of string   // What gets included once this event is executed

type Workflow = Map<string, Event>

val create: string -> Workflow -> Workflow
val tryAdd: string -> Relation -> Workflow -> Workflow option
val tryExecute: string -> Workflow -> Workflow option
val tryGet: string -> Workflow -> (bool * bool * bool) option
val showWorkflow: Workflow -> unit
val getNodes: Workflow -> string list