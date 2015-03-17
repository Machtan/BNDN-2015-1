module Workflow

type Event
type Relation =             // The internal string is a http address
    | Dependent of string   // What is enabled once a node is executed
    | Exclusion of string   // What becomes excluded once this node is executed
    | Response of string    // What becomes pending once a node is executed

val create: string -> Map<string, Event> -> Map<string, Event>
val tryAdd: string -> Relation -> Map<string, Event> -> Map<string, Event> option
val tryExecute: string -> Map<string, Event> -> Map<string, Event> option
val tryGet: string -> Map<string, Event> -> (bool * bool * bool) option
val showProcess: Map<string, Event> -> unit