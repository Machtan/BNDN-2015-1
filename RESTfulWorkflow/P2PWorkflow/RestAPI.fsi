module Rest
open System
open System.Net
open WorkflowOld

type event = string
type role = string
type eventBody = string  
type included = bool
type executable = bool
type pending = bool
type eventState = DateTime option * included * executable * pending
type httpResponse = string * int * string * eventState option
type httpResponseNoState = string * int * string * Workflow
type restResponse = string * int * string
type typ = string
type dest = string
type url = string

val ProcessRestCall : HttpListenerRequest -> httpResponseNoState

val getEvent : event -> Workflow -> httpResponse

val getExecuted : event -> Workflow -> httpResponseNoState

val getIncluded : event -> Workflow -> httpResponseNoState

val getExecutable : event -> Workflow -> httpResponseNoState

val getPending : event -> Workflow -> httpResponseNoState

val setExecuted : event -> role -> Workflow -> httpResponseNoState

val createEvent : event -> eventBody -> Workflow -> httpResponseNoState

val addRelation : event ->  typ ->  dest ->  Workflow -> httpResponseNoState