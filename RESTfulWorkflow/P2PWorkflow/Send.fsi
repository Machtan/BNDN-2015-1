type GUID = uint64*uint64
type WorkflowName = string
type EventName = WorkflowName*string

type Message =
| Forward                   of GUID*GUID*string*string  //origin, destination, url, data
| GetStatus                 of EventName            // workflow, event  (executed, included)
| Lock                      of EventName
| Unlock                    of EventName
| UpdateLog                 of string                   // new log entry
| UpdateWorkflowLog         of WorkflowName*string          // workflow, new log entry
| SetExcluded               of EventName            // The target event becomes excluded
| SetIncluded               of EventName            // The target event becomes included
| SetPending                of EventName            // The target event becomes pending
| FindExecutableWithRoles   of WorkflowName*string list       // workflow, roles

val send: Message -> unit

