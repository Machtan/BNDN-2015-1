module Send

open Repository_types

type GUID = uint64*uint64

type Message =
| Forward                   of GUID*GUID*string*string  //origin, destination, url, data
| GetStatus                 of EventName                // workflow, event  (executed, included)
| Lock                      of EventName
| Unlock                    of EventName
| UpdateLog                 of string                   // new log entry
| UpdateWorkflowLog         of WorkflowName*string      // workflow, new log entry
| SetExcluded               of EventName                // The target event becomes excluded
| SetIncluded               of EventName                // The target event becomes included
| SetPending                of EventName                // The target event becomes pending
| FindExecutableWithRoles   of WorkflowName*string list // workflow, roles

/// Sends given message with the use of pastery.fs
let send (message : Message) : unit =
    failwith "Not implemented yed."

