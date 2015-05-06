module Send

open Repository_types

type Message =
| GetStatus                 of EventName                // workflow, event  (executed, included)
| GetIfCondition            of EventName
| Lock                      of EventName
| Unlock                    of EventName
| SetExcluded               of EventName * bool                // The target event becomes excluded
| SetIncluded               of EventName * bool               // The target event becomes included
| SetPending                of EventName * bool                // The target event becomes pending

/// Sends given message with the use of pastery.fs
val send: Message -> SendFunc<Repository> -> Repository -> ResourceResponse<Repository>

/// tests if a ResourceResponse is positive http
val check_if_positive: ResourceResponse<Repository> -> bool

/// tests if a ResourceResponse is positive bool
val check_if_positive_bool: ResourceResponse<Repository> -> bool