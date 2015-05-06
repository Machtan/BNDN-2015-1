module Send

open Repository_types

type Message =
| GetStatus                 of EventName                // workflow, event  (executed, included)
| GetIfCondition            of EventName
| Lock                      of EventName
| Unlock                    of EventName
| SetIncluded               of EventName * bool                // The target event becomes included
| SetPending                of EventName * bool                // The target event becomes pending
| GetUserRoles              of UserName

/// Sends given message with the use of pastery.fs
let send (message : Message) (sendFunc : SendFunc<Repository>) (repository : Repository) : ResourceResponse<Repository> =
    match message with
    | GetStatus(eventName)      ->
        let workflow, name = eventName
        sendFunc (sprintf "%s/%s" workflow name) "GET" "" repository
    | GetIfCondition(eventName) ->
        let workflow, name = eventName
        sendFunc (sprintf "%s/%s/condition" workflow name) "GET" "" repository
    | Lock(eventName)           ->
        let workflow, name = eventName
        sendFunc (sprintf "%s/%s/lock" workflow name) "PUT" "" repository
    | Unlock(eventName)         ->
        let workflow, name = eventName
        sendFunc (sprintf "%s/%s/unlock" workflow name) "PUT" "" repository
    | SetIncluded(eventName, x) ->
        let workflow, name = eventName
        sendFunc (sprintf "%s/%s/included" workflow name) "PUT" (sprintf "%A" x) repository
    | SetPending(eventName, x)  ->
        let workflow, name = eventName
        sendFunc (sprintf "%s/%s/pending" workflow name) "PUT" (sprintf "%A" x) repository
    | GetUserRoles(userName)    ->
        failwith "not implemented yed"

/// tests if a ResourceResponse is positive
let check_if_positive (response : ResourceResponse<Repository>) : bool =
    let _, _, status = response
    if status >= 200
        then
            if status < 300
            then true
            else false
        else false

let check_if_positive_bool (response : ResourceResponse<Repository>) : bool =
    if check_if_positive response
    then
        let _, answer, _ = response
        answer = "True"
    else false