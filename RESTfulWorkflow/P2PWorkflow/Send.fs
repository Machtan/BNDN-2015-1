module Send

open Repository_types

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
    | AddFromRelation(toEventName, relationTyp, fromEventName) ->
        let thisWorkflow, thisEvent = fromEventName
        let toWorkflow, toEvent = toEventName
        sendFunc (sprintf "%s/%s/%A" thisWorkflow thisEvent relationTyp) "PUT" (sprintf "%s, %s" toWorkflow toEvent) repository
    | RemoveFromRelation(toEventName, relationTyp, fromEventName) ->
        let thisWorkflow, thisEvent = fromEventName
        let toWorkflow, toEvent = toEventName
        sendFunc (sprintf "%s/%s/%A" thisWorkflow thisEvent relationTyp) "DELETE" (sprintf "%s, %s" toWorkflow toEvent) repository
    | RemoveToRelation(toEventName, relationTyp, fromEventName) ->
        // Virker ikke ......
        let thisWorkflow, thisEvent = fromEventName
        let toWorkflow, toEvent = toEventName
        sendFunc (sprintf "%s/%s/%A" thisWorkflow thisEvent relationTyp) "DELETE" (sprintf "%s, %s" toWorkflow toEvent) repository

/// tests if a ResourceResponse is positive
let check_if_positive (response : ResourceResponse<Repository>) : bool =
    let _, _, status = response
    if status >= 200 && status < 300
    then true
    else false

let check_if_positive_bool (response : ResourceResponse<Repository>) : bool =
    if check_if_positive response
    then
        let _, answer, _ = response
        answer = "True"
    else false