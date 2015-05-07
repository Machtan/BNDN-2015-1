module Send

open Pastry
open Repository_types

/// Sends given message with the use of pastery.fs
let send (message : Message) (sendFunc : SendFunc<Repository>) (repository : Repository) : ResourceResponse<Repository> =
    match message with
    | GetIfCondition(eventName) ->
        let workflow, name = eventName
        sendFunc (sprintf "workflow/%s/event/%s/getif" workflow name) "GET" "" repository
    | Lock(eventName)           ->
        let workflow, name = eventName
        sendFunc (sprintf "workflow/%s/event/%s/lock" workflow name) "PUT" "" repository
    | Unlock(eventName)         ->
        let workflow, name = eventName
        sendFunc (sprintf "workflow/%s/event/%s/unlock" workflow name) "PUT" "" repository
    | SetIncluded(eventName, x) ->
        let workflow, name = eventName
        let boo =
            match x with
            | true  -> "true"
            | false -> "false"
        sendFunc (sprintf "workflow/%s/event/%s/included" workflow name) "PUT" boo repository
    | SetPending(eventName, x)  ->
        let workflow, name = eventName
        let boo =
            match x with
            | true  -> "true"
            | false -> "false"
        sendFunc (sprintf "workflow/%s/event/%s/pending" workflow name) "PUT" boo repository
    | GetUserRoles(userName, workflowName)    ->
        sendFunc (sprintf "user/%s/workflow/%s/roles" userName workflowName) "GET" "" repository
    | AddFromRelation(toEventName, relationTyp, fromEventName) ->
        let thisWorkflow, thisEvent = fromEventName
        let toWorkflow, toEvent = toEventName
        let typ = 
            match relationTyp with
            | Condition -> "condition"
            | Exclusion -> "exclusion"
            | Response  -> "response"
            | Inclusion -> "inclusion"
        sendFunc (sprintf "workflow/%s/event/%s/%s?from" thisWorkflow thisEvent typ) "PUT" (sprintf "%s, %s" toWorkflow toEvent) repository
    | RemoveFromRelation(toEventName, relationTyp, fromEventName) ->
        let thisWorkflow, thisEvent = fromEventName
        let toWorkflow, toEvent = toEventName
        let typ = 
            match relationTyp with
            | Condition -> "condition"
            | Exclusion -> "exclusion"
            | Response  -> "response"
            | Inclusion -> "inclusion"
        sendFunc (sprintf "workflow/%s/event/%s/%s?from" thisWorkflow thisEvent typ) "DELETE" (sprintf "%s, %s" toWorkflow toEvent) repository
    | RemoveToRelation(toEventName, relationTyp, fromEventName) ->
        let thisWorkflow, thisEvent = fromEventName
        let toWorkflow, toEvent = toEventName
        let typ = 
            match relationTyp with
            | Condition -> "condition"
            | Exclusion -> "exclusion"
            | Response  -> "response"
            | Inclusion -> "inclusion"
        sendFunc (sprintf "workflow/%s/event/%s/%s?to" thisWorkflow thisEvent typ) "DELETE" (sprintf "%s, %s" toWorkflow toEvent) repository

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