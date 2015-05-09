module Send

open Pastry
open Repository_types

/// Sends given message with the use of pastery.fs
let send (message : Message) (sendFunc : SendFunc<Repository>) (repository : Repository) : ResourceResponse<Repository> =
    match message with
    | GetIfCondition(eventName) ->
        let workflow, name = eventName
        let url = sprintf "workflow/%s/%s/getif" workflow name
        sendFunc url "GET" "" repository

    | Lock(eventName) ->
        let workflow, name = eventName
        let url = sprintf "workflow/%s/%s/lock" workflow name
        sendFunc url "PUT" "" repository

    | Unlock(eventName) ->
        let workflow, name = eventName
        let url = sprintf "workflow/%s/%s/unlock" workflow name
        sendFunc url "PUT" "" repository

    | SetIncluded(eventName, x) ->
        let workflow, name = eventName
        let boo =
            match x with
            | true  -> "true"
            | false -> "false"
        let url = sprintf "workflow/%s/%s/included" workflow name
        sendFunc url "PUT" boo repository

    | SetPending(eventName, x) ->
        let workflow, name = eventName
        let boo =
            match x with
            | true  -> "true"
            | false -> "false"
        let url = sprintf "workflow/%s/%s/pending" workflow name
        sendFunc url "PUT" boo repository

    | GetUserRoles(userName, workflowName)    ->
        sendFunc (sprintf "user/%s/workflow/%s/roles" userName workflowName) "GET" "" repository

    | AddFromRelation(fromEvent, relationTyp, toEvent) ->
        let from_wf, from_event = fromEvent
        let to_wf, to_event = toEvent
        let typ =
            match relationTyp with
            | Condition -> "condition"
            | Exclusion -> "exclusion"
            | Response  -> "response"
            | Inclusion -> "inclusion"
        let data = sprintf "%s,%s" from_wf from_event
        let url = sprintf "workflow/%s/%s/%s/from" to_wf to_event typ
        sendFunc url "PUT" data repository

    | RemoveFromRelation(fromEvent, relationTyp, toEvent) ->
        let from_wf, from_event = fromEvent
        let to_wf, to_event = toEvent
        let typ =
            match relationTyp with
            | Condition -> "condition"
            | Exclusion -> "exclusion"
            | Response  -> "response"
            | Inclusion -> "inclusion"
        let data = sprintf "%s,%s" from_wf from_event
        let url = sprintf "workflow/%s/%s/%s/from" to_wf to_event typ
        sendFunc url "DELETE" data repository

    | RemoveToRelation(fromEvent, relationTyp, toEvent) ->
        let from_wf, from_event = fromEvent
        let to_wf, to_event = toEvent
        let typ =
            match relationTyp with
            | Condition -> "condition"
            | Exclusion -> "exclusion"
            | Response  -> "response"
            | Inclusion -> "inclusion"
        let data = sprintf "%s,%s" to_wf to_event
        let url = sprintf "workflow/%s/%s/%s/to" from_wf from_event typ
        sendFunc url "DELETE" data repository

/// tests if a ResourceResponse is positive
let check_if_positive (status: int) : bool =
    if status >= 200 && status < 300
    then true
    else false

let check_if_positive_bool (resp: string) (status: int): bool =
    if check_if_positive status
    then resp = "True"
    else false