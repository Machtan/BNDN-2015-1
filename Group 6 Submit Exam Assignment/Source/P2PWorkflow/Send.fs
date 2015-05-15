module Send

open System
open Pastry
open Repository_types

/// Sends given message with the use of pastery.fs
let send (message : Message) (sendFunc : SendFunc<Repository>)
        (state: PastryState<Repository>) : ResourceResponse<Repository> =
    match message with
    | GetIfCondition(eventName) ->
        let workflow, name = eventName
        let url = sprintf "workflow/%s/%s/getif" workflow name
        sendFunc url "GET" "" state

    | Lock(eventName) ->
        let workflow, name = eventName
        let url = sprintf "workflow/%s/%s/lock" workflow name
        sendFunc url "PUT" "" state

    | Unlock(eventName) ->
        let workflow, name = eventName
        let url = sprintf "workflow/%s/%s/unlock" workflow name
        sendFunc url "PUT" "" state

    | SetIncluded(eventName, x) ->
        let workflow, name = eventName
        let boo =
            match x with
            | true  -> "true"
            | false -> "false"
        let url = sprintf "workflow/%s/%s/included" workflow name
        sendFunc url "PUT" boo state

    | SetPending(eventName, x) ->
        let workflow, name = eventName
        let boo =
            match x with
            | true  -> "true"
            | false -> "false"
        let url = sprintf "workflow/%s/%s/pending" workflow name
        sendFunc url "PUT" boo state

    | GetUserRoles(userName, workflowName)    ->
        sendFunc (sprintf "user/%s/roles/%s" userName workflowName) "GET" "" state

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
        sendFunc url "PUT" data state

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
        sendFunc url "DELETE" data state

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
        sendFunc url "DELETE" data state

    | Log(eventName, dateTime, userName) ->
        let workflow, event = eventName
        let data = sprintf "%s,%s" (string dateTime) userName
        let url = sprintf "log/%s/%s" workflow event
        sendFunc url "PUT" data state

/// tests if a ResourceResponse is positive
let check_if_positive (status: int) : bool =
    if status >= 200 && status < 300
    then true
    else false

let check_if_positive_bool (resp: string) (status: int): bool =
    if check_if_positive status
    then resp = "true"
    else false