module Repository_types

open Pastry
open System

type UserName = string              // The name of a user
type WorkflowName = string          // The name of a workflow
type Roles = Set<string>           //A list of roles

type RelationType =                 //The types of relations
    | Condition
    | Exclusion
    | Response
    | Inclusion

type EventState = {
    included: bool;
    pending: bool;
    executed: bool;
}

type User = {
    name: UserName;
    roles: Map<WorkflowName, Roles>;
}

type EventName = WorkflowName*string      // WorkflowName*EventName
type Workflow = { // The name of a workflow and the events it contain
    name: string;
    events: string list;
    logs: string list;
}

// Shemas used when finding all executabel event for a user
type ExecutableInWorkflow = WorkflowName*(string*EventState list)
type Executable = Map<WorkflowName, (string*EventState) list>

type Relation = RelationType*EventName      // A relation containd by a event
type ToRelations = Set<Relation>
type FromRelations = Set<Relation>

// We only really use this as the state singleton (which is nice, tho)
type Event = {
    name: EventName;
    included: bool;
    pending: bool;
    executed: bool;
    toRelations: ToRelations;
    fromRelations: FromRelations;
    roles: Roles;
}

// An error type for events not there
type NotFoundError =
| Event
| Workflow

// A result type for execution
type ExecutableState =
| Executable
| Unauthorized
| NotExecutable
| Locked

// A type for reads of things on an event
type ReadResult<'a> =
| Ok of 'a
| NotFound of NotFoundError

type Repository = {
    // workflow name: event name: (locked, state)
    events: Map<string, Map<string, bool*Event>>;
    users:  Map<string, User>;
    // workflow name: workflow
    workflows: Map<string, Workflow>;
}

// The result when creating a new user
type CreateUserResult =
| Ok of Repository
| UserAlreadyExists

// The result when adding roles to a user
type AddRolesResult =
| Ok of Repository
| UserNotFound

// Samey
type DeleteUserResult = AddRolesResult

// The result when removing roles from a user
type RemoveRolesResult =
| Ok of Repository
| UserNotFound
| NoRolesForWorkflow

// posible results when working with workflows
type ResultWorkflow =
    | Ok of Workflow
    | Unauthorized
    | NotExecutable
    | MissingEvent of string

// posible results when working with events
type ResultEvent =
    | Ok of Event
    | Unauthorized
    | NotExecutable
    | MissingRelation
    | Error of string
    | LockConflict

type Result =
    | Ok of Repository
    | Unauthorized
    | NotExecutable
    | MissingRelation
    | MissingEvent
    | MissingWorkflow
    | LockConflict
    | Error of string

type Message =
    | GetIfCondition            of EventName
    | Lock                      of EventName
    | Unlock                    of EventName
    | SetIncluded               of EventName * bool                // The target event becomes included
    | SetPending                of EventName * bool                // The target event becomes pending
    | GetUserRoles              of UserName * WorkflowName
    | AddFromRelation           of EventName * RelationType * EventName
    | RemoveFromRelation        of EventName * RelationType * EventName
    | RemoveToRelation          of EventName * RelationType * EventName
    | Log                       of EventName * DateTime * UserName