module Repository_types

type UserName = string              // The name of a user
type WorkflowName = string          // The name of a workflow
type Roles = Set<string>           //A list of roles
type RelationType =                 //The types of relations
    | Dependent
    | Exclusion
    | Response
    | Inclusion
type EventState = {
    included: bool;
    pending: bool;
    executed: bool;
}


type User = UserName*(WorkflowName*Roles list)  // The name of a user + witch roles he have it witch workflow
type EventName = WorkflowName*string      // WorkflowName*EventName
type Workflow = WorkflowName*(EventName list)  // The name of a workflow and the events it contain

// Shemas used when finding all executabel event for a user
type ExecutableInWorkflow = WorkflowName*(string*EventState list)
type Executable = Map<WorkflowName, string*EventState list>

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

type Repository = {
    // workflow name: event name: (locked, state)
    events: Map<string, Map<string, bool*Event>>;
    users:  Map<string, User>;
    // workflow name: [event name]
    workflows: Map<string, string list>;
    logs: string list;
}

// posible results when working with users
type ResultUser =
    | Ok of User
    | Unauthorized
    | NotExecutable
    | MissingEvent of string

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

///-----Midlertidig---- 
// Updated state, response, status code
type ResourceResponse<'a> = 'a * string * int

// A function for the resource request func to send requests through
// partial_resource_url, method, data, state -> response
type SendFunc<'a> = string -> string -> string -> 'a -> ResourceResponse<'a>

type Result =
    | Ok of Repository
    | Unauthorized
    | NotExecutable
    | MissingRelation
    | LockConflict
    | Error of string