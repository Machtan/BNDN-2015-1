module Repository
type Roles = string list
type RelationType = 
    | Dependent
    | Exclusion
    | Response
    | Inclusion
type WorkflowName = string
type EventName = string*string      // WorkflowName*EventName
type EventState = bool*bool*bool    // Executed*Pending*Includet
type UserName = string

type Relation = RelationType*EventName
type ToRelations = Relation list
type FromRelations = Relation list
type Workflow = WorkflowName*(string list)
type User = UserName*(WorkflowName*Roles list)
type Event = EventName*EventState*ToRelations*FromRelations*Roles
type ExecutableInWorkflow = WorkflowName*(string*EventState list)
type Executable = Map<WorkflowName, string*EventState list>

type ResultEvent =
    | Ok of Event
    | Unauthorized
    | NotExecutable
    | MissingEvent of string

type ResultWorkflow =
    | Ok of Workflow
    | Unauthorized
    | NotExecutable
    | MissingEvent of string

type ResultUser =
    | Ok of User
    | Unauthorized
    | NotExecutable
    | MissingEvent of string


//Create
val create_event:               EventName -> EventState -> ResultEvent
val create_workflow:            WorkflowName -> ResultWorkflow
val create_user:                UserName -> ResultUser
//Read
val find_executable_events:     User -> Executable
val find_executable_with_roles: Workflow -> Roles -> ExecutableInWorkflow
val get_event_state:            Event -> string*EventState
//VI MANGLER EN CHECK ROLES
//Update
val execute:                    Event -> UserName -> ResultEvent
val add_event:                  Workflow -> EventName -> ResultWorkflow
val add_user_roles:             User -> WorkflowName -> Roles -> ResultUser
val add_event_roles:            Event -> Roles -> ResultEvent
val add_relation_to:            Event -> RelationType -> EventName -> ResultEvent
val add_relation_from:          Event -> RelationType -> EventName -> ResultEvent
//Delete
val delete_event:               Event -> ResultEvent
val delete_workflow:            Workflow -> ResultWorkflow
val delete_user:                User -> ResultUser
val remove_event:               Workflow -> Event -> ResultWorkflow
val remove_user_roles:          User -> Workflow -> Roles -> ResultUser
val remove_event_roles:         Event -> Roles -> ResultEvent
val remove_relation_to:         Event -> Relation -> EventName -> ResultEvent
val remove_relation_from:       Event -> Relation -> EventName -> ResultEvent

