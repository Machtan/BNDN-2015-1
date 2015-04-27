module Repository
type Roles = string list            //A list of roles
type RelationType =                 //The types of relations
    | Dependent
    | Exclusion
    | Response
    | Inclusion
type WorkflowName = string          // The name of a workflow
type EventName = string*string      // WorkflowName*EventName
type EventState = bool*bool*bool    // Executed*Pending*Includet
type UserName = string              // The name of a user

type Relation = RelationType*EventName // A relation containd by a event
type ToRelations = Relation list
type FromRelations = Relation list
type Workflow = WorkflowName*(string list)  // The name of a workflow and the events it contain
type User = UserName*(WorkflowName*Roles list)  // The name of a user + witch roles he have it witch workflow
type Event = EventName*EventState*ToRelations*FromRelations*Roles // all nessary information for a event

// Shemas used when finding all executabel event for a user
type ExecutableInWorkflow = WorkflowName*(string*EventState list)
type Executable = Map<WorkflowName, string*EventState list>

// posible results when working with events
type ResultEvent =
    | Ok of Event
    | Unauthorized
    | NotExecutable
    | MissingEvent of string
    | LockConflict

// posible results when working with workflows
type ResultWorkflow =
    | Ok of Workflow
    | Unauthorized
    | NotExecutable
    | MissingEvent of string

// posible results when working with users
type ResultUser =
    | Ok of User
    | Unauthorized
    | NotExecutable
    | MissingEvent of string


//Create

/// Creates and returns a new event from a name and a start state
val create_event:               EventName -> EventState -> ResultEvent
/// Creates and returns a new workflow from a name
val create_workflow:            WorkflowName -> ResultWorkflow
/// Creates and returns a new user from a name
val create_user:                UserName -> ResultUser

//Read

/// Metodes used when finding all executabel event for a user 
val find_executable_events:     User -> Executable
val find_executable_with_roles: Workflow -> Roles -> ExecutableInWorkflow
/// Return name and state of given event
val get_event_state:            Event -> string*EventState
//VI MANGLER EN CHECK ROLES!!!

//Update

/// Executed and returns the given envent if the given user have the reqred role
val execute:                    Event -> UserName -> ResultEvent
/// Adds a given event to a given Workflow and returns the result
val add_event:                  Workflow -> EventName -> ResultWorkflow
/// Adds given workflow roles to a given user and returns the result
val add_user_roles:             User -> WorkflowName -> Roles -> ResultUser
/// Adds given roles to given event and returns the result
val add_event_roles:            Event -> Roles -> ResultEvent
/// Adds given relationships (going from given event) to given event and returns the result
val add_relation_to:            Event -> RelationType -> EventName -> ResultEvent
/// Adds given relationships (going to given event) to given event and returns the result
val add_relation_from:          Event -> RelationType -> EventName -> ResultEvent

//Delete

/// Deletes given event and returns it if its susesful
val delete_event:               Event -> ResultEvent
/// Deletes given workflow and returns it if its susesful
val delete_workflow:            Workflow -> ResultWorkflow
/// Deletes given user and returns it if its susesful
val delete_user:                User -> ResultUser
/// Removes given event form given workflow and returns the result
val remove_event:               Workflow -> Event -> ResultWorkflow
/// Removes given workflow roles form given user and returns the result
val remove_user_roles:          User -> Workflow -> Roles -> ResultUser
/// Removes given roles form given event and returns the result
val remove_event_roles:         Event -> Roles -> ResultEvent
/// Removes given relation form given event and returns the result
val remove_relation_to:         Event -> Relation -> EventName -> ResultEvent
/// Removes given relation form given event and returns the result
val remove_relation_from:       Event -> Relation -> EventName -> ResultEvent

