module EventLogic

type Roles = string list            //A list of roles
type RelationType =                 //The types of relations
    | Dependent
    | Exclusion
    | Response
    | Inclusion
type WorkflowName = string                  // The name of a workflow
type EventName = WorkflowName*string        // WorkflowName*EventName
type EventState = bool*bool*bool            // Executed*Pending*Includet
type UserName = string                      // The name of a user

type Relation = RelationType*EventName      // A relation containd by a event
type ToRelations = Relation list
type FromRelations = Relation list
type Event = EventName*EventState*ToRelations*FromRelations*Roles // all nessary information for a event

// posible results when working with events
type ResultEvent =
    | Ok of Event
    | Unauthorized
    | NotExecutable
    | MissingEvent of string
    | LockConflict

//Create

/// Creates and returns a new event from a name and a start state
val create_event:               EventName -> EventState -> ResultEvent

//Read

/// Return name and state of given event
val get_event_state:            Event -> string*EventState
/// Check if given event have one of given roles
val check_roles:                Event -> Roles -> bool

//Update

/// Executed and returns the given envent if the given user have the reqred role
val execute:                    Event -> UserName -> ResultEvent
/// Adds given roles to given event and returns the result
val add_event_roles:            Event -> Roles -> ResultEvent
/// Adds given relationships (going from given event) to given event and returns the result
val add_relation_to:            Event -> RelationType -> EventName -> ResultEvent
/// Adds given relationships (going to given event) to given event and returns the result
val add_relation_from:          Event -> RelationType -> EventName -> ResultEvent

//Delete

/// Deletes given event and returns it if its susesful
val delete_event:               Event -> ResultEvent
/// Removes given roles form given event and returns the result
val remove_event_roles:         Event -> Roles -> ResultEvent
/// Removes given relation form given event and returns the result
val remove_relation_to:         Event -> Relation -> EventName -> ResultEvent
/// Removes given relation form given event and returns the result
val remove_relation_from:       Event -> Relation -> EventName -> ResultEvent
