﻿module EventLogic

open Repository_types

//Create

/// Creates and returns a new event from a name and a start state
val create_event:               EventName -> EventState -> Repository -> Result

//Read

/// Return name and state of given event
val get_event_state:            EventName -> Repository -> EventState
/// Check if given event have one of given roles
val check_roles:                EventName -> Roles -> Repository -> bool
/// Checks if given event is executeble
val check_if_executeble:        EventName -> SendFunc<Repository> -> Repository -> bool
/// Checks if given event is luck'et
val check_if_lucked:            EventName -> Repository -> bool
/// Checks if given event is executed / excluded
val check_condition:            EventName -> Repository -> bool

//Update

/// Executed and returns the given envent if the given user have the reqred role
val execute:                    EventName -> UserName -> SendFunc<Repository> -> Repository -> Result
/// Adds given roles to given event and returns the result
val add_event_roles:            EventName -> Roles -> Repository -> Result
/// Adds given relationships (going from given event) to given event and returns the result
val add_relation_to:            EventName -> RelationType -> EventName -> SendFunc<Repository> -> Repository -> Result
/// Adds given relationships (going to given event) to given event and returns the result
val add_relation_from:          EventName -> RelationType -> EventName -> Repository -> Result
/// luck given event
val luck_event:                 EventName -> Repository -> Result
/// unluck given event
val unluck_event:               EventName -> Repository -> Result

//Delete

/// Deletes given event and returns it if its susesful
val delete_event:               EventName -> SendFunc<Repository> -> Repository -> Result
/// Removes given roles form given event and returns the result
val remove_event_roles:         EventName -> Roles -> Repository -> Result
/// Removes given relation form given event and returns the result
val remove_relation_to:         EventName -> RelationType -> EventName -> SendFunc<Repository> -> Repository -> Result
/// Removes given relation form given event and returns the result
val remove_relation_from:       EventName -> RelationType -> EventName -> SendFunc<Repository> -> Repository -> Result
