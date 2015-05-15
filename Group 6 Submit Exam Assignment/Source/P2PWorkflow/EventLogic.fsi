module EventLogic

open Pastry
open Repository_types

//Create

/// Creates and returns a new event from a name and a start state
val create_event: EventName -> EventState -> string list -> bool -> PastryState<Repository> -> Result

//Read

/// Return name and state of given event
val get_event: EventName -> PastryState<Repository> -> ReadResult<Event * bool>

/// Check if given event have one of given roles
val check_roles: EventName -> Roles -> PastryState<Repository> -> ReadResult<bool>

/// Checks if given event is executeble
val check_if_executable: EventName -> UserName -> SendFunc<Repository> -> PastryState<Repository> -> SendResult<ReadResult<ExecutableState>>

/// Checks if given event is locked
val check_if_locked: EventName -> PastryState<Repository> -> ReadResult<bool>

/// Checks if given event is executed / excluded
val check_condition: EventName -> PastryState<Repository> -> ReadResult<bool>

//Update

/// Executed and returns the given envent if the given user have the reqred role
val execute: EventName -> UserName -> SendFunc<Repository> -> PastryState<Repository> -> Result

/// Adds given roles to given event and returns the result
val add_event_roles: EventName -> Roles -> PastryState<Repository> -> Result

/// Adds given relationships (going from given event) to given event and returns the result
val add_relation_to: EventName -> RelationType -> EventName -> SendFunc<Repository> -> PastryState<Repository> -> Result

/// Adds given relationships (going to given event) to given event and returns the result
val add_relation_from: EventName -> RelationType -> EventName -> PastryState<Repository> -> Result

/// Change state

val set_included: EventName -> bool -> PastryState<Repository> -> Result

val set_pending: EventName -> bool -> PastryState<Repository> -> Result

val set_executed: EventName -> bool -> PastryState<Repository> -> Result

/// lock given event
val lock_event: EventName -> PastryState<Repository> -> Result

/// unlock given event
val unlock_event: EventName -> PastryState<Repository> -> Result

//Delete

/// Deletes given event and returns it if its susesful
val delete_event: EventName -> SendFunc<Repository> -> PastryState<Repository> -> Result

/// Removes given roles form given event and returns the result
val remove_event_roles: EventName -> Roles -> PastryState<Repository> -> Result

/// Removes given relation form given event and returns the result
val remove_relation_to: EventName -> RelationType -> EventName -> SendFunc<Repository> -> PastryState<Repository> -> Result

/// Removes given relation form given event and returns the result
val remove_relation_from: EventName -> RelationType -> EventName -> SendFunc<Repository> -> PastryState<Repository> -> Result
