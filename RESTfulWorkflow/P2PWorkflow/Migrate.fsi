
// Gets the commands to migrate the needed parts from this repo
val get_migratable_commands: Repository -> (string -> bool) -> Repository * CommandType list =

// Gets all the migration commands (YES!)
val get_all_migration_commands: Repository -> CommandType list

