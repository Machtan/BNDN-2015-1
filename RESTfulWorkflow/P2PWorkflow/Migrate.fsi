module Migrate

open Repository_types

type Command = {
    path: string;
    meth: string;
    data: string;
}

// Gets the commands to migrate the needed parts from this repo
val get_migratable_commands: Repository -> (string -> bool) -> Repository * Command list

// Gets all the migration commands (YES!)
val get_all_migration_commands: Repository -> Command list

