
open Migrate
open Repository_types

[<EntryPoint>]
let main args =
    printfn "Hello migrator"

    let user_list = [
        ("Bob", ("Bob", [("Hospital", Set.ofList ["Medic"; "Patient"; "Janitor"])]));
        ("Alice", ("Alice", [("Jail", Set.ofList ["Medic"; "Inmate"; "Head honcho"])]));
    ]
    let repo_list = [
        ("Hospital", ["Enter"; "Leave"; "Surgery"; "Terrible accident"; "Zombie outbreak"]);
        ("Kindergarten", ["Enter"; "Eat"; "Run with scissors"; "Play with mud"])
    ]
    let eve w n i p e r t =
        let event = {
            name = (w, n);
            included = i;
            pending = p;
            executed = e;
            toRelations = Set.ofList t;
            fromRelations = Set.empty;
            roles = Set.ofList r
        }
        (false, event)
    let event_list = [
        ("Hospital", Map.ofList [
            ("Enter", eve "Hospital" "Enter" true true false ["Patient"] [(Condition,("Hospital", "Leave")); (Condition, ("Hospital", "Surgery"))]);
            ("Surgery", eve "Hospital" "Surgery" false false true ["Medic"] [(Condition, ("Hospital", "Terrible accident"))]);
            ("Zombie outbreak", eve "Hospital" "Zombie outbreak" true false false ["Janitor"] []);
        ]);
    ]
    let test_repository = {
        events = Map.ofList event_list;
        users = Map.ofList user_list;
        workflows = Map.ofList repo_list;
        logs = [];
    }

    List.iter print_cmd <| get_all_migration_commands test_repository

    printfn "%s" (String.replicate 90 "=")
    let dummy_predicate path = String.length path > 20
    let repo, cmds = get_migratable_commands test_repository dummy_predicate
    List.iter print_cmd cmds
    0