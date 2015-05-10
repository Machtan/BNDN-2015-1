module UserLogic

open Repository_types
open EventLogic


let getUser (user : UserName) (repository : Repository) : User option =
    match Map.tryFind user repository.users with
    | Some(innerMap) ->
        Some(innerMap)
    | None ->
        None

//Create

/// Creates and returns a new user from a name
let create_user (name: UserName) (repository : Repository): CreateUserResult =
    //Det skal lige chekkes om workflow'et ekesitire
    match Map.tryFind name repository.users with
    | Some(innerMap) ->
        UserAlreadyExists
    | None ->
        let user: User = {
            name = name;
            roles = Map.empty;
        }
        let updated_users = Map.add name user repository.users
        CreateUserResult.Ok({repository with users = updated_users; })

//Read

/// Metodes used when finding all executabel event for a user
let get_user_roles (username : UserName)  (workflow : WorkflowName)
        (repository : Repository) : Roles =
    match getUser username repository with
    | Some(user) ->
        match Map.tryFind workflow user.roles with
        | Some(roles) -> roles
        | None -> Set.empty
    | None -> Set.empty

//Update

/// Adds given workflow roles to a given user and returns the result
let add_user_roles  (username : UserName) (workflow : WorkflowName)
        (roles : Roles) (repository : Repository): AddRolesResult =
    match getUser username repository with
    | Some(user) ->
        let user_roles =
            match Map.tryFind workflow user.roles with
            | Some(roles) -> roles
            | None -> Set.empty
        let updated_roles = Map.add workflow (Set.union user_roles roles) user.roles
        let updated_user = { user with roles = updated_roles; }
        let updated_users = Map.add user.name updated_user repository.users
        let updated_repo = { repository with users = updated_users; }
        AddRolesResult.Ok(updated_repo)
    | None ->
        AddRolesResult.UserNotFound

//Delete

/// Deletes given user and returns it if its successful
let delete_user username repository : DeleteUserResult =
    match getUser username repository with
     | Some(user) ->
        let updated_users = Map.remove user.name repository.users
        DeleteUserResult.Ok({repository with users = updated_users; })
     | None ->
        DeleteUserResult.UserNotFound

/// Removes given workflow roles form given user and returns the result
let remove_user_roles  (username : UserName) (workflow : WorkflowName)
        (roles : Roles) (repository : Repository): RemoveRolesResult =
    match getUser username repository with
    | Some(user) ->
        match Map.tryFind workflow user.roles with
        | Some(user_roles) ->
            let valid_role (role: string) =
                not (Set.contains role roles)
            let updated_roles = Map.add workflow (Set.filter valid_role user_roles) user.roles
            let updated_user = { user with roles = updated_roles; }
            let updated_users = Map.add user.name updated_user repository.users
            let updated_repo = { repository with users = updated_users; }
            RemoveRolesResult.Ok(updated_repo)
        | None ->
            RemoveRolesResult.NoRolesForWorkflow
    | None ->
        RemoveRolesResult.UserNotFound
