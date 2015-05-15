module UserLogic

open Repository_types
open EventLogic
open Pastry

let get_user (user : UserName) (state : PastryState<Repository>) : User option =
    match Map.tryFind user state.data.users with
    | Some(innerMap) ->
        Some(innerMap)
    | None ->
        None

//Create

/// Creates and returns a new user from a name
let create_user (name: UserName) (state : PastryState<Repository>)
        : CreateUserResult =
    //Det skal lige chekkes om workflow'et ekesitire
    match Map.tryFind name state.data.users with
    | Some(innerMap) ->
        UserAlreadyExists
    | None ->
        let user: User = {
            name = name;
            roles = Map.empty;
        }
        let updated_users = Map.add name user state.data.users
        let new_data = {state.data with users = updated_users; }
        CreateUserResult.Ok({state with data = new_data;})

//Read

/// Metodes used when finding all executabel event for a user
let get_user_roles (username : UserName)  (workflow : WorkflowName)
        (state : PastryState<Repository>) : Roles =
    match get_user username state with
    | Some(user) ->
        match Map.tryFind workflow user.roles with
        | Some(roles) ->
            roles
        | None ->
            Set.empty
    | None ->
        Set.empty

//Update

/// Adds given workflow roles to a given user and returns the result
let add_user_roles  (username : UserName) (workflow : WorkflowName)
        (roles : Roles) (state : PastryState<Repository>): AddRolesResult =
    match get_user username state with
    | Some(user) ->
        let user_roles =
            match Map.tryFind workflow user.roles with
            | Some(roles) ->
                roles
            | None ->
                Set.empty
        let updated_roles = Map.add workflow (Set.union user_roles roles) user.roles
        let updated_user = { user with roles = updated_roles; }
        let updated_users = Map.add user.name updated_user state.data.users
        let new_data = { state.data with users = updated_users; }
        AddRolesResult.Ok({state with data = new_data; })
    | None ->
        AddRolesResult.UserNotFound

//Delete

/// Deletes given user and returns it if its successful
let delete_user (username: UserName) (state: PastryState<Repository>)
        : DeleteUserResult =
    match get_user username state with
     | Some(user) ->
        let updated_users = Map.remove user.name state.data.users
        let new_data = {state.data with users = updated_users; }
        DeleteUserResult.Ok({state with data = new_data;})
     | None ->
        DeleteUserResult.UserNotFound

/// Removes given workflow roles form given user and returns the result
let remove_user_roles  (username : UserName) (workflow : WorkflowName)
        (roles : Roles) (state : PastryState<Repository>): RemoveRolesResult =
    match get_user username state with
    | Some(user) ->
        match Map.tryFind workflow user.roles with
        | Some(user_roles) ->
            let valid_role (role: string) =
                not (Set.contains role roles)
            let updated_roles = Map.add workflow (Set.filter valid_role user_roles) user.roles
            let updated_user = { user with roles = updated_roles; }
            let updated_users = Map.add user.name updated_user state.data.users
            let new_data = { state.data with users = updated_users; }
            RemoveRolesResult.Ok({state with data = new_data;})
        | None ->
            RemoveRolesResult.NoRolesForWorkflow
    | None ->
        RemoveRolesResult.UserNotFound
