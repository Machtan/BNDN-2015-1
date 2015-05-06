module UserLogic

open Repository_types

type UpdateMapResult =
| MapOk of Map<string, User>
| MapErr of ResultUser
 

let getUser (user : UserName) (repository : Repository) : User option =
    match Map.tryFind user repository.users with
    | Some(innerMap)   -> Some(innerMap)
    | None      -> None

//Create

/// Creates and returns a new user from a name
let create_user (user : UserName)  (repository : Repository) : ResultUser =     
    let nUser = (user,[]) : User
    //Det skal lige chekkes om workflow'et ekesitire
    match Map.tryFind user repository.users with
    | Some(innerMap)   -> ResultUser.NotExecutable
    | None      ->  ResultUser.Ok({repository with users = Map.add user nUser repository.users})
   


//Read

/// Metodes used when finding all executabel event for a user 
let get_user_roles (username : UserName)  (workflow : WorkflowName) (repository : Repository) : Roles list =
//        let user = getUser username repository
//        match (user) with
//        | Some(u) -> 
//            let (uuserName,uWfRolesList) = u 
//            let rec UpdateUserRoles ls =
//                match (ls) with
//                | (wf,rl)::ls ->  
//                    match (wf) with
//                    | wf when wf = workflow ->   
//                            (wf, Set.union rl roles) :: UpdateUserRoles ls                           
//                    | _ -> (wf,rl) :: UpdateUserRoles ls
//                | [] -> []
//
//            ResultUser.Ok({repository with users = Map.add username (username, UpdateUserRoles uWfRolesList) repository.users})
//        | None -> Map.empty
    failwith "Not Implemented"

/// Metodes used when finding all executabel event for a user 
let find_executable_events (username : UserName) (repository : Repository) : Executable =
//        let user = getUser username repository
//        match (user) with
//        | Some(u) -> 
//            let (uuserName,uWfRolesList) = u 
//            let rec UpdateUserRoles ls =
//                match (ls) with
//                | (wf,rl)::ls ->  
//                    match (wf) with
//                    | wf when wf = workflow ->   
//                            (wf, Set.union rl roles) :: UpdateUserRoles ls                           
//                    | _ -> (wf,rl) :: UpdateUserRoles ls
//                | [] -> []
//
//            ResultUser.Ok({repository with users = Map.add username (username, UpdateUserRoles uWfRolesList) repository.users})
//        | None -> Map.empty
    failwith "Not Implemented"

//Update

/// Adds given workflow roles to a given user and returns the result
let add_user_roles  (username : UserName) (workflow : WorkflowName) (roles : Roles) (repository : Repository): ResultUser =  
        let user = getUser username repository
        match (user) with
        | Some(u) -> 
            let (uuserName,uWfRolesList) = u 
            let rec UpdateUserRoles ls =
                match (ls) with
                | (wf,rl)::ls ->  
                    match (wf) with
                    | wf when wf = workflow ->   
                            (wf, Set.union rl roles) :: UpdateUserRoles ls                           
                    | _ -> (wf,rl) :: UpdateUserRoles ls
                | [] -> []

            ResultUser.Ok({repository with users = Map.add username (username, UpdateUserRoles uWfRolesList) repository.users})
        | None -> ResultUser.MissingUser


//Delete

/// Deletes given user and returns it if its successful
let delete_user username repository : ResultUser =
    let user = getUser username repository
    match (user) with
     | Some(u) ->  
        let inner (x : Repository) = Map.remove username x.users
        ResultUser.Ok({repository with users = inner repository})
     | None -> ResultUser.MissingUser
    

/// Removes given workflow roles form given user and returns the result
let remove_user_roles  (username : UserName) (workflow : WorkflowName) (roles : Roles) (repository : Repository): ResultUser =  
        let user = getUser username repository
        match (user) with
        | Some(u) -> 
            let (uuserName,uWfRolesList) = u 
            let rec UpdateUserRoles ls =
                match (ls) with
                | (wf,rl)::ls ->  
                    match (wf) with
                    | wf when wf = workflow ->   
                            (wf, Set.difference rl roles) :: UpdateUserRoles ls                           
                    | _ -> (wf,rl) :: UpdateUserRoles ls
                | [] -> []

            ResultUser.Ok({repository with users = Map.add username (username, UpdateUserRoles uWfRolesList) repository.users})
        | None -> ResultUser.MissingUser
