module Rest
open System
open Pastry
open Repository_types
open System.Net

val handle_resource: string -> string -> string -> SendFunc<Repository> -> PastryState<Repository> -> ResourceResponse<Repository>