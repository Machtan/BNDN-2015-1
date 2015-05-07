module Rest
open System
open Pastry
open Repository_types
open System.Net
open WorkflowOld

val resource_handler: string -> string -> string -> SendFunc<Repository> -> Repository -> ResourceResponse<Repository>

val KonoTestoKawaii: int