module Send

open Pastry
open Repository_types

/// Sends given message with the use of pastery.fs
val send: Message -> SendFunc<Repository> -> PastryState<Repository> -> ResourceResponse<Repository>

/// tests if a ResourceResponse is positive http
val check_if_positive: int -> bool

/// tests if a ResourceResponse is positive bool
val check_if_positive_bool: string -> int -> bool