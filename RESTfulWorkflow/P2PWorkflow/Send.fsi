module Send

open Repository_types

/// Sends given message with the use of pastery.fs
val send: Message -> SendFunc<Repository> -> Repository -> ResourceResponse<Repository>

/// tests if a ResourceResponse is positive http
val check_if_positive: ResourceResponse<Repository> -> bool

/// tests if a ResourceResponse is positive bool
val check_if_positive_bool: ResourceResponse<Repository> -> bool