module Pastry

open PastryTypes

// Creates a local node and makes it join the Pastry network
// Whenever a request for a resource enters, the resource request func is called
// Thes send func is for the resource handler to request other resources with
val start_server<'a> : NetworkLocation -> NetworkLocation option -> ResourceRequestFunc<'a> -> 'a -> unit