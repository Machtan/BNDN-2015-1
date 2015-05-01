module pastry

open pastry_types

// A URL denoting the address of a server eg: 127.0.0.1
type NetworkLocation = string

// A function for the resource request func to send requests through
// partial_resource_url, method, data, state -> state, response
type SendFunc<'a> = string -> string -> string -> 'a -> 'a * string

// A function to handle resource requests
// url, method, send_func, state -> state, response
type ResourceRequestFunc<'a> = string -> string -> SendFunc<'a> -> 'a -> 'a * string

// Creates a local node and makes it join the Pastry network
// Whenever a request for a resource enters, the resource request func is called
// Thes send func is for the resource handler to request other resources with
val start_server<'a> : NetworkLocation -> NetworkLocation option -> ResourceRequestFunc<'a> -> 'a -> unit