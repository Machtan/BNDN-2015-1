module Pastry

open PastryTypes

// Updated state, response, status code
type ResourceResponse<'a> = 'a * string * int

// A function for the resource request func to send requests through
// partial_resource_url, method, data, state -> response
type SendFunc<'a> = string -> string -> string -> 'a -> ResourceResponse<'a>

// A function to handle resource requests
// url, method, data, send_func, state -> response
type ResourceRequestFunc<'a> = string -> string -> string -> SendFunc<'a> -> 'a -> ResourceResponse<'a>

// A function to serialize the state passed through pastry
type SerializeFunc<'a> = 'a -> string

// A record containing the types needed to interface properly with the pastry
// network
type PastryInterface<'a> = {
    send: SendFunc<'a> option;
    handle: ResourceRequestFunc<'a>;
    serialize: SerializeFunc<'a>;
    state: 'a;
}

// Creates a local node and makes it join the Pastry network
// Whenever a request for a resource enters, the resource request func is called
// Thes send func is for the resource handler to request other resources with
val start_server<'a when 'a: equality> : NetworkLocation -> NetworkLocation option -> ResourceRequestFunc<'a> -> SerializeFunc<'a> -> 'a -> unit

// Starts a server with a fixed guid
val test_server<'a when 'a: equality> : NetworkLocation -> NetworkLocation option -> ResourceRequestFunc<'a> -> SerializeFunc<'a> -> string -> 'a -> unit
