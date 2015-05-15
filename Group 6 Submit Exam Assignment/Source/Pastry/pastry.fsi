module Pastry

open System.Net
open PastryTypes

type Request = {
    url: string;
    meth: string;
    data: string;
}

// The main state of pastry
type PastryState<'a when 'a : equality> = {
    node: Node; // Because nodes change during routing
    data: 'a;   // The data of the application
    requests: Map<Request, HttpListenerResponse list>;
}

// Updated state, response, status code
type ResourceResponse<'a when 'a : equality> = {
    state: PastryState<'a>
    message: string;    // Http response string
    status: int;        // Http status code
}

// A function for the resource request func to send requests through
// partial_resource_url, method, data, state -> response
type SendFunc<'a when 'a : equality> = string -> string -> string -> PastryState<'a> -> ResourceResponse<'a>

// A function to handle resource requests
// url, method, data, send_func, state -> response
type ResourceRequestFunc<'a when 'a : equality> = string -> string -> string -> SendFunc<'a> -> PastryState<'a> -> ResourceResponse<'a>

// A function to serialize the state passed through pastry
type SerializeFunc<'a when 'a : equality> = 'a -> string

// A record used for passing the current application environment easily through
// functions
type PastryEnv<'a when 'a : equality> = {
    send: SendFunc<'a>;
    handle: ResourceRequestFunc<'a>;
    serialize: SerializeFunc<'a>;
    state: PastryState<'a>;
}

// Creates a new resource response from the given values
val resource_response<'a when 'a: equality> : PastryState<'a> -> string -> int -> ResourceResponse<'a>

// Returns whether the given resource belongs on this pastry node or another
// self_guid resource other_guid
val belongs_on_other : string -> string -> string -> bool

// Creates a local node and makes it join the Pastry network
// Whenever a request for a resource enters, the resource request func is called
// Thes send func is for the resource handler to request other resources with
val start_server<'a when 'a: equality> : NetworkLocation -> NetworkLocation option -> ResourceRequestFunc<'a> -> SerializeFunc<'a> -> 'a -> unit

// Starts a server with a fixed guid
val test_server<'a when 'a: equality> : NetworkLocation -> NetworkLocation option -> ResourceRequestFunc<'a> -> SerializeFunc<'a> -> string -> 'a -> unit
