module pastry_types

open System
open System.Net

// =================== TYPE DEFINITIONS ======================
type U128 = {
    a: uint64; // First part of the 128 bits
    b: uint64; // Second part of the 128 bits
}
type NetworkLocation = string // an url... Maybe something better later
type GUID = U128 // u128... Maybe something better later
type Address = NetworkLocation

// NOTE NOTE NOTE
// When you UPDATE this:
// Also update 'send_messsage' and 'start_listening.listen'
type MessageType =
| Join      // A new node is joining and requesting states
| Update    // A new node has been added sucessfully: update to include it
| JoinState // The state data for a new node
// Request a resource somewhere to be routed back using the context
| Resource of string * string
    // Add more persistency commands here

// The state record of a Pastry node
type Node = {
    guid: GUID;
    address: NetworkLocation;
    neighbors: Map<GUID, Address>;  // Physical location proximity (IP address)
    leaves: Map<GUID, Address>;     // GUID-numerically closest nodes
    minleaf: U128;
    maxleaf: U128;
    routing_table: Map<GUID, Address> list;
}

// Something to make the JSON serialization work
type SerializableNode = {
    guid: string;
    address: NetworkLocation;
    routing_table: Map<string, Address> list;
    minleaf: string;
    maxleaf: string;
    neighbors: Map<string, Address>;
    leaves: Map<string, Address>;
}

// A result type to simplify the interpretation a little
type InterpretResult<'a> =
| Valid of Node * 'a
| Invalid of string * int * string

type Destination =
| Ok of GUID
| Error of string * int * string