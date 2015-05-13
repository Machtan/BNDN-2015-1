module PastryTypes

open System
open System.Net

// =================== TYPE DEFINITIONS ======================
type U128 = bigint
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
| Backup        // This is the backup state of a watched node
| Ping          // You alive, mate?
| GetState      // Hey, I need something you have (such as a new leaf)
| DeadNode      // Someone has died. Find the one who can fix it

// A result type for HTTP requests
type HttpResult =
| Ok of string
| Error of string * int
| ConnectionError of string

// An argument type for HTTP requests
type HttpAction =
| Download of string * string
| Upload of string * string * string

// The state record of a Pastry node
type Node = {
    guid: GUID;
    address: NetworkLocation;
    neighbors: Map<GUID, Address>;  // Physical location proximity (IP address)
    leaves: Map<GUID, Address>;     // GUID-numerically closest nodes
    minleaf: GUID;
    maxleaf: GUID;
    routing_table: Map<GUID, Address> list;
    backup: string; // A serialized backup of its closest larger leaf
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
    backup: string;
}

type Destination =
| Ok of GUID
| Error of string * int * string