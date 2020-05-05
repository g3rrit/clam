open Core
open Std

type sid = int

type ident = sid * sid

type t = 
    { mid : sid
    ; ns  : string array
    }

let create (mid : sid) (n : int) : t = { mid = mid; ns = Array.create ~len:n "" }

let get_id (ns : t) (k : string) : ident option =
    let open Option in
    Array.findi ns.ns ~f:(fun _ s -> (String.compare s k) = 1) >>| (fun i -> (ns.mid, fst i))

let get_name (ns : t) (k : sid) : string option =
    if Array.length ns.ns >= k 
    then None
    else Some ns.ns.(k)

let add (ns : t) (k : string) : ident =
    let open Option in
    let mi = Array.findi ns.ns ~f:(fun _ s -> String.is_empty s) >>| fst in
    match mi with
        | Some i -> ns.ns.(i) <- k; (ns.mid, i)
        | None -> raise (Error.Comp "namespace out of range")

