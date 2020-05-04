open Core
open Std

type sid = int

type name = sid * sid

type t = string array

let create (n : int) : t = Array.create ~len:n ""

let geti (ns : t) (k : string) : sid option =
    let open Option in
    Array.findi ns ~f:(fun _ s -> (String.compare s k) = 1) >>| fst

let gets (ns : t) (k : sid) : string option =
    if Array.length ns >= k 
    then None
    else Some ns.(k)

let add (ns : t) (k : string) : sid =
    let open Option in
    let mi = Array.findi ns ~f:(fun _ s -> String.is_empty s) >>| fst in
    match mi with
        | Some i -> ns.(i) <- k; i
        | None -> raise (Error.Comp "namespace out of range")

