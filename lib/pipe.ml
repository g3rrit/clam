open Base
open Util
open Stdio

let run (fs : File.t list) : unit = 
    ignore (List.map fs ~f:(fun s -> File.to_string s |> printf "%s\n") : unit list)