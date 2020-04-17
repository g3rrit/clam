open Base
open Util
open Stdio

module Config = Config

let run (fs : File.t list) : unit 
    = ignore (List.map fs ~f:(fun s -> File.to_string s |> printf "%s\n") : unit list)
    ; printf "%s\n" (Config.to_string ())