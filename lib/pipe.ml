open Core
open Stdio
open Ast
open Std

module Config = Config
module Std = Std

let run (fs : File.t list) : unit = 
    ignore (List.map fs ~f:(fun s -> File.to_string s |> printf "%s\n") : unit list);
    printf "%s\n" (Config.to_string ());
    let r = parse ["test"; "example.clm"] in
    match r with 
        | _ -> ()


