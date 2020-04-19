open Core
open Stdio
open Ast
open Std

module Config = Config
module Std = Std


let rec run (fs : File.t list) : unit = 
    ignore (List.map fs ~f:(fun s -> File.to_string s |> printf "%s\n") : unit list);
    printf "%s\n" (Config.to_string ());
    try pipe fs with | e -> Error.handle e;

and pipe (fs : File.t list) : unit =
    let _ : Types.Module.t list = parse_mods fs in ()

and parse_mods (fs : File.t list) : Types.Module.t list =
    List.map fs ~f:(fun fi -> parse fi)