open Lexing
open Stdio
open Types
open Std

module Types = Types

let parse (file : File.t) : Module.t =
    let inx = In_channel.create (File.to_string file) in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = (File.to_string file) };
    try Parser.entry Lexer.read lexbuf with
        | _ -> raise (Std.Error.(E { src = file; pos = Pos (0, 0)}));



