open Lexing
open Stdio
open Types
open Std

module Types = Types

let parse (file : File.t) : Module.t =
    let inx = In_channel.create (File.to_string file) in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = (File.to_string file) };
    try 
        Parser.entry Lexer.read lexbuf 
    with _ ->
        begin
            let curr = lexbuf.Lexing.lex_curr_p in
            let lnum = curr.Lexing.pos_lnum in
            let cnum = curr.Lexing.pos_cnum in
            let tok  = Lexing.lexeme lexbuf in
            raise (Std.Error.(E 
                    { src = file
                    ; ek  = ParserError
                    ; pos = { line = lnum; col = cnum }
                    ; msg = "unexpected token: " ^ tok
                    }))
        end

        (*| _ -> raise (Std.Error.(E { src = file; pos = Pos (0, 0)}));*)



