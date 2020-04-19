open Core

module File = struct
    type t = string list
    let to_string (s : t) : string = 
        String.concat ~sep:"/" s
end

module Pos = struct 
    type t = 
        { line : int 
        ; col  : int
        }
end

module Error = struct 
    type kind = 
        | LexerError
        | ParserError

    type t = 
        { src : File.t 
        ; ek  : kind
        ; pos : Pos.t 
        ; msg : string
        }

    exception E of t

    let to_string (e : t) =
        Printf.sprintf "Error in %s at (%d, %d):\n[%s]\n" 
            (File.to_string e.src) (e.pos.line) (e.pos.col) e.msg

    let print (e : t) : unit = 
        Printf.printf "%s" @@ to_string e; ()
    
    let handle : exn -> 'a = function
        | E e -> print e; exit (-1)
        | e -> Printf.printf "COMPILER ERROR\n"; raise e

end

