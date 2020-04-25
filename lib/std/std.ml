open Core

module File = struct
    type t = string list
    let to_string (s : t) : string = 
        String.concat ~sep:"/" s
end

module Location = struct 
    type pos = 
        { line : int 
        ; col  : int
        }

    type loc = pos * pos

    let (<:>) ((l, _) : loc) ((_, r) : loc) : loc =
        (l, r)

end

module Error = struct 
    type kind = 
        | LexerError
        | ParserError

    type t = 
        { src : File.t 
        ; ek  : kind
        ; loc : Location.loc 
        ; msg : string
        }

    exception E of t

    let to_string (e : t) =
        Printf.sprintf "Error in %s at (%d, %d):\n[%s]\n" 
            (File.to_string e.src) ((fst e.loc).line) ((fst e.loc).col) e.msg

    let print (e : t) : unit = 
        Printf.printf "%s" @@ to_string e; ()
    
    let handle : exn -> 'a = function
        | E e -> print e; exit (-1)
        | e -> Printf.printf "COMPILER ERROR\n"; raise e

end

