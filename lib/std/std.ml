open Core

module File = struct
    type t = string list
    let to_string (s : t) : string = 
        String.concat ~sep:"/" s
end

module Pos = struct 
    type t = Pos of int * int
end

module Error = struct 
    type t = 
        { src : File.t 
        ; pos : Pos.t 
        }

    exception E of t
end

