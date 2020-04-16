open Base

module File = struct
    type t = string list
    let to_string (s : t) : string = 
        String.concat ~sep:"/" s
end