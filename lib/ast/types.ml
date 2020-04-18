
module Comb = struct 
    type t =
        { name  : string
        ; value : int
        }
end
 
module Module = struct
    type t =
        { comb : Comb.t list
        }
end


   