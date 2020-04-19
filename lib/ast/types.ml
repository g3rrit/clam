open Base

module Type = struct 
    type t =
        | Prim of string
        | Fn of t * t
end

module Field = struct 
    type t = 
        { name : string option
        ; ty   : Type.t
        }
end

module Exp = struct
    type prim = 
        | PInt of int

    type t =
        | App of t * t
        | Val of string
        | Prim of prim
end

module Comb = struct 
    type t =
        { name : string
        ; args : Field.t list
        ; ty   : Type.t
        ; exp  : Exp.t
        }
end

module Record = struct 
    type t =
        { name : string 
        ; mem  : Field.t list
        }
end

module Variant = struct
    type t = 
        { name : string
        ; var  : Record.t list
        }
end

module Data = struct
    type t =
        | Var of Variant.t
        | Rec of Record.t
end

module Toplevel = struct 
    type t = (Data.t, Comb.t) Either.t
end
 
module Module = struct
    type t =
        { tls : Toplevel.t list
        }
end
