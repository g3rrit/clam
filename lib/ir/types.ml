open Std
open Namespace

module Type = struct 
    type t =
        | Prim of name
        | Fn of t * t
end

module Field = struct 
    type t = 
        { name : name
        ; ty   : Type.t
        }
end

module Exp = struct
    type prim = 
        | PInt of int
        | PString of string

    type alter =
        { con : name
        ; arg : name
        ; exp : t
        }

    and t =
        | App of t * t
        | Ref of name
        | Prim of prim
        | Lam of name list * t
        | If of t * t * t
        | Case of t * alter list
        | Let of name * Type.t * t
        | Seq of t * t
end

module Comb = struct 
    type t =
        { name : name
        ; args : Field.t list
        ; ty   : Type.t
        ; exp  : Exp.t
        }
end

module Record = struct 
    type t =
        { name : name 
        ; fs   : Field.t list
        }
end

module Variant = struct
    type t = 
        { name : name
        ; vars : Record.t list
        }
end

module Data = struct
    type t =
        | Var of Variant.t
        | Rec of Record.t
end

module Module = struct
    type t =
        { file : File.t
        ; cs   : Comb.t list
        ; ds   : Data.t list
        }
end

module Unit = struct
    type t =
        { mods : Module.t list
        }
end