open Std
open Namespace

module Type = struct 
    type t =
        | Prim of sid
        | Fn of t * t
end

module Field = struct 
    type t = 
        { name : sid
        ; ty   : Type.t
        }
end

module Exp = struct
    type prim = 
        | PInt of int
        | PString of string

    type alter =
        { con : rid
        ; arg : sid
        ; exp : t
        }

    and t =
        | App of t * t
        | Ref of rid
        | Prim of prim
        | Lam of sid list * t
        | If of t * t * t
        | Case of t * alter list
        | Let of sid * Type.t * t
        | Seq of t * t
end

module Comb = struct 
    type t =
        { name : sid
        ; args : Field.t list
        ; ty   : Type.t
        ; exp  : Exp.t
        }
end

module Record = struct 
    type t =
        { name : sid 
        ; fs   : Field.t list
        }
end

module Variant = struct
    type t = 
        { name : sid
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