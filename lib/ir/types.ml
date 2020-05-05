open Std
open Namespace

module Type = struct 
    type t =
        | Prim of ident
        | Fn of t * t
end

module Field = struct 
    type t = 
        { id : ident
        ; ty  : Type.t
        }
end

module Exp = struct
    type prim = 
        | PInt of int
        | PString of string

    type alter =
        { con : ident
        ; arg : ident
        ; exp : t
        }

    and t =
        | App of t * t
        | Ref of ident
        | Prim of prim
        | Lam of ident list * t
        | If of t * t * t
        | Case of t * alter list
        | Let of ident * Type.t * t
        | Seq of t * t
end

module Comb = struct 
    type t =
        { id   : ident
        ; args : Field.t list
        ; ty   : Type.t
        ; exp  : Exp.t
        }
end

module Record = struct 
    type t =
        { id : ident 
        ; fs : Field.t list
        }
end

module Variant = struct
    type t = 
        { id   : ident
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