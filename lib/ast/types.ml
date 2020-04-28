open Base

open Std

module type Show = sig 
    type t
    val to_string : t -> string
end

(* 
let paren (type a) (module S : Show with type t = a) (v : a) =
    "(" ^ (S.to_string v) ^ ")"
*)

let paren (s : string) : string =
    "(" ^ s ^ ")"

let brack (s : string) : string =
    "{" ^ s ^ "}"

let brace (s : string) : string =
    "[" ^ s ^ "]"

module Type = struct 
    type t =
        | Prim of string
        | Fn of t * t

    let rec to_string = function
        | Prim s -> s
        | Fn (l, r) -> (to_string l) ^ " -> " ^ (to_string r) |> paren

end

module Field = struct 
    type t = 
        { name : string
        ; ty   : Type.t
        }

    let to_string (v : t) : string =
        v.name ^ " : " ^ (Type.to_string v.ty)
end

module Exp = struct
    type prim = 
        | PInt of int

    type alter =
        { con : string
        ; arg : string
        ; exp : t
        }

    and t =
        | App of t * t
        | Ref of string
        | Prim of prim
        | Lam of string list * t
        | If of t * t * t
        | Case of t * alter list
        | Let of string * Type.t option * t
        | Seq of t * t

    let rec to_string = function
        | App (l, r) -> ((to_string l) ^ (to_string r) |> paren)
        | Ref s -> s
        | Prim p -> 
            begin match p with
                | PInt i -> Int.to_string i
            end
        | Lam (ars, e) -> "\\" ^ (List.map ~f:(fun s -> s ^ " ") ars |> String.concat)
            ^ " -> " ^ (to_string e) |> paren
        | If (_, _, _) -> "if"
        | Case (_, _) -> "case"
        | Let (_, _, _) -> "let"
        | Seq (l, r) -> (to_string l) ^ "\n; " ^ (to_string r) |> paren
end

module Comb = struct 
    type t =
        { name : string
        ; args : Field.t list
        ; ty   : Type.t
        ; exp  : Exp.t
        }

    let to_string (v : t) : string = 
        "comb " ^ v.name 
            ^ (List.map ~f:(fun f -> paren @@ Field.to_string f) v.args |> String.concat)
            ^ " : " ^ (Type.to_string v.ty) 
            ^ " = " ^ (Exp.to_string v.exp)
            ^ "\nend_comb"
end

module Record = struct 
    type t =
        { name : string 
        ; fs   : Field.t list
        }

    let to_string (v : t) : string =
        "record " ^ v.name ^ " =\n"
            ^ (List.map ~f:(fun f -> paren (Field.to_string f) ^ "\n") v.fs |> String.concat) 
            ^ "end_record"
end

module Variant = struct
    type t = 
        { name : string
        ; vars : Record.t list
        }

    let to_string (v : t) : string =
        "variant " ^ v.name ^ " =\n"
            ^ (List.map ~f:(fun r -> paren (Record.to_string r) ^ "\n") v.vars |> String.concat) 
            ^ "end_variant"
end

module Data = struct
    type t =
        | Var of Variant.t
        | Rec of Record.t

    let to_string = function
        | Var v -> Variant.to_string v
        | Rec r -> Record.to_string r
end

module Toplevel = struct 
    type t = (Data.t, Comb.t) Either.t

    let to_string = function
        | Either.First d -> Data.to_string d
        | Either.Second c -> Comb.to_string c
end
 
module Module = struct
    type t =
        { file : File.t
        ; tls  : Toplevel.t list
        }

    let to_string (v : t) : string =
        "module " ^ (File.to_string v.file) ^ "\n"
            ^ (List.map ~f:(fun tl -> (Toplevel.to_string tl) ^ "\n\n") v.tls |> String.concat)
            ^ "end_module"
end
