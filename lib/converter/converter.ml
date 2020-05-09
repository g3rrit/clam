open Printf

module A = Ast.Types
module I = Ir.Types

module type S = sig

    val i : int
    (* val ast : A.Module.t *)

end

module Make (M : S) = struct

    let convert () = printf "converting to ir : %d" M.i

end