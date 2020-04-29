
type t = [
    | `ID of string
    | `INT of int
    | `STRING of string

    (* keywords *)
    | `LET
    | `DATA
    | `STRUCT
    | `OF

    (* operators *)
    | `EQ_OP 
    | `OP of string

    (* parens *)
    | `LPAREN  (* ( *)
    | `RPAREN  (* ) *)
    | `LBRACE  (* { *)
    | `RBRACE  (* } *)
    | `LBRACK  (* [ *)
    | `RBRACK  (* ] *)

    | `EOF
]