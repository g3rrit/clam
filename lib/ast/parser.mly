%token <string> ID
%token <int> INT
%token <string> STRING

%token LET
%token DATA
%token STRUCT
%token OF
%token IF
%token THEN
%token ELSE
%token MATCH
%token END

%token EQ
%token PIPE
%token SPIPE
%token DOLLAR
%token BACKSLASH
%token COLON
%token SEMICOLON
%token ARROW

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token EOF


%start <Types.Toplevel.t list> entry

%type <Types.Toplevel.t> p_toplevel
%type <Types.Comb.t> p_comb
%type <Types.Data.t> p_data
%type <Types.Variant.t> p_variant
%type <Types.Record.t> p_record
%type <Types.Record.t> p_variant_field
%type <Types.Field.t> p_field
%type <Types.Type.t> p_type

%type <Types.Exp.prim> p_prim
%type <Types.Exp.alter> p_alter
%type <Types.Exp.t> p_exp_basic
%type <Types.Exp.t> p_exp_app
%type <Types.Exp.t> p_exp_dollar
%type <Types.Exp.t> p_exp_seq
%type <Types.Exp.t> p_exp_spipe
%type <Types.Exp.t> p_exp

%%

entry:
    | tls = list(p_toplevel); EOF { tls }

p_toplevel:
    | d = p_data { First d }
    | c = p_comb { Second c }

p_data:
    | v = p_variant { Types.Data.Var v }
    | r = p_record  { Types.Data.Rec r }

p_variant:
    | DATA; n = ID; EQ; vs = separated_list(PIPE, p_variant_field) { { name = n; vars = vs } }

p_variant_field:
    | n = ID; OF; vs = p_record_fields  { { name = n; fs = vs } }
    | n = ID; { { name = n; fs = [] } }

p_record:
    | STRUCT; n = ID; EQ; fs = p_record_fields { { name = n; fs = fs } }

p_record_fields:
    | fs = separated_list(SEMICOLON, p_field) { fs }

p_field:
    | n = ID; COLON; t = p_type { { name = n; ty = t }}

p_comb:
    | LET; n = ID; ags = p_comb_args; COLON; t = p_type; EQ; e = p_exp 
        { { name = n; args = ags; ty = t; exp = e }} 

p_comb_arg:
    | LPAREN; a = p_field; RPAREN { a }

p_comb_args:
    | ags = list(p_comb_arg) { ags }
    | a = p_field { [a] }

p_type:
    | i = ID; ARROW; r = p_type { Types.Type.Fn (Types.Type.Prim i, r) }
    | i = ID { Types.Type.Prim i }

(* EXPRESSION *)

p_prim:
    | v = INT { Types.Exp.PInt v }
    | s = STRING { Types.Exp.PString s }

p_alter:
    | PIPE; c = ID; a = ID; ARROW; e = p_exp; { {con = c; arg = a; exp = e }}

p_exp_basic:
    | LPAREN; e = p_exp; RPAREN { e }
    | e = p_prim { Types.Exp.Prim e }
    | i = ID; { Types.Exp.Ref i }
    | MATCH; e = p_exp; ars = list(p_alter); END
        { Types.Exp.Case (e, ars) }

p_exp_app:
    | l = p_exp_app; r = p_exp_basic { Types.Exp.App (l, r) }
    | e = p_exp_basic { e }

p_exp_spipe:
    | l = p_exp_spipe; SPIPE; r = p_exp_app { Types.Exp.App (r, l) }
    | e = p_exp_app { e }

p_exp_dollar:
    | l = p_exp_dollar; DOLLAR; r = p_exp_spipe { Types.Exp.App (l, r) }
    | e = p_exp_spipe { e }

p_exp_if:
    | IF; c = p_exp; THEN; t = p_exp; ELSE; f = p_exp_if 
        { Types.Exp.If (c, t, f) }
    | e = p_exp_dollar { e }

p_exp_lam:
    | BACKSLASH; ars = list(ID); ARROW; e = p_exp_lam
        { Types.Exp.Lam (ars, e) }
    | e = p_exp_if { e } 

p_exp_let:
    | i = ID; COLON; t = p_type; EQ; e = p_exp_let
        { Types.Exp.Let (i, Some t, e)}
    | e = p_exp_lam { e }

p_exp_seq:
    | l = p_exp_seq; SEMICOLON; r = p_exp_let { Types.Exp.Seq (l, r) }
    | e = p_exp_let { e }

p_exp:
    | e = p_exp_seq { e }
