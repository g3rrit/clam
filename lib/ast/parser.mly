%token <string> ID
%token <int> INT
%token <string> STRING

%token LET
%token DATA
%token STRUCT
%token OF

%token EQ
%token PIPE
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
%type <Types.Exp.t> p_exp
%type <Types.Exp.alter> p_alter
%type <Types.Exp.prim> p_prim
%type <Types.Field.t> p_field
%type <Types.Type.t> p_type

%nonassoc NON_FN_TY
%nonassoc NON_EXP
%left NON_APP

%right ARROW

%left SEMICOLON


%{
    let make_fn (l : Types.Type.t) (r : Types.Type.t) : Types.Type.t =
        Types.Type.Fn (l, r)

    let make_exp_app (l : Types.Exp.t) (r : Types.Exp.t) : Types.Exp.t =
        Types.Exp.App (l, r)

    let make_exp_seq (l : Types.Exp.t) (r : Types.Exp.t) : Types.Exp.t =
        Types.Exp.Seq (l, r)

%}

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

p_prim:
    | v = INT { Types.Exp.PInt v }

p_exp_s:
    | e = p_exp; SEMICOLON; { make_exp_seq e }
    | e = p_exp { make_exp_app e } %prec NON_APP

p_exp_basic:
    | LPAREN; e = p_exp; RPAREN { e }
    | e = p_prim { Types.Exp.Prim e }
    | i = ID; { Types.Exp.Ref i }

p_exp:
    | e = p_exp_basic { e }
    | f = p_exp_s; e = p_exp { f e } %prec NON_EXP

p_alter:
    | PIPE; c = ID; a = ID; ARROW; e = p_exp_basic; { {con = c; arg = a; exp = e }}

p_type_s:
    | l = p_type; ARROW { make_fn l }

p_type:
    | i = ID { Types.Type.Prim i }
    | f = p_type_s; r = p_type { f r } %prec NON_FN_TY


