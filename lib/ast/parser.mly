%token <string> ID
%token <int> INT
%token <string> STRING

%token LET
%token DATA
%token STRUCT

%token EQ
%token PIPE
%token COLON
%token ARROW

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token EOF


%start <Types.Module.t> entry

%type <Types.Toplevel.t> toplevel
%type <Types.Comb.t> comb
%type <Types.Data.t> data
%type <Types.Variant.t> variant
%type <Types.Record.t> record
%type <Types.Exp.t> exp
%type <Types.Exp.alter> alter
%type <Types.Exp.prim> prim
%type <Types.Field.t> field
%type <Types.Type.t> ttype


%%

%inline ilist(X):
    | (* empty *)           { [] }
    | xs = list(X); x = X   { xs @ [x] }

list(X):
    | xs = ilist(X)    { xs }

%inline ioption(X):
    | /* nothing */ { None }
    | x = X { Some x }

option(X):
    | o = ioption(X) { o }

entry:
    | m = list(toplevel); EOF { { tls = m } }

toplevel:
    | d = data { First d }
    | c = comb { Second c }

data:
    | v = variant { Types.Data.Var v }
    | r = record  { Types.Data.Rec r }

variant:
    | STRUCT; n = ID { { name = n; var = [] } }

record:
    | DATA; n = ID { { name = n; mem = [] } }

comb:
    | LET; n = ID; COLON; t = ttype; EQ; e = exp 
        { { name = n; args = []; ty = t; exp = e }} 

prim:
    | v = INT { Types.Exp.PInt v }

exp:
    | e = prim { Types.Exp.Prim e }

alter:
    | PIPE; c = ID; a = ID; ARROW; e = exp; { {cons = c; arg = a; clo = e }}

field:
    | n = ID; COLON; t = ttype { { name = n; ty = t }}

ttype:
    | i = ID { Types.Type.Prim i }


