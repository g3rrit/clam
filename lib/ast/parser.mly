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

%{
    open Types
%}

%start <Module.t> entry

%type <Toplevel.t> toplevel
%type <Comb.t> comb
%type <Data.t> data
%type <Variant.t> variant
%type <Record.t> record
%type <Exp.t> exp
%type <Exp.alter> alter
%type <Exp.prim> prim
%type <Field.t> field
%type <Type.t> ttype


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
    | v = variant { Data.Var v }
    | r = record  { Data.Rec r }

variant:
    | STRUCT; n = ID { { name = n; var = [] } }

record:
    | DATA; n = ID { { name = n; mem = [] } }

comb:
    | LET; n = ID; COLON; t = ttype; EQ; e = exp 
        { { name = n; args = []; ty = t; exp = e }} 

prim:
    | v = INT { Exp.PInt v }

exp:
    | e = prim { Exp.Prim e }

alter:
    | PIPE; c = ID; a = ID; ARROW; e = exp; { {cons = c; arg = a; clo = e }}

field:
    | n = ID; COLON; t = ttype { { name = n; ty = t }}

ttype:
    | i = ID { Type.Prim i }


