%token <string> ID
%token <int> INT
%token <string> STRING

%token LET

%token EQ

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token EOF

%start <Types.Module.t> entry

%type <Types.Comb.t> toplevel

%%

%inline ilist(X):
| (* empty *)           { [] }
| xs = list(X); x = X   { xs @ [x] }

list(X):
| xs = ilist(X)    { xs }

%inline ioption(X):
| /* nothing */
    { None }
| x = X
    { Some x }

option(X):
  o = ioption(X)
    { o }

entry:
    | m = list(toplevel); EOF { { comb = m } }

toplevel:
    | LET; n = ID; EQ; v = INT { { name = n; value = v } } 

data:

variant:

record:

comb:
    | LET; n = ID;  


