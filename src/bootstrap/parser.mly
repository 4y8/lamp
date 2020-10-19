%{
    open Gram
%}

%token SEMI EQU LAM LPAR RPAR ARR EOF
%token <char> CHAR
%token <string> OP 
%token <string> IDE

%left OP

%start program
%type <(string * Gram.expr) list> program

%%

program:
  | decl* EOF { $1 }
;

decl:
  | IDE IDE* EQU expr SEMI          { ($1, mklam $4 $2) }
  | LPAR OP RPAR IDE* EQU expr SEMI { ($2, mklam $6 $4) }
;

op_expr:
  | expr OP expr { op $1 $2 $3 }
;

app:
  | non_app non_app { App ($1, $2) }
  | app non_app     { App ($1, $2) }
;

non_app:
  | IDE               { Var $1 }
  | CHAR              { Chr $1 }
  | LPAR OP RPAR      { Var $2 }
  | LPAR expr RPAR    { $2 }
;

expr:
  | non_app           { $1 }
  | app               { $1 }
  | op_expr           { $1 }
  | LAM IDE* ARR expr { mklam $4 $2 }
;
