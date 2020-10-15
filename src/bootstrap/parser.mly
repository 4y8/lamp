%{
    open Gram
%}

%token SEMI EQU LAM LPAR RPAR ARR EOF
%token <char> CHAR
%token <string> OP 
%token <string> IDE

%start program
%type <(string * Gram.expr) list> program

%%

program:
  | decl* EOF { $1 }
;

decl:
  | IDE IDE* EQU op_expr SEMI          { ($1, mklam $4 $2) }
  | LPAR OP RPAR IDE* EQU op_expr SEMI { ($2, mklam $6 $4) }
;

op_expr:
  | expr                    { $1 }
  | op_expr OP op_expr      { Opp($1, $2, $3) }
;

app:
  | expr expr { App ($1, $2) }
  | app expr { App ($1, $2) }
;

expr:
  | app               { $1 }
  | IDE               { Var $1 }
  | CHAR              { Chr $1 }
  | LPAR OP RPAR      { Var $2 }
  | LPAR expr RPAR    { $2 }
  | LPAR op_expr RPAR { $2 }
  | LAM IDE* ARR expr { mklam $4 $2 }
;
