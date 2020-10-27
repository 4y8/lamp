%{
    open Gram
%}

%token SEMI EQU LAM LPAR RPAR ARR EOF RBRACK LBRACK
%token <char list> STR
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
  | CHAR              { Chr (int_of_char $1) }
  | STR               { string_list $1 }
  | LPAR OP RPAR      { Var $2 }
  | LPAR expr RPAR    { $2 }
  | LBRACK RBRACK      { Var "K" }
  | LBRACK brlist RBRACK { list_list $2 }
;

expr:
  | non_app            { $1 }
  | app                { $1 }
  | op_expr            { $1 }
  | LAM IDE* ARR expr  { mklam $4 $2 }
;

brlist:
  | expr { [$1] }
  | brlist SEMI expr { $1 @ [$3]}
;
