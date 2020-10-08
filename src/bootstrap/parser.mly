%{
    open Gram
%}

%token SEMI EQU LAM LPAR RPAR ARR EOF
%token <string> IDE

%start program
%type <(string * Gram.expr) list> program

%%

program:
  | decl* EOF { $1 }
;

decl:
  | IDE EQU expr SEMI { ($1, $3) }
;

expr:
  | IDE { Var $1 }
  | expr expr { App ($1, $2) }
  | LAM IDE* ARR expr { let rec mklam e =
			  function [] -> e
				 | hd :: tl -> Lam(hd, mklam e tl)
			in
			mklam $4 $2 }
  | LPAR expr RPAR { $2 }
;
