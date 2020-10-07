%token SEMI EQU LAM LPAR RPAR ARR
%token <string> IDE

expr:
  | IDE { Var $1 }
  | expr expr { App ($1, $2) }
  | LAM IDE* ARR expr { let rec mklam e =
			  function [] -> e
				 | hd :: tl -> Lam(hd, mklam e tl)
			in
			mklam $4 $2 }
