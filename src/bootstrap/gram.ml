type expr
  = Lam of string * expr
  | Var of string
  | App of expr * expr
