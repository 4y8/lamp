type expr
  = Lam of string * expr
  | Var of string
  | App of expr * expr
  | Chr of char
  | Deb of int
[@@deriving show]

let rec mklam e =
  function [] -> e
	 | hd :: tl -> Lam(hd, mklam e tl)

let op l op r =
  App (App (Var op, l), r)
