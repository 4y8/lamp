type expr
  = Lam of string * expr
  | Var of string
  | App of expr * expr
  | Chr of char
  | Deb of int
  | Loc of int
[@@deriving show]

let rec mklam e =
  function [] -> e
         | hd :: tl -> Lam(hd, mklam e tl)

let op l op r =
  App (App (Var op, l), r)

let rec string_list =
  function [] -> Var "K"
         | c :: tl -> App (App (Var ":", Chr c), string_list tl)

let rec list_list =
  function [] -> Var "K"
         | e :: tl -> App (App (Var ":", e), list_list tl)
