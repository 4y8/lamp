open Gram

let rec eval e c =
  match e with
    Var (s) -> List.assoc s c
  | App (Lam (v, b), r) -> eval b ((v, r) :: c)
  | App (l, r) -> eval (App (eval l c, r)) c
  | Opp (l, op, r) -> eval (App (App (Var op, l), r)) c
  | e -> e
