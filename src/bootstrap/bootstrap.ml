open Gram

let rec eval e c =
  match e with
    Var(s) -> List.assoc s c
  | App(Lam(v, b), r) -> eval b ((v, r) :: c) 
  | App(l, r) -> eval (App (eval l c, r)) c
  | e -> e
