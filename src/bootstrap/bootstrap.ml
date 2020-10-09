open Gram

let rec eval e c =
  match e with
    Var (s) -> List.assoc s c
  | App (Lam (v, b), r) -> eval b ((v, r) :: c)
  | App (l, r) -> eval (App (eval l c, r)) c
  | Opp (l, op, r) -> eval (App (App (Var op, l), r)) c
  | e -> e

let () =
  let ic = open_in "../main.lamp" in
  let p  = Parser.program Lexer.lex (Lexing.from_channel ic) in
  List.iter (fun (s, e) -> Printf.printf "%s, %s\n" s (show_expr e)) p
