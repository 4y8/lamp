open Gram

let rec to_deb e v n =
  match e with
    Var s when s = v -> Deb n
  | App (l, r) -> App (to_deb l v n, to_deb r v n)
  | Lam (v', b) -> Lam ("", to_deb (to_deb b v' 0) v (n + 1))
  | e -> e

let rec eval e c g =
  match e with
    Var s -> List.assoc s g
  | Deb n -> List.nth c n
  | App (Lam (_, b), r) -> eval b (r :: c) g
  | App (l, r) -> eval (App (eval l c g, r)) c g
  | e -> e

let () =
  let ic = open_in "../main.lamp" in
  let p  = Parser.program Lexer.lex (Lexing.from_channel ic) in
  let rec clist l c =
    match l with
      [] -> raise Not_found
    | ("main", e) :: _ ->
       eval (to_deb e "#" (-1)) [] c
    | (f, s) :: tl ->
       clist tl ((f, to_deb s "#" (-1)) :: c)
  in 
  print_endline (show_expr (clist p []))
