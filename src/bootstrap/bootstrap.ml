open Gram

let ($) l r = App (l, r)

let combinators = ["K"; "I"; "B"; "C"; "S"]

let rec to_deb e v n =
  match e with
    Var s when s = v -> Deb n
  | App (l, r) -> App (to_deb l v n, to_deb r v n)
  | Lam (v', b) -> Lam ("", to_deb (to_deb b v' 0) v (n + 1))
  | e -> e

let rec in_lam e =
  match e with
    App (l, r) -> ((in_lam l) || (in_lam r))
  | Lam _ -> true
  | _ -> false

let rec dec_deb =
  function
    Deb n -> Deb (n - 1)
  | App (l, r) -> (dec_deb l) $ (dec_deb r)
  | Lam ("", b) -> Lam ("", dec_deb b)
  | e -> e

let rec abs e i =
  match e with
    Deb n when n = i -> Var "I"
  | App (l, r ) when (not (in_lam e)) ->
     (Var "S") $ (abs l i) $ (abs r i) 
  | n -> (Var "K") $ (dec_deb n)

let rec brack e c =
  match e with
    App (l, r) -> (brack l c) $ (brack r c)
  | Lam (_, b) -> (abs (brack b c) 0)
  | Var v when (not (List.mem v combinators)) ->
     brack (List.assoc v c) c
  | n -> n

let rec eval =
  function
    App (Var "I", e) -> eval e
  | App (App (Var "K", l), _) -> (eval l)
  | App (App (App (Var "S", x), y), z) ->
     let z = eval z in
     eval ((x $ z) $ (y $ z))
  | App (l, r) ->
     let r' = eval r in
     let l' = eval l in
     let e = l' $ r' in
     if (l = l') && (r = r')
     then e
     else eval e
  | e -> e

let () =
  let ic = open_in "../main.lamp" in
  let p  = Parser.program Lexer.lex (Lexing.from_channel ic) in
  let rec clist l c =
    match l with
      [] -> raise Not_found
    | ("main", e) :: _ ->
       eval (brack (to_deb e "#" (-1)) c) 
    | (f, s) :: tl ->
       clist tl ((f, to_deb s "#" (-1)) :: c)
  in
  print_endline (show_expr (clist p []))
