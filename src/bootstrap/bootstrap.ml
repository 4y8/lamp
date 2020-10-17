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

let opt =
  function
    App (App (Var "S", App (Var "K", p)), App (Var "K", q)) ->
     Var "K" $ (p $ q)
  | App (App (Var "S", App (Var "K", p)), Var "I") -> p
  | App (App (Var "S", App (Var "K", p)), q) ->
     Var "B" $ p $ q
  | App (App (Var "S", p), App (Var "K", q)) ->
     Var "C" $ p $ q
  | e -> e

let rec abs e i =
  match e with
    Deb n when n = i -> Var "I"
  | App (l, r) when (not (in_lam e)) ->
     opt ((Var "S") $ (abs l i) $ (abs r i)) 
  | n -> (Var "K") $ (dec_deb n)

let rec brack e =
  match e with
    App (l, r) -> (brack l) $ (brack r)
  | Lam (_, b) -> (abs (brack b) 0)
  | n -> n

let rec eval c =
  function
    App (Var "I", e) -> eval c e
  | App (App (Var "K", l), _) -> (eval c l)
  | App (App (App (Var "B", f), g), x) ->
     eval c (f $ (g $ x))
  | App (App (App (Var "C", f), g), x) ->
     eval c (f $ x $ g)
  | App (App (App (Var "S", x), y), z) ->
     let z = eval c z in
     eval c ((x $ z) $ (y $ z))
  | App (App (App (App (Var "S'", r), f), g), x) ->
     eval c (r $ (f $ x) $ (g $ x))
  | Var v when (not (List.mem v combinators)) ->
     brack (List.assoc v c)
  | App (l, r) ->
     let r' = eval c r in
     let l' = eval c l in
     let e = l' $ r' in
     if l = l' && r = r'
     then e
     else eval c e
  | e -> e

let () =
  let ic = open_in "../main.lamp" in
  let p  = Parser.program Lexer.lex (Lexing.from_channel ic) in
  let rec clist l c =
    match l with
      [] -> raise Not_found
    | ("main", e) :: _ ->
       eval c (brack (to_deb e "#" (-1))) 
    | (f, s) :: tl ->
       clist tl ((f, (to_deb s "#" (-1))) :: c)
  in
  print_endline (show_expr (clist p []))
