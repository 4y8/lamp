open Gram

let ($) f g x = f(g(x))

let id i = Deb i

let lift_one i = Deb (i + 1)

let cons sigma t = function
    0 -> t
  | x -> sigma (x - 1)

let rec apply_subs sigma = function
    Deb i -> sigma i
  | Lam (_, t) -> Lam ("", apply_subs (function
                                 0 -> Deb 0
                               | i -> apply_subs lift_one (sigma (i - 1))
                             ) t)
  | App (t1, t2) -> App (apply_subs sigma t1, apply_subs sigma t2)
  | e -> e

let is_value = function
  | Lam _ | Deb _ | Var _ -> true
  | _ -> false

let rec small_step c = function
    App (Lam (_, t), v) when is_value v ->
     apply_subs (cons id v) t
  | App (t, u) when is_value t ->
     App (t, small_step c u)
  | App (t, u) ->
     App (small_step c t, u)
  | Var v -> List.assoc v c
  | t when is_value t ->
     t
  | e -> e

let rec eval c = function
  | Var v -> List.assoc v c
  | t when is_value t -> t
  | t -> let t' = small_step c t in
         if t' = t then t
         else eval c t'

let rec to_deb e v n =
  match e with
    Var s when s = v -> Deb n
  | App (l, r) -> App (to_deb l v n, to_deb r v n)
  | Lam (v', b) -> Lam ("", to_deb (to_deb b v' 0) v (n + 1))
  | e -> e

let () =
  let ic = open_in "../main.lamp" in
  let p  = Parser.program Lexer.lex (Lexing.from_channel ic) in
  let rec clist l c =
    match l with
      [] -> raise Not_found
    | ("main", e) :: _ ->
       eval c (eval c (to_deb e "#" (-1))) 
    | (f, s) :: tl ->
       clist tl ((f, to_deb s "#" (-1)) :: c)
  in
  print_endline (show_expr (clist p []))
