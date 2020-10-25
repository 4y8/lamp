open Gram
open Combo

type asm
  = S
  | K
  | I
  | B
  | C
  | L
  | E
  | P
  | M
  | A  of asm * asm
  | Cr of int
  | In of int
  | Cn
[@@deriving show]

let combs =
  ['S', Var "S";
   'K', Var "K";
   'I', Var "I";
   'C', Var "C";
   'B', Var "B";
   '+', Var "+";
   '-', Var "-";
   'E', Var "E";
   'L', Var "L";
   ':', Var ":"
  ]

let revcombs =
  let f, s = List.split combs in
  List.combine s f

let rec expr =
  fun s ->
           ((choice (List.map (fun (x, y) -> return y <* char x) combs))
       <|> ((fun l r -> App (l, r)) <$ char '`' <*> expr <*> expr)
       <|> ((fun i -> Loc i) <$ char '@' <*> (int_of_char <$> any))
       <|> ((fun c -> Chr c) <$ char '#' <*> any)) s

let ($) l r = App (l, r)

let combinators =
  ["K"; "I"; "B"; "B*"; "C"; "C'"; "S"; "S'"; "E"; "L"; "+"; "-"; ":"]

let rec last =
  function
    [hd] -> hd
  | _ :: tl -> last tl
  | _ -> failwith "Last element of empty list"

let rec to_deb e v n =
  match e with
    Var s when s = v -> Deb n
  | App (l, r) -> App (to_deb l v n, to_deb r v n)
  | Lam (v', b) -> Lam ("", to_deb (to_deb b v' 0) v (n + 1))
  | e -> e

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
(*
  | App (App (Var "S", App (Var "K", p)), App (App (Var "B", q), r)) ->
     Var "B*" $ p $ q $ r
 *)
  | App (App (Var "S", App (Var "K", p)), q) ->
     Var "B" $ p $ q
(*
  | App (App (Var "S", (App (App (Var "B", p), q))), App (Var "K", r)) ->
     Var "C'" $ p $ q $ r
 *)
  | App (App (Var "S", p), App (Var "K", q)) ->
     Var "C" $ p $ q
(*
  | App (App (Var "S", (App (App (Var "B", p), q))), r) ->
     Var "S'" $ p $ q $ r
 *)
  | e -> e

let rec print_expr =
  function
    Var v -> v
  | App (l, r) -> (String.make 1 '`') ^ (print_expr l) ^ (print_expr r)
  | Loc n -> inplode ['@'; char_of_int (n + 32)]
  | Chr c -> inplode ['#'; c]
  | _ -> raise Not_found

(*
let rec print_asm e =
  let o = List.assq_opt e revcombs in
  match o with
    Some s -> String.make 1 s
  | None ->
     match e with
       Cr c -> inplode ['#'; char_of_int c]
     | In n -> inplode ['@'; char_of_int n]
     | A (l, r) -> (inplode ['`']) ^ (print_asm l) ^ (print_asm r)
     | _ -> raise Not_found
 *)

let rec abs e i =
  match e with
    Deb n when n = i -> Var "I"
  | App (l, r) ->
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
  | App (App (Var "K", l), _) -> eval c l
  | App (App (Var "E", l), r) ->
     if (eval c l) = (eval c r)
     then Var "K"
     else Var "K" $ Var "I"
  | App (App (Var "L", l), r) ->
     let l' = eval c l in
     let r' = eval c r in
     begin
       match l', r' with
         Chr c, Chr c' when c <= c' -> Var "K"
       | _ -> Var "K" $ Var "I"
     end
  | App (App (Var "+", l), r) as e ->
     let l' = eval c l in
     let r' = eval c r in
     begin
       match l', r' with
         Chr c, Chr c' ->
          Chr (char_of_int ((int_of_char c) + (int_of_char c')))
       | _ -> e
     end
  | App (App (Var "-", l), r) as e ->
     let l' = eval c l in
     let r' = eval c r in
     begin
       match l', r' with
         Chr c, Chr c' ->
          Chr (char_of_int ((int_of_char c) - (int_of_char c')))
       | _ -> e
     end
  | App (App (App (App (Var ":", s), p), _), q) ->
     eval c (q $ s $ p)
  | App (App (App (Var "B", f), g), x) ->
     eval c (f $ (g $ x))
  | App (App (App (Var "C", f), g), x) ->
     eval c (f $ x $ g)
  | App (App (App (Var "S", x), y), z) ->
     let z = eval c z in
     eval c ((x $ z) $ (y $ z))
(*
  | App (App (App (App (Var "S'", r), f), g), x) ->
     eval c (r $ (f $ x) $ (g $ x))
  | App (App (App (App (Var "B*", r), f), g), x) ->
     eval c (r $ (f $ (g $ x)))
  | App (App (App (App (Var "C'", r), f), g), x) ->
     eval c (r $ (f $ x) $ g)
 *)
  | Var v when (not (List.mem v combinators)) ->
     List.assoc v c
  | App (l, r) ->
     let l' = eval c l in
     let e = l' $ r in
     if l = l'
     then e
     else eval c e
  | Loc n -> snd (List.nth c (n - 32))
  | e -> e

let rec find v =
  function
    [] -> raise Not_found
  | (f, _) :: _ when f = v -> 0
  | _ :: tl -> 1 + find v tl

let rec reloc c =
  function
    Var v when (not (List.mem v combinators)) -> Loc (find v c)
  | App (l, r) -> App (reloc c l, reloc c r)
  | Lam (_, e) -> Lam ("", reloc c e)
  | e -> e

let rec evalvm c =
  function
    In n -> List.nth c (n - 32)
  | A (I, e) -> evalvm c e
  | A (A (K, e), _) -> evalvm c e
  | A (A (A (S, p), q), r) ->
     evalvm c (A (A (p, r), A (q, r)))
  | A (A (A (B, f), g), x) ->
     evalvm c (A (f, A (g, x)))
  | A (A (A (C, f), g), x) ->
     evalvm c (A (A (f, x), g))
  | A (A (E, l), r) ->
     let l = evalvm c l in
     let r = evalvm c r in
     begin
       match l, r with
         Cr c, Cr c' when c = c' -> K
       | _ -> A (K, I)
     end
  | A (A (L, l), r) ->
     let l = evalvm c l in
     let r = evalvm c r in
     begin
       match l, r with
         Cr c, Cr c' when c <= c' -> K
       | _ -> A (K, I)
     end
  | A (A (P, l), r) as e ->
     let l = evalvm c l in
     let r = evalvm c r in
     begin
       match l, r with
         Cr c, Cr c' -> Cr (c + c')
       | _ -> e
     end
  | A (A (M, l), r) as e ->
     let l = evalvm c l in
     let r = evalvm c r in
     begin
       match l, r with
         Cr c, Cr c' -> Cr (c - c')
       | _ -> e
     end
  | A (A (A (A (Cn, p), q), _), r) ->
     evalvm c (A (A (r, p), q))
  | A (l, r) ->
     let l' = evalvm c l in
     let e = A (l', r) in
     if l = l'
     then e
     else evalvm c e
  | e -> e

let rec get s =
  match s with
    '`' :: tl ->
     let f, tl = get tl in
     let s, tl = get tl in
     ('`' :: f @ s), tl
  | ('#' as c) :: c' :: tl
  | ('@' as c) :: c' :: tl ->
      [c; c'], tl
  | hd :: tl -> [hd], tl
  | _ -> raise Not_found

let rec svm s =
  match s with
    '`' :: 'I' :: tl -> svm tl
  | '`' :: '`' :: 'K' :: tl ->
     let x, tl = get tl in 
     let _, tl = get tl in
     svm (x @ tl)
  | _ -> raise Not_found

let rec encode_asm =
  function
  [] -> K
| hd :: tl -> A (A (Cn, Cr (int_of_char hd)), (encode_asm tl))

let rec decode_asm c e =
  match evalvm c e with
    K -> ""
  | A (A (Cn, h), t) ->
     begin
       match evalvm c h with
         Cr c' -> (String.make 1 (char_of_int c')) ^ (decode_asm c t)
       | _ -> failwith "Bad string"
     end
  | _ -> failwith "Bad string"

let rec encode c =
  function
  [] -> Var "K"
| hd :: tl -> (Var ":" $ Chr hd $ (encode c tl))

let rec decode c e =
  match eval c e with
    Var "K" -> ""
  | App (App (Var ":", h), t) ->
     begin
       match eval c h with
         Chr c' -> (String.make 1 c') ^ (decode c t)
       | _ -> failwith "Bad string"
     end
  | _ -> failwith "Bad string"

let vm s =
  let l = sepBy (between spaces (char ';') spaces) expr s in
  match l with
    None -> failwith "Syntax error"
  | Some (p, _) ->
     let ic = open_in "../main.lamp" in
     seek_in ic 0;
     let s = really_input_string ic (in_channel_length ic) in
     let l = List.map (Fun.const "") p in
     let e = last p in
     let p = List.combine l p in
     let e = eval p (e $ encode [] (explode s)) in
     decode p e

let () =

  let ic = open_in "comb" in
  let s = really_input_string ic (in_channel_length ic) in
  print_endline (vm (explode s));
  (*
  let ic = open_in "../main.lamp" in
  let p  = Parser.program Lexer.lex (Lexing.from_channel ic) in
  let rec clist l c =
    match l with
      [] -> ()
    | ("main", e) :: _ ->
       seek_in ic 0;
       let prelude = really_input_string ic (in_channel_length ic) in
       let s = encode c (explode (prelude)) in
       let e = brack (to_deb e "#" (-1)) in
       let s = decode c (eval c (e $ s)) in
       print_endline s;
    (*print_endline (vm (explode s))*)
    | (f, s) :: tl ->
       clist tl ((f, brack (to_deb s "#" (-1))) :: c)
       (*
    | (_, e) :: tl ->
       print_string (print_expr (reloc c (brack (to_deb e "#" (-1)))));
       print_char ';';
       clist tl c
        *)
  in
  clist p p
   *)
