{
  open Parser
  open Lexing
}

let var = ['_' 'a'-'z' 'A'-'Z'] ['_' ''' 'a'-'z' 'A'-'Z' '0'-'9']*
let op = [':' '=' '|' '&' '*' '<' '>' '$' '+' '-' ',' '@' '.']* 

rule lex = parse
    "--" [^'\n']* '\n' { Lexing.new_line lexbuf; lex lexbuf }
  | [' ' '\t']         { lex lexbuf }
  | "->"               { ARR }
  | '='                { EQU }
  | '\n'               { Lexing.new_line lexbuf; lex lexbuf }
  | '('                { LPAR }
  | ')'                { RPAR }
  | ';'                { SEMI }
  | '\\'               { LAM }
  | '\'' _ '\''        { CHAR (lexeme_char lexbuf 1) }
  | var                { IDE (lexeme lexbuf) }
  | op                 { OP (lexeme lexbuf) }
  | eof                { EOF }

{
}