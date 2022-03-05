{
open Lexing
open Parser

exception SyntaxError of string

let char_of_string x = String.get x 0;;
}

let char = ['a'-'z' 'A'-'Z' '0'-'9']

rule read =
  parse
  | char { CHAR (char_of_string (Lexing.lexeme lexbuf))}
  | '['  { LSQPAREN }
  | ']'  { RSQPAREN }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '|'  { PIPE }
  | '*'  { STAR }
  | '-'  { DASH }
  | eof  { EOF }
  | _    { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }