open Lexer 
open Lexing 

let parse s = Parser.start Lexer.read (Lexing.from_string s)

let tests = [
  "a";
  "a*";
  "a|b";
  "a|b|c";
  "a|(b|c)";
  "(abc)|(def)";
  "[a-zA-Z0-9]";
]

let do_test x = Format.printf "%s => %a\n" x Deriv.format_derivation_tree (parse x)

let rec do_tests = function
  | [] -> ()
  | x::xs -> do_test x; do_tests xs

let _ = do_tests tests;;