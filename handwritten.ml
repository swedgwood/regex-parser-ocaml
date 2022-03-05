exception IllegalChar of char
exception UnclosedSquareParen
exception UnclosedParen
exception UnexpectedEnd

(* ========== *)
(*   Lexing   *)
(* ---------- *)

type token =
  | CHAR of char  (* characters *)
  | LSQPAREN  (* [ *)
  | RSQPAREN  (* ] *)
  | LPAREN    (* ( *)
  | RPAREN    (* ) *)
  | PIPE      (* | *)
  | STAR      (* * *)
  | DASH      (* - *)

exception UnexpectedTokenWhenParsingRange of token
exception UnexpectedTokenWhenParsingAtom of token

let chars = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
             'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
             'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
             'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; 
             '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let rec lex = function
  | [] -> []
  | '['::xs -> LSQPAREN::(lex xs)
  | ']'::xs -> RSQPAREN::(lex xs)
  | '('::xs -> LPAREN::(lex xs)
  | ')'::xs -> RPAREN::(lex xs)
  | '|'::xs -> PIPE::(lex xs)
  | '*'::xs -> STAR::(lex xs)
  | '-'::xs -> DASH::(lex xs)
  | x::xs ->
    if List.exists (fun c -> c == x) chars then
      (CHAR x)::(lex xs) 
    else raise (IllegalChar x)

(* ============================= *)
(* Parsing out a derivation tree *)
(* ----------------------------- *)

(* Note, in most places where I use (inp, lhs), this is kind of like a book
   i.e. reading characters from inp, and flipping them straight over to lhs *)

(* each parse_* function roughly refers to a production step in reverse, but
   it's a bit more complicated when it comes to concat, star and atom*)

type rangeatom =
 | D_RANGEATOM1 of char
 | D_RANGEATOM2 of char * char
and range =
 | D_RANGE1 of rangeatom
 | D_RANGE2 of rangeatom * range
and class_ =
 | D_CLASS of range
and atom =
 | D_ATOM1 of char
 | D_ATOM2 of class_
 | D_ATOM3 of alt
and concat =
 | D_CONCAT1 of star * concat
 | D_CONCAT2 of star
and star =
 | D_STAR1 of atom (* has kleene star *)
 | D_STAR2 of atom (* does not have kleene star*)
and alt =
 | D_ALT1 of concat * alt
 | D_ALT2 of concat
and derivation_tree =
 | D_START of alt


let rec parse inp = D_START (parse_alt inp)
and parse_alt inp = parse_alt_rec inp []
(* searches until first pipe outside of brackets, then recurse with ALT1
   if no pipe, then recurse with ALT2*)
and parse_alt_rec inp lhs = match inp with
  | [] -> (D_ALT2 (parse_concat (List.rev lhs)))
  | LPAREN::rest ->
      (* ignore pipes within parentheses *)
      let (rest, lhs) = skip_paren rest (LPAREN::lhs) in
      parse_alt_rec rest lhs
  | PIPE::rest -> (D_ALT1 ((parse_concat (List.rev lhs)), parse_alt rest))
  | next::rest -> parse_alt_rec rest (next::lhs)

(* Takes in input and *)
and skip_paren inp lhs = match inp with
  | LPAREN::rest -> 
      let (rest, lhs) = skip_paren rest (LPAREN::lhs) in
      skip_paren rest lhs
  | RPAREN::rest -> (rest, RPAREN::lhs)
  | x::rest -> skip_paren rest (x::lhs)
  | [] -> raise UnclosedParen
(* searches to the end to see if there is a star at the end or not *)
and pop_last = function
  | [] -> []
  | x::[] -> []
  | x::xs -> x::(pop_last xs)

(* parses out individual stars (and subsequently atoms) and concats them *)
and parse_concat inp = 
  let (atom, rest) = parse_and_extract_star inp in
  if rest == [] then
    D_CONCAT2 atom
  else
    (D_CONCAT1 (atom, parse_concat rest))
and parse_and_extract_star inp = 
  let (atom, rest) = parse_and_extract_atom inp in
  match rest with
  | STAR::xs -> (D_STAR1 atom, xs)
  | _ -> (D_STAR2 atom, rest)
and parse_and_extract_atom = function
  | (CHAR c)::xs -> (D_ATOM1 c, xs)
  | LSQPAREN::xs -> parse_and_extract_atom2 xs
  | LPAREN::xs -> parse_and_extract_atom3 xs
  | x::_ -> raise (UnexpectedTokenWhenParsingAtom x)
  | [] -> raise UnexpectedEnd
and parse_and_extract_atom2 inp = parse_and_extract_atom2_rec inp []
and parse_and_extract_atom2_rec inp lhs = match inp with
  | RSQPAREN::xs -> ((D_ATOM2 (D_CLASS (parse_range (List.rev lhs)))), xs)
  | x::xs -> parse_and_extract_atom2_rec xs (x::lhs)
  | [] -> raise UnclosedSquareParen
and parse_and_extract_atom3 inp = parse_and_extract_atom3_rec inp []
and parse_and_extract_atom3_rec inp lhs = match inp with
  | RPAREN::rest -> ((D_ATOM3 (parse_alt (List.rev lhs))), rest)
  | LPAREN::rest ->
      let (rest, lhs) = skip_paren rest (LPAREN::lhs) in
      parse_and_extract_atom3_rec rest lhs
  | x::xs -> parse_and_extract_atom3_rec xs (x::lhs)
  | [] -> raise UnclosedParen
and parse_range = function
  | (CHAR c1)::DASH::(CHAR c2)::[] -> D_RANGE1 (D_RANGEATOM2 (c1, c2))
  | (CHAR c1)::DASH::(CHAR c2)::xs -> D_RANGE2 (D_RANGEATOM2 (c1, c2), parse_range xs)
  | (CHAR c)::[] -> D_RANGE1 (D_RANGEATOM1 c)
  | (CHAR c)::xs -> D_RANGE2 (D_RANGEATOM1 c, parse_range xs)
  | [] -> raise UnexpectedEnd
  | x::_ -> raise (UnexpectedTokenWhenParsingRange x)


(* https://stackoverflow.com/a/52392884 *)

(* ============================================ *)
(* Converting derivation tree to simplified AST *)
(* -------------------------------------------- *)
type ast_expr =
  | AST_CHAR of char
  | AST_CONCAT of ast_expr * ast_expr
  | AST_ALT of ast_expr * ast_expr
  | AST_STAR of ast_expr
  | AST_CLASS of ast_class list
and ast_class =
  | AST_RANGE of char * char
  | AST_SINGLE_CHAR of char


let rec ast_from_derivation = function
  | D_START alt -> ast_from_alt alt
and ast_from_alt = function
  | D_ALT1 (concat, alt) -> AST_ALT (ast_from_concat concat, ast_from_alt alt)
  | D_ALT2 concat -> ast_from_concat concat
and ast_from_concat = function
  | D_CONCAT1 (star, concat) -> AST_CONCAT (ast_from_star star, ast_from_concat concat)
  | D_CONCAT2 star -> ast_from_star star
and ast_from_star = function
  | D_STAR1 atom -> AST_STAR (ast_from_atom atom)
  | D_STAR2 atom -> ast_from_atom atom
and ast_from_atom = function
  | D_ATOM1 c -> AST_CHAR c
  | D_ATOM2 class_ -> ast_from_class class_
  | D_ATOM3 alt -> ast_from_alt alt
and ast_from_class = function
  | D_CLASS range -> AST_CLASS (ast_range range)
and ast_range = function
  | D_RANGE1 rangeatom -> [ast_rangeatom rangeatom]
  | D_RANGE2 (rangeatom, range) -> (ast_rangeatom rangeatom)::(ast_range range)
and ast_rangeatom = function
  | D_RANGEATOM1 c -> AST_SINGLE_CHAR c
  | D_RANGEATOM2 (c1, c2) -> AST_RANGE (c1, c2)

(* =================== *)
(* Putting it together *)
(* ------------------- *)

let explode s = List.init (String.length s) (String.get s)

let lex_and_parse inp = ast_from_derivation (parse (lex (explode (inp))))

let ast1 = lex_and_parse "(H|h)ello*(W|w)oo*rld"
let ast2 = lex_and_parse "[chrpbmsf]at"
let ast3 = lex_and_parse "[a-zA-Z0-9]*"

(* This took a very long time... hopefully I didn't overcomplicate it *)