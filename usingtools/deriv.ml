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

open Format

let rec format_derivation_tree pp = function
  | D_START x -> fprintf pp "START(%a)" format_alt x
and format_alt pp = function
  | D_ALT1 (c, a) -> fprintf pp "ALT1(%a,%a)" format_concat c format_alt a
  | D_ALT2 c -> fprintf pp "ALT2(%a)" format_concat c
and format_concat pp = function
  | D_CONCAT1 (s, c) -> fprintf pp "CONCAT1(%a,%a)" format_star s format_concat c
  | D_CONCAT2 s -> fprintf pp "CONCAT2(%a)" format_star s
and format_star pp = function
  | D_STAR1 a -> fprintf pp "STAR1(%a)" format_atom a
  | D_STAR2 a -> fprintf pp "STAR2(%a)" format_atom a
and format_atom pp = function
  | D_ATOM1 c -> fprintf pp "ATOM1(%c)" c
  | D_ATOM2 c -> fprintf pp "ATOM2(%a)" format_class c
  | D_ATOM3 a -> fprintf pp "ATOM3(%a)" format_alt a
and format_class pp = function
  | D_CLASS r -> fprintf pp "CLASS(%a)" format_range r
and format_range pp = function
  | D_RANGE1 ra -> fprintf pp "RANGE1(%a)" format_rangeatom ra
  | D_RANGE2 (ra, r) -> fprintf pp "RANGE2(%a,%a)" format_rangeatom ra format_range r
and format_rangeatom pp = function
  | D_RANGEATOM1 c -> fprintf pp "RANGEATOM1(%c)" c
  | D_RANGEATOM2 (c1,c2) -> fprintf pp "RANGEATOM2(%c,%c)" c1 c2