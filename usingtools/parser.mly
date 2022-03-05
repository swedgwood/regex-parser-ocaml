
%token<char> CHAR
%token LSQPAREN RSQPAREN LPAREN RPAREN PIPE STAR DASH EOF
%start start
%type <Deriv.derivation_tree> start
%%

start:
  | alt { Deriv.D_START $1 }

alt:
  | concat PIPE alt { Deriv.D_ALT1 ($1, $3) }
  | concat          { Deriv.D_ALT2 $1 }

concat:
  | star concat { Deriv.D_CONCAT1 ($1, $2) }
  | star        { Deriv.D_CONCAT2 $1 }

star:
  | atom STAR { Deriv.D_STAR1 $1 }
  | atom      { Deriv.D_STAR2 $1 }

atom:
  | CHAR              { Deriv.D_ATOM1 $1 }
  | class_            { Deriv.D_ATOM2 $1 }
  | LPAREN alt RPAREN { Deriv.D_ATOM3 $2 }

class_:
  | LSQPAREN range RSQPAREN { Deriv.D_CLASS $2 }

range:
  | rangeatom       { Deriv.D_RANGE1 $1 }
  | rangeatom range { Deriv.D_RANGE2 ($1, $2) }

rangeatom:
  | CHAR           { Deriv.D_RANGEATOM1 $1 }
  | CHAR DASH CHAR { Deriv.D_RANGEATOM2 ($1, $3) }