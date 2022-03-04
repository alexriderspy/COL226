(* User  declarations *)
fun lookup "special" = 1000
  | lookup "IN" = 900
  
  | lookup s = 0 

%%
(* required declarations *)
%name Calc

%term
  ID of string | NUM of int
| PLUS | TIMES | SUB | DIV  | RPAREN | LPAREN | EOF

%nonterm EXP of int | START of int option 

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%left SUB PLUS
%left TIMES DIV
(* %right *)
  (* %nonassoc*)
%start START

%verbose

%%
START: EXP (SOME EXP)
      |    (NONE)

  EXP: NUM (print(Int.toString(NUM));NUM)
  | ID (lookup ID)
  | EXP PLUS EXP (print("+");EXP1  +  EXP2)
  | EXP SUB  EXP (print("-");EXP1  - EXP2)
  | EXP TIMES  EXP (print("*");EXP1 * EXP2)
  | EXP DIV  EXP (print("div");EXP1 div EXP2)
