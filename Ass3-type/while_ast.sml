signature ASTtree =
sig

    type id = string
    datatype bin  = PLUS|MINUS|TIMES|EQ|GT|GEQ|LT|LEQ|NEQ|DIV|MOD|AND|OR
    datatype sin = NOT|NEGATE

    datatype DEC = INT of id list 
                | BOOL of id list

    datatype Exp = IEXP of int
            | BEXP of bool
            | VEXP of id
            | BIXP of bin*Exp*Exp (*Binary expression*)
            | SINEXP of sin*Exp (*Unary expression*)

    and CMD = EXP of Exp 
            | SET of id*Exp 
            | READ of id 
            | WRITE of Exp 
            | ITE of Exp*CMD list*CMD list
            | WH of Exp*CMD list

    datatype While = PROG of DEC list*CMD list

end;

structure AST:ASTtree =
struct
    type id = string
    datatype bin  = PLUS|MINUS|TIMES|EQ|GT|GEQ|LT|LEQ|NEQ|DIV|MOD|AND|OR
    datatype sin = NOT|NEGATE

    datatype DEC = INT of id list | BOOL of id list

    datatype Exp = IEXP of int
            | BEXP of bool
            | VEXP of id
            | BIXP of bin * Exp * Exp
            | SINEXP of sin * Exp

    and CMD = EXP of Exp 
            | SET of id*Exp
            | READ of id 
            | WRITE of Exp 
            | ITE of Exp * CMD list * CMD list
            | WH of Exp * CMD list

    datatype While = PROG of DEC list*CMD list
    
end;