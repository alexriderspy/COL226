signature ASTtree =
sig

    type id = string
    datatype binop  = PLUS|MINUS|TIMES|EQ|GT|GEQ|LT|LEQ|NEQ|DIV|MOD|AND|OR
    datatype unop = NOT|NEGATE

    datatype Typ = Int | Bool

    datatype Exp = NumExp of int
    | BoolExp of bool
    | VarExp of id
    | BinExp of binop * Exp * Exp
    | UnExp of unop * Exp
    | ITE of Exp * Stat list * Stat list
    | WH of Exp * Stat list

    and Stat = EXP of Exp | SET of id*Exp | READ of id | WRITE of Exp 
    datatype While = PROG of Stat list

end;
structure AST:ASTtree =
struct
    type id = string
    datatype binop  = PLUS|MINUS|TIMES|EQ|GT|GEQ|LT|LEQ|NEQ|DIV|MOD|AND|OR
    datatype unop = NOT|NEGATE

    datatype Typ = Int | Bool

    datatype Exp = NumExp of int
    | BoolExp of bool
    | VarExp of id
    | BinExp of binop * Exp * Exp
    | UnExp of unop * Exp
    | ITE of Exp * Stat list * Stat list
    | WH of Exp * Stat list

    and Stat = EXP of Exp | SET of id*Exp| READ of id | WRITE of Exp
    datatype While = PROG of Stat list
    
end;