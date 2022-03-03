signature ASTtree =
sig

    type id = string
    datatype binop  = PLUS|MINUS|TIMES|EQ|GT|GEQ|LT|LEQ|NEQ|DIV|MOD|AND|OR
    datatype unop = NOT|NEGATE

    datatype DEC = INT of id list | BOOL of id list

    datatype Exp = NumExp of int
    | BoolExp of bool
    | VarExp of id
    | BinExp of binop * Exp * Exp
    | UnExp of unop * Exp

    and CMD = EXP of Exp | SET of id*Exp | READ of id | WRITE of Exp     | ITE of Exp * CMD list * CMD list
    | WH of Exp * CMD list

     
    datatype While = PROG of DEC list*CMD list

end;
structure AST:ASTtree =
struct
    type id = string
    datatype binop  = PLUS|MINUS|TIMES|EQ|GT|GEQ|LT|LEQ|NEQ|DIV|MOD|AND|OR
    datatype unop = NOT|NEGATE

    datatype DEC = INT of id list | BOOL of id list

    datatype Exp = NumExp of int
    | BoolExp of bool
    | VarExp of id
    | BinExp of binop * Exp * Exp
    | UnExp of unop * Exp

    and CMD = EXP of Exp | SET of id*Exp| READ of id | WRITE of Exp     | ITE of Exp * CMD list * CMD list
    | WH of Exp * CMD list

    datatype While = PROG of DEC list *CMD list
    
end;