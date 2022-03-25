signature ASTtree =
sig

type id = string

datatype DEC = INT of id list | BOOL of id list

datatype Exp = IEXP of int
        | BEXP of bool
        | VEXP of id
        | PLUS of Exp * Exp
        | MINUS of Exp * Exp
        | TIMES of Exp * Exp
        | EQ of Exp * Exp
        | GT of Exp * Exp
        | GEQ of Exp * Exp
        | LT of Exp * Exp
        | LEQ of Exp * Exp
        | NEQ of Exp * Exp
        | DIV of Exp * Exp
        | MOD of Exp * Exp
        | AND of Exp * Exp
        | OR of Exp * Exp
        | NOT of Exp
        | NEGATE of Exp

and CMD = EXP of Exp 
        | SET of id*Exp 
        | READ of id 
        | WRITE of Exp 
        | ITE of Exp*CMDlist*CMDlist
        | WH of Exp*CMDlist

and CMDS = CMD of CMD

and CMDlist = CMDSEQ of CMDS list

datatype While = PROG of DEC list*CMDlist

end;

structure AST:ASTtree =
struct

type id = string

datatype DEC = INT of id list | BOOL of id list

datatype Exp = IEXP of int
        | BEXP of bool
        | VEXP of id
        | PLUS of Exp * Exp
        | MINUS of Exp * Exp
        | TIMES of Exp * Exp
        | EQ of Exp * Exp
        | GT of Exp * Exp
        | GEQ of Exp * Exp
        | LT of Exp * Exp
        | LEQ of Exp * Exp
        | NEQ of Exp * Exp
        | DIV of Exp * Exp
        | MOD of Exp * Exp
        | AND of Exp * Exp
        | OR of Exp * Exp
        | NOT of Exp
        | NEGATE of Exp

and CMD = EXP of Exp 
        | SET of id*Exp 
        | READ of id 
        | WRITE of Exp 
        | ITE of Exp*CMDlist*CMDlist
        | WH of Exp*CMDlist

and CMDS = CMD of CMD

and CMDlist = CMDSEQ of CMDS list

datatype While = PROG of DEC list*CMDlist

end;