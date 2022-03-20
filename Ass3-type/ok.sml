signature ASTtree =
sig
    datatype While = PROG of int list*bool list

end;

structure AST:ASTtree =
struct
    datatype While = PROG of int list*bool list
    
end;