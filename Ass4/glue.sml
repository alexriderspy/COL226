use "while_ast.sml";
use "stack.sml";
use "postfix.sml";
Control.Print.printDepth:=100;
Control.Print.printLength := 100;
use "hash.sml";
val symbols = symbolTable([INT ["b","c"],BOOL ["d","e"]])
val lis = postfix(     
        CMDSEQ
       [CMD (SET (VEXP "b",IEXP 1)),CMD (SET (VEXP "c",IEXP 2)),
        CMD (SET (VEXP "c",PLUS (VEXP "c",TIMES (VEXP "b",VEXP "c"))))])
val C = Funstack.list2stack (lis)
val V = Funstack.create
val M = Array.array(1000,0);
use "semantics.sml";
use "execute.sml";
val finalConfig = execute(V,M,C);

