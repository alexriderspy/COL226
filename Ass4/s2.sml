use "while_ast.sml";
use "stack.sml";
use "vmc.sml";
Control.Print.printDepth:=100;
Control.Print.printLength := 100;
start(     CMDSEQ
       [CMD (SET (VEXP "b",IEXP 1)),CMD (SET (VEXP "c",IEXP 2)),
        CMD (SET (VEXP "c",PLUS (VEXP "c",TIMES (VEXP "b",VEXP "c"))))],[])