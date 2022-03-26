CM.make "while.cm";
Control.Print.printDepth:=100;

(*Change test3.wh to the file that contains the WHILE program.*)
While.compile "test3.wh";


val tree_ = it;
use "stack.sml";
use "postfix.sml";
Control.Print.printLength := 100;
use "hash.sml";
fun deconstruct (t)=  
case t of 
AST.PROG(x,y) => (x,y)
val (x,y) = deconstruct(tree_)
val symbols = symbolTable(x)
val lis = postfix(y)
val C = Funstack.list2stack (lis)
val V = Funstack.create
val M = Array.array(1000,0);
use "semantics.sml";
use "execute.sml";
val finalConfig = execute(V,M,C);
fun iden (x) = x;
val V_final = Funstack.toString iden (#1 finalConfig);
val C_final = Funstack.toString iden (#3 finalConfig);
