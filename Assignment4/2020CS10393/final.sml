CM.make "while.cm";
Control.Print.printDepth:=100;

(*Change test3.wh to the file that contains the WHILE program to be tested.*)
(* While.compile "test_cases/test1.txt"; *)
(*While.compile "test_cases/test2.txt";*)
(* While.compile "test_cases/test3.txt"; *)
(* While.compile "test_cases/test4.txt"; *)
(*While.compile "test_cases/test5.txt";*)
(* While.compile "test_cases/test6.txt"; *)
(* While.compile "test_cases/test7.txt"; *)
(* While.compile "test_cases/test8.txt"; *)
While.compile "test_cases/test9.txt"; 
(*While.compile "test_cases/test10.txt";*)

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
use "rule.sml";
use "execute.sml";
val finalConfig = execute(V,M,C);
fun iden (x) = x;
val V_final = Funstack.toString iden (#1 finalConfig);
val C_final = Funstack.toString iden (#3 finalConfig);
