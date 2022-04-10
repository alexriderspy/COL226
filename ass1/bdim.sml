(*This is an interpreter written in SML-NJ *)

(*These are the exceptions I have raised and handled in my code*)
exception DivisionByZero; (*handles division by 0 exception*)
exception NegativeIndex; (*handles negative index exception*)
exception InvalidOpcode; (*handles invalid opcode exception*)
exception InvalidOperation; (* handles the case when we have an int where we are expecting a bool, mainly in boolean or,and operations*)

(*readlist function converts input file into a list of strings based on the newline character which acts as the delimiter*)
fun readlist (file:string) =
    let 
	val input = TextIO.openIn file 
	fun loop (input) = 
	    case TextIO.inputLine input of 
		    SOME line => line :: loop (input) 
		  | NONE      => [] 
    in 
	loop input before TextIO.closeIn input 
    end ;

(*this function breaks a string of the form "(0,0,0,0)\n" into individual characters, with every punctuation mark as the delimiter, while also ignoring negative signs that one may have added based on if the constant literal was positive or negative*)
fun isWordSep c = Char.isSpace c orelse (Char.isPunct c andalso c <> #"-")
val words = String.tokens isWordSep;

(*this function takes in a string list, which is of the form ["0","0","0","0"] and converts the 4 elements of the list into a quadruple*)
fun convertToTuple (xs:string list)=
    (valOf(Int.fromString(List.nth(xs,0))), valOf( Int.fromString(List.nth(xs,1))), valOf(Int.fromString(List.nth(xs,2))), valOf(Int.fromString(List.nth(xs,3))));

(*this function converts all the string lists that are present inside a vector. They are actually quadruples present in the form of string and they are converted into tuples one at a time, I have used vector here since vector has O(1) access*)
fun convertToTupleAll(i,xs:string vector,lis)=
    if i = Vector.length xs then lis
    else convertToTupleAll(i+1,xs,(convertToTuple o words)(Vector.sub(xs,i))::lis);

(*since mem stores only int, so this function is used for converting a bool false to 0 and bool true to 1 and store it in the mem array*)
fun boolToInt (x:bool)=
    if x then 1 else 0;

(*this function checks if the operands have boolean values before applying boolean operations*)
fun isBool (x:int)=
    if x=0 orelse x=1 then ()
    else raise InvalidOperation;

(*this function performs the various operations on our mem array based on the various opcodes*)
fun calc(1,v,j,k,mem) = Array.update(mem,k,v)
  | calc(2,i,j,k,mem)= Array.update(mem,k,Array.sub(mem,i))
  | calc (3,i,j,k,mem) =  (isBool(Array.sub(mem,i));Array.update(mem,k,boolToInt(Array.sub(mem,i)=0)))
  | calc (4,i,j,k,mem) = (isBool(Array.sub(mem,i));isBool(Array.sub(mem,j)); Array.update(mem,k,boolToInt(Array.sub(mem,i)=1 orelse Array.sub(mem,j)=1)))
  | calc (5,i,j,k,mem) =
    (isBool(Array.sub(mem,i));isBool(Array.sub(mem,j));Array.update(mem,k,boolToInt(Array.sub(mem,i)=1 andalso Array.sub(mem,j)=1)))
  | calc (6,i,j,k,mem) =  Array.update(mem,k,Array.sub(mem,i)+Array.sub(mem,j))
  | calc (7,i,j,k,mem) =  Array.update(mem,k,Array.sub(mem,i)-Array.sub(mem,j))
  | calc (8,i,j,k,mem) =  Array.update(mem,k,(Array.sub(mem,i)*Array.sub(mem,j)))
  | calc (9,i,j,k,mem) =  Array.update(mem,k,(Array.sub(mem,i) div Array.sub(mem,j)))
  | calc (10,i,j,k,mem) =  Array.update(mem,k,Array.sub(mem,i) mod Array.sub(mem,j))
  | calc (11,i,j,k,mem) =  Array.update(mem,k,boolToInt(Array.sub(mem,i)=Array.sub(mem,j)))
  | calc (12,i,j,k,mem) =  Array.update(mem,k,boolToInt(Array.sub(mem,i)>Array.sub(mem,j)))
  | calc (16,v,j,k,mem) = Array.update(mem,k,v)
  | calc (_,i,j,k,mem) = raise InvalidOpcode;

(*this function takes in input and is called when opcode is 1*)
fun takeInput()=
    (
      print("input:");
      let
	  val str = valOf (TextIO.inputLine TextIO.stdIn)
	  val i :int = valOf(Int.fromString str)
      in
	  i
      end
	  
    );

(* this function prints output when opcode is 15*)
fun printOutput(x:int)=
    print(Int.toString(x)^"\n");

(*this is the main function which makes a call to all the functions defined above. It stores all the instructions as quadruples in our vector code and executes the instructions from our vector code one by one and terminates when it sees the halt code or when all instructions in code has been executed*)
fun interpret (name:string)=
    let
	val code = Vector.fromList(convertToTupleAll(0,Vector.fromList(List.rev(readlist(name))),[]))
	val maxMemSize = 10000
	val mem = Array.array (maxMemSize, 0);
	fun execute(i)=
	    let
		val tuple = Vector.sub(code,i)
	    in
		if #1 tuple<0 then raise InvalidOpcode 
		else if (#2 tuple<0 andalso #1 tuple<>16) orelse #3 tuple<0 orelse #4 tuple <0 then raise NegativeIndex
		else if i=Vector.length(code) then ()
		else if #1 tuple = 0 then ()
		else if #1 tuple = 1 then (calc(1,takeInput(),#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 2 then (calc(2,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 3 then (calc(3,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 4 then (calc(4,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 5 then (calc(5,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 6 then (calc(6,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 7 then (calc(7,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 8 then (calc(8,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 9 then (
		    if Array.sub(mem,#3 tuple)=0 then raise DivisionByZero
		    else calc(9,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 10 then (
		    if Array.sub(mem,#3 tuple)=0 then raise DivisionByZero
		    else calc(10,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 11 then (calc(11,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 12 then (calc(12,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else if #1 tuple = 13 then (isBool(Array.sub(mem,#2 tuple));if Array.sub(mem,#2 tuple)=1 then execute(#4 tuple) else execute(i+1))
		else if #1 tuple = 14 then execute(#4 tuple)
		else if #1 tuple = 15 then (printOutput(Array.sub(mem,#2 tuple));execute(i+1))
		else if #1 tuple = 16 then (calc(16,#2 tuple,#3 tuple,#4 tuple,mem);execute(i+1))
		else raise InvalidOpcode
		
	    end
    in
	execute(0) (*this part handles all the exceptions as and when they occur*)
	handle Overflow => print("Overflow occurred. Program terminated!\n")
	     | DivisionByZero =>  print("Code tries to do division by zero. Program terminated!\n")
	     | NegativeIndex => print("Code tries to utilise negative indices. Program terminated!\n")
	     | InvalidOpcode => print("Code uses invalid opcode. Program terminated!\n")
		 | Subscript => print("Code tries to jump to an index that is out of the bounds of the vector length. Program terminated!\n")
		 | InvalidOperation => print("Code tries to store an int where it is supposed to place a bool. Program terminated!\n")
    end;
