open AST;

fun traverse ([],lis) = (lis)
|   traverse (CMD(h)::t,lis) = (traverse(t,lis@pf(h)))

and pf(h) = 
    case h of 
        SET(VEXP(a),IEXP(b)) => ([a]@[Int.toString(b)]@["SET"])
    |   SET(VEXP(a),BEXP(b)) => ([a]@[Bool.toString(b)]@["SET"])
    |   SET(VEXP(a),b) => ([a]@pf_exp(b)@["SET"])


    |   ITE(BEXP(a),CMDSEQ(b),CMDSEQ(c)) => ([Bool.toString(a)]@["ITE_B"]@traverse(b,[])@["CMDSEQ"]@traverse(c,[])@["CMDSEQ"]@["ITE"])
    |   ITE(a,CMDSEQ(b),CMDSEQ(c)) => (pf_exp(a)@["ITE_B"]@traverse(b,[])@["CMDSEQ"]@traverse(c,[])@["CMDSEQ"]@["ITE"])

    |   WH(BEXP(a),CMDSEQ(b)) => ([Bool.toString(a)]@traverse(b,[])@["CMDSEQ"]@["WH"])
    |   WH(a,CMDSEQ(b)) => (pf_exp(a)@traverse(b,[])@["CMDSEQ"]@["WH"])

    | _ => (print("Error");[])

and pf_exp(h) = 
    case h of 

    PLUS(VEXP(a),VEXP(b)) => ([a]@[b]@["PLUS"])
    |   PLUS(a,VEXP(b)) => (pf_exp(a)@[b]@["PLUS"])
    |   PLUS(VEXP(a),b) => ([a]@pf_exp(b)@["PLUS"])
    |   PLUS(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["PLUS"])
    |   PLUS(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["PLUS"])
    |   PLUS(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["PLUS"])
    |   PLUS(a,b) => (pf_exp(a)@pf_exp(b)@["PLUS"])

    |   MINUS(VEXP(a),VEXP(b)) => ([a]@[b]@["MINUS"])
    |   MINUS(a,VEXP(b)) => (pf_exp(a)@[b]@["MINUS"])
    |   MINUS(VEXP(a),b) => ([a]@pf_exp(b)@["MINUS"])
    |   MINUS(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["MINUS"])
    |   MINUS(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["MINUS"])
    |   MINUS(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["MINUS"])
    |   MINUS(a,b) => (pf_exp(a)@pf_exp(b)@["MINUS"])

    |   TIMES(VEXP(a),VEXP(b)) => ([a]@[b]@["TIMES"])
    |   TIMES(a,VEXP(b)) => (pf_exp(a)@[b]@["TIMES"])
    |   TIMES(VEXP(a),b) => ([a]@pf_exp(b)@["TIMES"])
    |   TIMES(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["TIMES"])
    |   TIMES(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["TIMES"])
    |   TIMES(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["TIMES"])
    |   TIMES(a,b) => (pf_exp(a)@pf_exp(b)@["TIMES"])

    |   DIV(VEXP(a),VEXP(b)) => ([a]@[b]@["DIV"])
    |   DIV(a,VEXP(b)) => (pf_exp(a)@[b]@["DIV"])
    |   DIV(VEXP(a),b) => ([a]@pf_exp(b)@["DIV"])
    |   DIV(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["DIV"])
    |   DIV(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["DIV"])
    |   DIV(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["DIV"])
    |   DIV(a,b) => (pf_exp(a)@pf_exp(b)@["DIV"])

    |   MOD(VEXP(a),VEXP(b)) => ([a]@[b]@["MOD"])
    |   MOD(a,VEXP(b)) => (pf_exp(a)@[b]@["MOD"])
    |   MOD(VEXP(a),b) => ([a]@pf_exp(b)@["MOD"])
    |   MOD(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["MOD"])
    |   MOD(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["MOD"])
    |   MOD(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["MOD"])
    |   MOD(a,b) => (pf_exp(a)@pf_exp(b)@["MOD"])


    |   AND(VEXP(a),VEXP(b)) => ([a]@[b]@["AND"])
    |   AND(a,VEXP(b)) => (pf_exp(a)@[b]@["AND"])
    |   AND(VEXP(a),b) => ([a]@pf_exp(b)@["AND"])
    |   AND(BEXP(a),BEXP(b)) => ([Bool.toString(a)]@[Bool.toString(b)]@["AND"])
    |   AND(a,BEXP(b)) => (pf_exp(a)@[Bool.toString(b)]@["AND"])
    |   AND(BEXP(a),b) => ([Bool.toString(a)]@pf_exp(b)@["AND"])
    |   AND(a,b) => (pf_exp(a)@pf_exp(b)@["AND"])


    |   OR(VEXP(a),VEXP(b)) => ([a]@[b]@["OR"])
    |   OR(a,VEXP(b)) => (pf_exp(a)@[b]@["OR"])
    |   OR(VEXP(a),b) => ([a]@pf_exp(b)@["OR"])
    |   OR(BEXP(a),BEXP(b)) => ([Bool.toString(a)]@[Bool.toString(b)]@["OR"])
    |   OR(a,BEXP(b)) => (pf_exp(a)@[Bool.toString(b)]@["OR"])
    |   OR(BEXP(a),b) => ([Bool.toString(a)]@pf_exp(b)@["OR"])
    |   OR(a,b) => (pf_exp(a)@pf_exp(b)@["OR"])


    |   LT(VEXP(a),VEXP(b)) => ([a]@[b]@["LT"])
    |   LT(a,VEXP(b)) => (pf_exp(a)@[b]@["LT"])
    |   LT(VEXP(a),b) => ([a]@pf_exp(b)@["LT"])
    |   LT(BEXP(a),BEXP(b)) => ([Bool.toString(a)]@[Bool.toString(b)]@["LT"])
    |   LT(a,BEXP(b)) => (pf_exp(a)@[Bool.toString(b)]@["LT"])
    |   LT(BEXP(a),b) => ([Bool.toString(a)]@pf_exp(b)@["LT"])
    |   LT(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["LT"])
    |   LT(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["LT"])
    |   LT(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["LT"])
    |   LT(a,b) => (pf_exp(a)@pf_exp(b)@["LT"])


    |   LEQ(VEXP(a),VEXP(b)) => ([a]@[b]@["LEQ"])
    |   LEQ(a,VEXP(b)) => (pf_exp(a)@[b]@["LEQ"])
    |   LEQ(VEXP(a),b) => ([a]@pf_exp(b)@["LEQ"])
    |   LEQ(BEXP(a),BEXP(b)) => ([Bool.toString(a)]@[Bool.toString(b)]@["LEQ"])
    |   LEQ(a,BEXP(b)) => (pf_exp(a)@[Bool.toString(b)]@["LEQ"])
    |   LEQ(BEXP(a),b) => ([Bool.toString(a)]@pf_exp(b)@["LEQ"])
    |   LEQ(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["LEQ"])
    |   LEQ(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["LEQ"])
    |   LEQ(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["LEQ"])
    |   LEQ(a,b) => (pf_exp(a)@pf_exp(b)@["LEQ"])

    |   GT(VEXP(a),VEXP(b)) => ([a]@[b]@["GT"])
    |   GT(a,VEXP(b)) => (pf_exp(a)@[b]@["GT"])
    |   GT(VEXP(a),b) => ([a]@pf_exp(b)@["GT"])
    |   GT(BEXP(a),BEXP(b)) => ([Bool.toString(a)]@[Bool.toString(b)]@["GT"])
    |   GT(a,BEXP(b)) => (pf_exp(a)@[Bool.toString(b)]@["GT"])
    |   GT(BEXP(a),b) => ([Bool.toString(a)]@pf_exp(b)@["GT"])
    |   GT(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["GT"])
    |   GT(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["GT"])
    |   GT(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["GT"])
    |   GT(a,b) => (pf_exp(a)@pf_exp(b)@["GT"])

    |   GEQ(VEXP(a),VEXP(b)) => ([a]@[b]@["GEQ"])
    |   GEQ(a,VEXP(b)) => (pf_exp(a)@[b]@["GEQ"])
    |   GEQ(VEXP(a),b) => ([a]@pf_exp(b)@["GEQ"])
    |   GEQ(BEXP(a),BEXP(b)) => ([Bool.toString(a)]@[Bool.toString(b)]@["GEQ"])
    |   GEQ(a,BEXP(b)) => (pf_exp(a)@[Bool.toString(b)]@["GEQ"])
    |   GEQ(BEXP(a),b) => ([Bool.toString(a)]@pf_exp(b)@["GEQ"])
    |   GEQ(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["GEQ"])
    |   GEQ(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["GEQ"])
    |   GEQ(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["GEQ"])
    |   GEQ(a,b) => (pf_exp(a)@pf_exp(b)@["GEQ"])


    |   EQ(VEXP(a),VEXP(b)) => ([a]@[b]@["EQ"])
    |   EQ(a,VEXP(b)) => (pf_exp(a)@[b]@["EQ"])
    |   EQ(VEXP(a),b) => ([a]@pf_exp(b)@["EQ"])
    |   EQ(BEXP(a),BEXP(b)) => ([Bool.toString(a)]@[Bool.toString(b)]@["EQ"])
    |   EQ(a,BEXP(b)) => (pf_exp(a)@[Bool.toString(b)]@["EQ"])
    |   EQ(BEXP(a),b) => ([Bool.toString(a)]@pf_exp(b)@["EQ"])
    |   EQ(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["EQ"])
    |   EQ(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["EQ"])
    |   EQ(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["EQ"])
    |   EQ(a,b) => (pf_exp(a)@pf_exp(b)@["EQ"])


    |   NEQ(VEXP(a),VEXP(b)) => ([a]@[b]@["NEQ"])
    |   NEQ(a,VEXP(b)) => (pf_exp(a)@[b]@["NEQ"])
    |   NEQ(VEXP(a),b) => ([a]@pf_exp(b)@["NEQ"])
    |   NEQ(BEXP(a),BEXP(b)) => ([Bool.toString(a)]@[Bool.toString(b)]@["NEQ"])
    |   NEQ(a,BEXP(b)) => (pf_exp(a)@[Bool.toString(b)]@["NEQ"])
    |   NEQ(BEXP(a),b) => ([Bool.toString(a)]@pf_exp(b)@["NEQ"])
    |   NEQ(IEXP(a),IEXP(b)) => ([Int.toString(a)]@[Int.toString(b)]@["NEQ"])
    |   NEQ(a,IEXP(b)) => (pf_exp(a)@[Int.toString(b)]@["NEQ"])
    |   NEQ(IEXP(a),b) => ([Int.toString(a)]@pf_exp(b)@["NEQ"])
    |   NEQ(a,b) => (pf_exp(a)@pf_exp(b)@["NEQ"])


    |   NEGATE(VEXP(a)) => ([a]@["NEGATE"])
    |   NEGATE(IEXP(a)) => ([Int.toString(a)]@["NEGATE"])
    |   NEGATE(a) => (pf_exp(a)@["NEGATE"])

    |   NOT(VEXP(a)) => ([a]@["NOT"])
    |   NOT(BEXP(a)) => ([Bool.toString(a)]@["NOT"])
    |   NOT(a) => (pf_exp(a)@["NOT"])
    | _ => (print("Error");[])

fun postfix (CMDSEQ(cmdSeq))= 
let
  fun aux(lis) = (traverse(cmdSeq,lis)@["CMDSEQ"])
in
  aux([])
end
