open AST;

val M = Array.array(1000,0);

val C = Funstack.create;
val V = Funstack.create;

fun traverse [] = ()
|   traverse (CMD(h)::t) = (Funstack.push("CMD",C);pf(h);traverse(t))

and pf(h) = 
    case h of 
        SET(VEXP(a),IEXP(b)) => (Funstack.push("SET",C);Funstack.push(b,C);Funstack.push(a,C))
    |   SET(VEXP(a),BEXP(b)) => (Funstack.push("SET",C);Funstack.push(b,C);Funstack.push(a,C))
    |   SET(VEXP(a),b) => (Funstack.push("SET",C);pf_exp(b);Funstack.push(a,C))


    |   ITE(BEXP(a),CMDSEQ(b),CMDSEQ(c)) => (Funstack.push("ITE",C);Funstack.push("CMDSEQ",C);traverse(c);Funstack.push("CMDSEQ",C);traverse(b);Funstack.push("BoolEXPITE",C);Funstack.push(Bool.toString(a),C))
    |   ITE(a,CMDSEQ(b),CMDSEQ(c)) => (Funstack.push("ITE",C);Funstack.push("CMDSEQ",C);traverse(c);Funstack.push("CMDSEQ",C);traverse(b);Funstack.push("BoolEXPITE",C);pf_exp(a))

    |   WH(BEXP(a),CMDSEQ(b)) => (Funstack.push("WH",C);Funstack.push("CMDSEQ",C);traverse(b);Funstack.push("BoolEXPWH",C);Funstack.push(Bool.toString(a),C))
    |   WH(a,CMDSEQ(b)) => (Funstack.push("WH",C);Funstack.push("CMDSEQ",C);traverse(b);Funstack.push("BoolEXPWH",C);pf_exp(a))

    | _ => (print("Error");Funstack.create)
and pf_exp(h) = 
    case h of 

    PLUS(VEXP(a),VEXP(b)) => (Funstack.push("PLUS",C);Funstack.push(b,C);Funstack.push(a,C))
    |   PLUS(a,VEXP(b)) => (Funstack.push("PLUS",C);Funstack.push(b,C);pf_exp(a))
    |   PLUS(VEXP(a),b) => (Funstack.push("PLUS",C);pf_exp(b);Funstack.push(a,C))
    |   PLUS(IEXP(a),IEXP(b)) => (Funstack.push("PLUS",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   PLUS(a,IEXP(b)) => (Funstack.push("PLUS",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   PLUS(IEXP(a),b) => (Funstack.push("PLUS",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   PLUS(a,b) => (Funstack.push("PLUS",C);pf_exp(b);pf_exp(a))

    |   TIMES(VEXP(a),VEXP(b)) => (Funstack.push("TIMES",C);Funstack.push(b,C);Funstack.push(a,C))
    |   TIMES(a,VEXP(b)) => (Funstack.push("TIMES",C);Funstack.push(b,C);pf_exp(a))
    |   TIMES(VEXP(a),b) => (Funstack.push("TIMES",C);pf_exp(b);Funstack.push(a,C))
    |   TIMES(IEXP(a),IEXP(b)) => (Funstack.push("TIMES",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   TIMES(a,IEXP(b)) => (Funstack.push("TIMES",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   TIMES(IEXP(a),b) => (Funstack.push("TIMES",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   TIMES(a,b) => (Funstack.push("TIMES",C);pf_exp(b);pf_exp(a))

    |   MINUS(VEXP(a),VEXP(b)) => (Funstack.push("MINUS",C);Funstack.push(b,C);Funstack.push(a,C))
    |   MINUS(a,VEXP(b)) => (Funstack.push("MINUS",C);Funstack.push(b,C);pf_exp(a))
    |   MINUS(VEXP(a),b) => (Funstack.push("MINUS",C);pf_exp(b);Funstack.push(a,C))
    |   MINUS(IEXP(a),IEXP(b)) => (Funstack.push("MINUS",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   MINUS(a,IEXP(b)) => (Funstack.push("MINUS",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   MINUS(IEXP(a),b) => (Funstack.push("MINUS",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   MINUS(a,b) => (Funstack.push("MINUS",C);pf_exp(b);pf_exp(a))

    |   DIV(VEXP(a),VEXP(b)) => (Funstack.push("DIV",C);Funstack.push(b,C);Funstack.push(a,C))
    |   DIV(a,VEXP(b)) => (Funstack.push("DIV",C);Funstack.push(b,C);pf_exp(a))
    |   DIV(VEXP(a),b) => (Funstack.push("DIV",C);pf_exp(b);Funstack.push(a,C))
    |   DIV(IEXP(a),IEXP(b)) => (Funstack.push("DIV",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   DIV(a,IEXP(b)) => (Funstack.push("DIV",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   DIV(IEXP(a),b) => (Funstack.push("DIV",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   DIV(a,b) => (Funstack.push("DIV",C);pf_exp(b);pf_exp(a))

    |   MOD(VEXP(a),VEXP(b)) => (Funstack.push("MOD",C);Funstack.push(b,C);Funstack.push(a,C))
    |   MOD(a,VEXP(b)) => (Funstack.push("MOD",C);Funstack.push(b,C);pf_exp(a))
    |   MOD(VEXP(a),b) => (Funstack.push("MOD",C);pf_exp(b);Funstack.push(a,C))
    |   MOD(IEXP(a),IEXP(b)) => (Funstack.push("MOD",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   MOD(a,IEXP(b)) => (Funstack.push("MOD",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   MOD(IEXP(a),b) => (Funstack.push("MOD",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   MOD(a,b) => (Funstack.push("MOD",C);pf_exp(b);pf_exp(a))


    |   OR(VEXP(a),VEXP(b)) => (Funstack.push("OR",C);Funstack.push(b,C);Funstack.push(a,C))
    |   OR(a,VEXP(b)) => (Funstack.push("OR",C);Funstack.push(b,C);pf_exp(a))
    |   OR(VEXP(a),b) => (Funstack.push("OR",C);pf_exp(b);Funstack.push(a,C))
    |   OR(BEXP(a),BEXP(b)) => (Funstack.push("OR",C);Funstack.push(Bool.toString(b),C);Funstack.push(Bool.toString(a),C))
    |   OR(a,BEXP(b)) => (Funstack.push("OR",C);Funstack.push(Bool.toString(b),C);pf_exp(a))
    |   OR(BEXP(a),b) => (Funstack.push("OR",C);pf_exp(b);Funstack.push(Bool.toString(a),C))
    |   OR(a,b) => (Funstack.push("OR",C);pf_exp(b);pf_exp(a))


    |   AND(VEXP(a),VEXP(b)) => (Funstack.push("AND",C);Funstack.push(b,C);Funstack.push(a,C))
    |   AND(a,VEXP(b)) => (Funstack.push("AND",C);Funstack.push(b,C);pf_exp(a))
    |   AND(VEXP(a),b) => (Funstack.push("AND",C);pf_exp(b);Funstack.push(a,C))
    |   AND(BEXP(a),BEXP(b)) => (Funstack.push("AND",C);Funstack.push(Bool.toString(b),C);Funstack.push(Bool.toString(a),C))
    |   AND(a,BEXP(b)) => (Funstack.push("AND",C);Funstack.push(Bool.toString(b),C);pf_exp(a))
    |   AND(BEXP(a),b) => (Funstack.push("AND",C);pf_exp(b);Funstack.push(Bool.toString(a),C))
    |   AND(a,b) => (Funstack.push("AND",C);pf_exp(b);pf_exp(a))


    |   LT(VEXP(a),VEXP(b)) => (Funstack.push("LT",C);Funstack.push(b,C);Funstack.push(a,C))
    |   LT(a,VEXP(b)) => (Funstack.push("LT",C);Funstack.push(b,C);pf_exp(a))
    |   LT(VEXP(a),b) => (Funstack.push("LT",C);pf_exp(b);Funstack.push(a,C))
    |   LT(BEXP(a),BEXP(b)) => (Funstack.push("LT",C);Funstack.push(Bool.toString(b),C);Funstack.push(Bool.toString(a),C))
    |   LT(a,BEXP(b)) => (Funstack.push("LT",C);Funstack.push(Bool.toString(b),C);pf_exp(a))
    |   LT(BEXP(a),b) => (Funstack.push("LT",C);pf_exp(b);Funstack.push(Bool.toString(a),C))
    |   LT(IEXP(a),IEXP(b)) => (Funstack.push("LT",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   LT(a,IEXP(b)) => (Funstack.push("LT",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   LT(IEXP(a),b) => (Funstack.push("LT",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   LT(a,b) => (Funstack.push("LT",C);pf_exp(b);pf_exp(a))


    |   LEQ(VEXP(a),VEXP(b)) => (Funstack.push("LEQ",C);Funstack.push(b,C);Funstack.push(a,C))
    |   LEQ(a,VEXP(b)) => (Funstack.push("LEQ",C);Funstack.push(b,C);pf_exp(a))
    |   LEQ(VEXP(a),b) => (Funstack.push("LEQ",C);pf_exp(b);Funstack.push(a,C))
    |   LEQ(BEXP(a),BEXP(b)) => (Funstack.push("LEQ",C);Funstack.push(Bool.toString(b),C);Funstack.push(Bool.toString(a),C))
    |   LEQ(a,BEXP(b)) => (Funstack.push("LEQ",C);Funstack.push(Bool.toString(b),C);pf_exp(a))
    |   LEQ(BEXP(a),b) => (Funstack.push("LEQ",C);pf_exp(b);Funstack.push(Bool.toString(a),C))
    |   LEQ(IEXP(a),IEXP(b)) => (Funstack.push("LEQ",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   LEQ(a,IEXP(b)) => (Funstack.push("LEQ",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   LEQ(IEXP(a),b) => (Funstack.push("LEQ",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   LEQ(a,b) => (Funstack.push("LEQ",C);pf_exp(b);pf_exp(a))

    |   EQ(VEXP(a),VEXP(b)) => (Funstack.push("EQ",C);Funstack.push(b,C);Funstack.push(a,C))
    |   EQ(a,VEXP(b)) => (Funstack.push("EQ",C);Funstack.push(b,C);pf_exp(a))
    |   EQ(VEXP(a),b) => (Funstack.push("EQ",C);pf_exp(b);Funstack.push(a,C))
    |   EQ(BEXP(a),BEXP(b)) => (Funstack.push("EQ",C);Funstack.push(Bool.toString(b),C);Funstack.push(Bool.toString(a),C))
    |   EQ(a,BEXP(b)) => (Funstack.push("EQ",C);Funstack.push(Bool.toString(b),C);pf_exp(a))
    |   EQ(BEXP(a),b) => (Funstack.push("EQ",C);pf_exp(b);Funstack.push(Bool.toString(a),C))
    |   EQ(IEXP(a),IEXP(b)) => (Funstack.push("EQ",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   EQ(a,IEXP(b)) => (Funstack.push("EQ",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   EQ(IEXP(a),b) => (Funstack.push("EQ",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   EQ(a,b) => (Funstack.push("EQ",C);pf_exp(b);pf_exp(a))

    |   GT(VEXP(a),VEXP(b)) => (Funstack.push("GT",C);Funstack.push(b,C);Funstack.push(a,C))
    |   GT(a,VEXP(b)) => (Funstack.push("GT",C);Funstack.push(b,C);pf_exp(a))
    |   GT(VEXP(a),b) => (Funstack.push("GT",C);pf_exp(b);Funstack.push(a,C))
    |   GT(BEXP(a),BEXP(b)) => (Funstack.push("GT",C);Funstack.push(Bool.toString(b),C);Funstack.push(Bool.toString(a),C))
    |   GT(a,BEXP(b)) => (Funstack.push("GT",C);Funstack.push(Bool.toString(b),C);pf_exp(a))
    |   GT(BEXP(a),b) => (Funstack.push("GT",C);pf_exp(b);Funstack.push(Bool.toString(a),C))
    |   GT(IEXP(a),IEXP(b)) => (Funstack.push("GT",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   GT(a,IEXP(b)) => (Funstack.push("GT",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   GT(IEXP(a),b) => (Funstack.push("GT",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   GT(a,b) => (Funstack.push("GT",C);pf_exp(b);pf_exp(a))


    |   GEQ(VEXP(a),VEXP(b)) => (Funstack.push("GEQ",C);Funstack.push(b,C);Funstack.push(a,C))
    |   GEQ(a,VEXP(b)) => (Funstack.push("GEQ",C);Funstack.push(b,C);pf_exp(a))
    |   GEQ(VEXP(a),b) => (Funstack.push("GEQ",C);pf_exp(b);Funstack.push(a,C))
    |   GEQ(BEXP(a),BEXP(b)) => (Funstack.push("GEQ",C);Funstack.push(Bool.toString(b),C);Funstack.push(Bool.toString(a),C))
    |   GEQ(a,BEXP(b)) => (Funstack.push("GEQ",C);Funstack.push(Bool.toString(b),C);pf_exp(a))
    |   GEQ(BEXP(a),b) => (Funstack.push("GEQ",C);pf_exp(b);Funstack.push(Bool.toString(a),C))
    |   GEQ(IEXP(a),IEXP(b)) => (Funstack.push("GEQ",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   GEQ(a,IEXP(b)) => (Funstack.push("GEQ",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   GEQ(IEXP(a),b) => (Funstack.push("GEQ",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   GEQ(a,b) => (Funstack.push("GEQ",C);pf_exp(b);pf_exp(a))


    |   NEQ(VEXP(a),VEXP(b)) => (Funstack.push("NEQ",C);Funstack.push(b,C);Funstack.push(a,C))
    |   NEQ(a,VEXP(b)) => (Funstack.push("NEQ",C);Funstack.push(b,C);pf_exp(a))
    |   NEQ(VEXP(a),b) => (Funstack.push("NEQ",C);pf_exp(b);Funstack.push(a,C))
    |   NEQ(BEXP(a),BEXP(b)) => (Funstack.push("NEQ",C);Funstack.push(Bool.toString(b),C);Funstack.push(Bool.toString(a),C))
    |   NEQ(a,BEXP(b)) => (Funstack.push("NEQ",C);Funstack.push(Bool.toString(b),C);pf_exp(a))
    |   NEQ(BEXP(a),b) => (Funstack.push("NEQ",C);pf_exp(b);Funstack.push(Bool.toString(a),C))
    |   NEQ(IEXP(a),IEXP(b)) => (Funstack.push("NEQ",C);Funstack.push(Int.toString(b),C);Funstack.push(Int.toString(a),C))
    |   NEQ(a,IEXP(b)) => (Funstack.push("NEQ",C);Funstack.push(Int.toString(b),C);pf_exp(a))
    |   NEQ(IEXP(a),b) => (Funstack.push("NEQ",C);pf_exp(b);Funstack.push(Int.toString(a),C))
    |   NEQ(a,b) => (Funstack.push("NEQ",C);pf_exp(b);pf_exp(a))


    |   NEGATE(VEXP(a)) => (Funstack.push("NEGATE",C);Funstack.push(a,C))
    |   NEGATE(a) => (Funstack.push("NEGATE",C);pf_exp(a))
    | _ => (print("Error");Funstack.create)

fun start (CMDSEQ(cmdSeq))= (Funstack.push("CMDSEQ",C);traverse(cmdSeq))
