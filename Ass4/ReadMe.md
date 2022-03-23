# Assignment 3 - Converting Abstract Syntax Trees into Programs
##### _by Sreemanti Dey, 2020CS10393_

## Context-free grammar

	STARTS  -> program
	program -> "program" id "::" decSeq "{" cmdSeq "}"
	decSeq -> dec ";" decSeq | epsilon
	dec -> "var" varList ":" "int" | "var" varList ":" "bool"
	varList -> id "," varList | id
	cmdSeq -> cmd ";" cmdSeq | epsilon
	cmd -> exp | assign | "read" id | "write" exp | "if" exp "then" "{" cmdSeq "}" "else" "{" cmdSeq "}" "endif" | "while" exp "do" "{" cmdSeq "}" "endwh"
	assign: id ":=" exp
	exp -> "tt" | "ff" | num | id | "!" exp | exp "+" exp | exp "-" exp | exp "||" exp | exp "*" exp | exp "/" exp | exp "%" exp | exp "and" exp | exp "<" exp | exp "<=" exp | exp "=" exp | exp ">" exp | exp ">=" exp | exp "<>" exp | "~" exp | "(" exp ")"


Along with this, I have specified associativity rules to ensure that my grammar is correct, these rules include
%right IF THEN ELSE ENDIF
%right WHILE DO ENDWH
%left AND OR 
%left EQ 
%left NEQ LEQ LT GEQ GT
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT NEGATE

## AST Datatype definition (including auxiliary datatypes)

	type  id  =  string

	datatype  bin  =  PLUS|MINUS|TIMES|EQ|GT|GEQ|LT|LEQ|NEQ|DIV|MOD|AND|OR
	datatype  sin  =  NOT|NEGATE

	datatype  DEC  =  INT  of  id  list |  BOOL  of  id  list
	datatype  Exp  =  IEXP  of  int |  BEXP  of  bool |  VEXP  of  id | BIXP  of  bin*Exp*Exp|  SINEXP  of  sin*Exp

		and  CMD  =  EXP  of  Exp |  SET  of  id*Exp |  READ  of  id |  WRITE  of  Exp |  ITE  of  Exp*CMD  list*CMD  list |  WH  of  Exp*CMD  list

	 datatype  While  =  PROG  of  DEC  list*CMD  list	

## Syntax-Directed Translation
The rules written in the brackets denote the semantic action that is associated with that rule.

The terminals of my code are those denoted by all capital letters, excluding STARTS. I have written STARTS(which is a non-terminal) in all capital letters because it is the start symbol.

	STARTS: program  (PROG(program))
	program: PROGRAM  ID  START  decSeq  LSPAR  cmdSeq  RSPAR (decSeq,cmdSeq)
	decSeq: dec  EOS  decSeq  (dec::decSeq) | epsilon ([])
	dec: VAR  varList  TYPEOF  INTX  (INT(varList)) |  VAR  varList  TYPEOF  BOOLX  (BOOL(varList))
	varList: ID  COMMA  varList  (ID::varList) |  ID  ([ID])
	cmdSeq: cmd  EOS  cmdSeq  (cmd::cmdSeq) |  ([])
	cmd: 	exp  (EXP(exp)) 
		|  assign  (SET(assign))
		|  READ  ID  (READ(ID))
		|  WRITE  exp  (WRITE(exp))
		|  IF  exp  THEN  LSPAR  cmdSeq  RSPAR  ELSE  LSPAR  cmdSeq  RSPAR  ENDIF  (ITE(exp1,cmdSeq1,cmdSeq2))
		|  WHILE  exp  DO  LSPAR  cmdSeq  RSPAR  ENDWH  (WH(exp,cmdSeq))
	assign:	ID  ASSIGN  exp  ((ID,exp))
	exp:	TT  (BEXP(TT))	
		|  FF  (BEXP(FF))
		|  NUM  (IEXP(NUM))
		|  ID  (VEXP(ID))
		|  NOT  exp  (SINEXP(NOT,exp))
		|  exp  PLUS  exp  (BIXP(PLUS,exp1,exp2))
		|  exp  MINUS  exp  (BIXP(MINUS,exp1,exp2))
		|  exp  OR  exp  (BIXP(OR,exp1,exp2))
		|  exp  TIMES  exp  (BIXP(TIMES,exp1,exp2))
		|  exp  DIV  exp  (BIXP(DIV,exp1,exp2))
		|  exp  MOD  exp  (BIXP(MOD,exp1,exp2))
		|  exp  AND  exp  (BIXP(AND,exp1,exp2))
		|  exp  LT  exp  (BIXP(LT,exp1,exp2))
		|  exp  LEQ  exp  (BIXP(LEQ,exp1,exp2))
		|  exp  EQ  exp  (BIXP(EQ,exp1,exp2))
		|  exp  GT  exp  (BIXP(GT,exp1,exp2))
		|  exp  GEQ  exp  (BIXP(GEQ,exp1,exp2))
		|  exp  NEQ  exp  (BIXP(NEQ,exp1,exp2))
		|  NEGATE  exp  (SINEXP(NEGATE,exp))
		|  LPAR  exp  RPAR  (exp)

## Auxiliary Datatypes
I have defined some auxiliary datatypes for my while programming language, which mainly include the following:
1. `BIXP` - binary expression
2. `SINEXP` - unary expression
3. `VEXP` - var expression

## Other Design Decisions
In order to resolve ambiguities in the EBNF specification, I have taken the following steps:
1. I have merged intexpression and boolexpression into one expression, intFactor and boolFactor into one, intTerm and boolTerm into one common exp.
2. I have also written some extra datatypes called binary expression (BIEXP) and unary expression (SINEXP) and var expression (VEXP) to further resolve shift/reduce conflict that I had encountered
3. Thus I have generated the AST. I will do type checking after the creation of the AST, currently I have generated AST based on the constructors given in the specifications and the normal associativity rules.

## Other Implementation Decisions
I have used ML-Lex to get the tokens from my code and ML-Yacc to parse my code and generate appropriate abstract syntax tree based on the semantic rules mentioned above. I have set lookahead token as 15 in my code.

I have the following file structure in my code:
a.  `while.lex` - contains the lexer specification of while programming language 
b. `while.grm`- contains the parser specification of while programming language
c. `while_ast.sml`- contains the datatypes for my ast
d. `compiler.sml` - parses the input and uses the files to generate and print the AST, also print errors if any
e. `integrate.sml` - builds the lexer and parser
f. `while.cm` - the compilation manager 

### Steps to run my code
1. Open terminal and type `sml`. This starts the SML environment.
2. Type `CM.make "while.cm";`
3. Type `Control.Print.printDepth:=100;` to see all the branches and leaves of the AST.
4. Say a test code file is `test1.wh`. Type  `While.compile "test1.wh";`

## Acknowledgements
1. I have referred to [hypernotes](https://www.cse.iitd.ac.in/~sak/courses/pl/pl.pdf) uploaded by the professor on his web page to know about WHILE programming language.
2. I have referred to [User's Guide to ML-lex and ML-yacc](http://rogerprice.org/ug/ug.pdf) book that is mentioned on the professor's web page, to get an idea how to use ML-Lex and ML-Yacc. Also I have used the idea behind pi.lex, pi.yacc, datatypes.sml, glue.sml, compiler.sml and pi.cm to get an idea of the overall structure I will be using for my implementation of while programming language.
a. I have used glue.sml for my integrate.sml file 
b. I have used compiler.sml for my compiler.sml file
3. I have also referred to [Using_YACC](http://cs.wellesley.edu/~cs235/fall08/lectures/35_YACC_revised.pdf) pdf document to get an idea of the structure of the .grm file and ast.sml file that I will be generating for my project.
4. I would like to thank the professor for teaching us the concepts of context-free grammars and syntax-directed translation, thus overall helping us to complete the assignment.
