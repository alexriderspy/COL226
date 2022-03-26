# Assignment 4 - Evaluating the Abstract Syntax trees using VMC Machine
##### _by Sreemanti Dey, 2020CS10393_

## Design Decisions
In order to resolve ambiguities in the EBNF specification, I have taken the following steps:
1. I have merged intexpression and boolexpression into one expression, intFactor and boolFactor into one, intTerm and boolTerm into one common exp.
2. I have also written some extra datatypes called binary expression (BIEXP) and unary expression (SINEXP) and var expression (VEXP) to further resolve shift/reduce conflict that I had encountered

## Implementation Decisions
I have used ML-Lex to get the tokens from my code and ML-Yacc to parse my code and generate appropriate abstract syntax tree based on the semantic rules mentioned above. I have set lookahead token as 15 in my code.

I have the following file structure in my code:
a.  `while.lex` - contains the lexer specification of while programming language 
b. `while.grm`- contains the parser specification of while programming language
c. `while_ast.sml`- contains the datatypes for my ast
d. `compiler.sml` - parses the input and uses the files to generate and print the AST, also print errors if any
e. `integrate.sml` - builds the lexer and parser
f. `while.cm` - the compilation manager 

### Steps to run my code
1. Open terminal and type `sml final.sml`. This will execute the program.

## Acknowledgements
1. I have referred to [hypernotes](https://www.cse.iitd.ac.in/~sak/courses/pl/pl.pdf) uploaded by the professor on his web page to know about WHILE programming language.
2. I have referred to [User's Guide to ML-lex and ML-yacc](http://rogerprice.org/ug/ug.pdf) book that is mentioned on the professor's web page, to get an idea how to use ML-Lex and ML-Yacc. Also I have used the idea behind pi.lex, pi.yacc, datatypes.sml, glue.sml, compiler.sml and pi.cm to get an idea of the overall structure I will be using for my implementation of while programming language.
a. I have used glue.sml for my integrate.sml file 
b. I have used compiler.sml for my compiler.sml file
3. I have also referred to [Using_YACC](http://cs.wellesley.edu/~cs235/fall08/lectures/35_YACC_revised.pdf) pdf document to get an idea of the structure of the .grm file and ast.sml file that I will be generating for my project.
4. I would like to thank the professor for teaching us the concepts of context-free grammars and syntax-directed translation, thus overall helping us to complete the assignment.
