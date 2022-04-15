# Assignment 4 - Evaluating Abstract Syntax tree using VMC Machine

##### _by Sreemanti Dey, 2020CS10393_

## Design Decisions

I have added some constructors called CMD, CMDSEQ, WH_B and ITE_B, to aid me in evaluating the AST. 

## Implementation Details

I have used the Abstract Syntax Tree that I had generated in my previous assignment and converted it into postfix form. And based on the rules defined in the structure Vmc, I have evaluated the postfix expression, while making use of the control stack, memory array and value stack.

In addition to the files that were present before, I have the following new files in my directory:

1.  `stack.sml` - contains the implementation of the stack based on the signature defined in the assignment specifications.

2.  `postfix.sml` - contains the function postfix that converts an AST into its corresponding postfix form.

3.  `rule.sml`- contains the semantic rules that evaluate my AST

4.  `hash.sml`- contains the symbol table and the table that contains the types of my variables.

5.  `execute.sml` - executes the rules defined in semantics.sml

6.  `final.sml` - integrates all the files together and outputs all the configurations of my value stack and control stack at each stage.

### Steps to run my code

1. Open `final.sml`. Change the name of the file in quotes to the one that contains the WHILE program to be tested. 
1. Open terminal and type `sml final.sml`. This will execute the program.

## Acknowledgements

1. I have referred to [hypernotes](https://www.cse.iitd.ac.in/~sak/courses/pl/pl.pdf)

2. I also referred to some links to know about the use of various data structures and signatures in sml. These include:


	a. [HashTable](https://www.smlnj.org/doc/smlnj-lib/Util/str-HashTable.html)
	b. [List](https://smlfamily.github.io/Basis/list.html)
	c. [Array](https://smlfamily.github.io/Basis/array.html)
	d. [Signature&Structure](https://homepages.inf.ed.ac.uk/mfourman/teaching/mlCourse/notes/sml-modules.html)

3. I have also referred to a book on sml, the pdf version of which can be found at [SML-Book-by-Robert-Harper](http://www.cs.cmu.edu/~rwh/isml/book.pdf) 