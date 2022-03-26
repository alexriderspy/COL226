# Assignment 4 - Evaluating the Abstract Syntax trees using VMC Machine
##### _by Sreemanti Dey, 2020CS10393_

## Implementation Decisions
I have used the Abstract Syntax Tree that I had generated in my previous assignment and converted it into postfix form. And based on the rules defined in the structure Vmc, I have evaluated the postfix expression, while making use of control stack and value stack. 

In addition to the files that were present before, I have the following new files in my directory:

1.  `stack.sml` - contains the implementation of the stack based on the signature defined in the assignment specifications.
2.  `postfix.sml` - contains the function postfix that converts an AST into its corresponding postfix form.
3. `semantics.sml`- contains the semantic rules that evaluate my AST
4. `hash.sml`- contains the symbol table and the table that contains the types of my variables.
5. `execute.sml` - executes the rules defined in semantics.sml
6. `final.sml` - integrates all the files together and outputs all the configurations of my value stack and control stack at each stage.

### Steps to run my code
1. Open terminal and type `sml final.sml`. This will execute the program.

## Acknowledgements
1. I have referred to [hypernotes](https://www.cse.iitd.ac.in/~sak/courses/pl/pl.pdf)
2. I also referred to some links to know about the use of various data structures in sml. These include:
1. [HashTable](https://www.smlnj.org/doc/smlnj-lib/Util/str-HashTable.html)
2. [List](https://smlfamily.github.io/Basis/list.html)
3. [Array](https://smlfamily.github.io/Basis/array.html)
4. [Signature&Structure](https://homepages.inf.ed.ac.uk/mfourman/teaching/mlCourse/notes/sml-modules.html)