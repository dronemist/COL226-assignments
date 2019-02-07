# A simple definitional interpreter and stack machine
## Setup
Keep a0.mli,a1.mli,a0.ml,a1.ml and .ocamlinit in the same folder. Run the following command from the terminal 
```
ocamlbuild a1.cmo a0.cmo
```
This will create a _build directory and then the functions can be used from ocaml top level

## Components
**The abstract syntax** is characterised by the type
```ocaml
type  exptree =  N of int
| Plus of exptree * exptree
| Minus of exptree * exptree
| Mult of exptree * exptree
| Div of exptree * exptree
| Rem of exptree * exptree
| Nega of exptree (* Neg is for sign in BigInt. Nega is negative of expression  *)
| Abs of exptree
```
**The definitional interpreter** is defined as a function 
```ocaml
val eval : exptree -> int
```
**The type of opcodes** of the stack machine is defined as:
```ocaml
type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS ;;
```
**The stack machine** is defined as a tail-recursive function
```ocaml
val stackmc: (bigint list) -> (opcode list) -> bigint
```
Here bigint is the bigint package implemented in assignment 0.  
**Compile**
The compile function is simply a postorder traversal of an abstract syntax tree of an expression
The compiler is defined as a recursive function
```ocaml
val compile: exptree -> opcode list
```
Finally we conclude that for an exptree t, `eval t = stackmc l1 (compile t)` 












