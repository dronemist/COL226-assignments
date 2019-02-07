# A simple definitional interpreter and stack machine
## Setup
Keep a0.mli,a1.mli,a0.ml,a1.ml and .ocamlinit in the same folder. Run the following command from the terminal 
```
ocamlbuild a1.cmo a0.cmo
```
This will create a _build directory and then the functions can be used from ocaml top level

## Components
**The abstract syntax** is characterised by the type
```
<span style="color:red">type</span> <span style="color:cyan">exptree</span> = <span style="color:green"> N </span>of int  
|<span style="color:green"> Plus</span> of <span style="color:cyan">exptree *  exptree</span>  
|<span style="color:green"> Minus</span> of <span style="color:cyan">exptree *  exptree</span>  
|<span style="color:green"> Mult</span> of <span style="color:cyan">exptree *  exptree</span>  
|<span style="color:green"> Div</span> of <span style="color:cyan">exptree *  exptree</span>  
|<span style="color:green"> Rem</span> of <span style="color:cyan">exptree *  exptree</span>  
|<span style="color:green"> Neg</span> of <span style="color:cyan"> exptree</span>  
|<span style="color:green"> Abs</span> of <span style="color:cyan"> exptree</span>  
;;
```
**The definitional interpreter** is defined as a function 
```
<span style="color:green"> eval </span> :<span style="color:cyan"> exptree -> int</span>  
```
**















