# Big Int package for Ocaml
The Big Int package contains the following functions
## Arithmetic operators
### Addition.
Type: add: bigint -> bigint -> bigint
Function call:
```
add b1 b2;;
```
### Multiplication.
Type: mult: bigint -> bigint -> bigint
Function call:
```
mult b1 b2;;
```
### Subtraction.
Type: sub: bigint -> bigint -> bigint
Function call:
```
sub b1 b2;;
```
### Quotient:
Type: div: bigint -> bigint -> bigint
Function call:
```
div b1 b2;;
```
### Remainder.
Type: sub: bigint -> bigint -> bigint
Function call:
```
rem b1 b2;;
``` 
### Unary negation.
Type: minus: bigint -> bigint
Function call:
```
minus b1;;
```
### Absolute value.  
Type: bigint -> bigint
Function call:
```
abs b1;;
```
## Comparison operations:
### Equal.   
Type: eq: bigint -> bigint -> bool
Function call:
```
eq b1 b2;;
```
### Greater_than.  
Type: gt:  bigint -> bigint -> bool
Function call:
```
gt b1 b2;;
```
### Less_than.  
Type: lt:  bigint -> bigint -> bool
Function call:
```
lt b1 b2;;
```
### Great_or_equal.  
Type: geq:  bigint -> bigint -> bool
Function call:
```
geq b1 b2;;
```
### Less_or_equal.  
Type: leq:  bigint -> bigint -> bool
Function call:
```
leq b1 b2;;
```
## Functions to present the result in the form of a string. 
### print_num:  
Type: print_num:bigint -> string
Function call:
```
print_num b1;;
```
## Conversion functions from OCaml int to bigint.
### mk_big:
Type: mk_big: int -> bigint
Function call:
```
mk_big num;;
```

















