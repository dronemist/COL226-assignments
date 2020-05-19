# Type Checking a functional language with definitions and functions
The language in this part was extended to support type checking and functions and definitions.

**Functions** are:
 - Expressions for function abstractions on one variable and function call: `\x.e` \& `e1(e2)`
 
 **Definitions** are :
 - Simple : `def x = e`
 - Sequential : `d1 ; d2`
 - Parallel : `d1 || d2` 
 - Locally Scoped : `local d1 in d2 end`
 
**Type Checking** </br>
 Type is defined as followed. </br>
 ``` ocaml
  Type = Tint | Tunit | Tbool |  t1 * .... * tn | t1 to t2
 ```
Now for a *type-checker* for this language.  
Let **G** be a set of type assumptions on variables.

- The type association `G |- e : t` is provided by
``` ocaml
 val hastype : gamma -> expression -> exptype
```
- Definition yield `G |- d : G[G']` is provided by
``` ocaml
 val yields: gamma -> definition -> gamma
