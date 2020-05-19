# COL226-assignments
Assignments for the course COL226 under Prof Sanjeeva Prasad, JAN-APRIL 2019
## Assignments 
0) _[Assignment 0](Assignment%200/README.md) . A Bigint package_ </br>
  - In this assignment, I wrote a BIGNUM package in OCaml where I implemented arithmetic for arbitrarily large numbers, using lists of digits to implement an integer.

1) _[Assignment 1](Assignment%201/README.md): A simple definitional interpreter and stack machine_ </br>
  - In this assignment, I modelled the "abstract syntax" of a simple calculator language for integer expressions, and give it "semantic meaning" in terms of OCaml's built-in types. 
  - In the second part, I implemented the calculator as a simple stack-based machine.

2) _[Assignment 2](Assignment%202/README.md): Building a scanner using OCaml-Lex_
  - In this assignment, I created a lexer using Ocaml-lex.

3) _[Assignment 3](Assignment%203/README.md): Parsing for a simple expression evaluator_
  - In this assignment, I designed a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS). The language contain the following types of expressions:  `integers and booleans`

4) _[Assignment 4](Assignment%204/README.md): Type-checking a functional language with Definitions and functions_
  - Extended the language of Assignment 3 with definitions and first-class functions

5) _[Assignment 5](Assignment%205/README.md): CBV and CBN Interpreters for a tiny functional language_
  - In this assignment I implemented **krivine** machine (call by name) and the **SECD** machine (call by value for a tiny functional language)

6) _[Assignment 6](Assignment%206): A simulator for nested procedure calls_
  - In this assignment I implemented the core ideas (not the actual layout for any given architecture or language/compiler, and not the actual code generation) for understanding implementations of the (static/lexical) scoping discipline in Algol-like languages, particularly the visibility rules and the set-up/tear-down involved in procedure call and return
