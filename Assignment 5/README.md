# CBV and CBN Interpreters for a tiny functional language
For our language, the following abtract machines were implemented:

- **The Krivine Machine** *(in closure form)*, that implements **Call-by-Name** semantics.
    
```ocaml
  val krivine_compile : abstract_tree -> compiled list
  val krivine : stack -> environment -> compiled list -> answer_closure
 ```
 
 - **The SECD Machine** that implements **Call-by-Value** semantics.
 
```ocaml
  val secd_compile : abstract_tree -> compiled list
  val secd : stack -> environment -> compiled list -> dump -> answer
 ```
