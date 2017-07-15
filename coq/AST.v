
Require Import List.

Module Export AST.


(* Instead of strings, represent labels as natural numbers. *)
Definition label := nat.

(* The AST for MImp. Note that extraneous operations like |, =, >, *, and /
   have been removed, as well as the jmp statement. This makes the
   theory easier. These operations can be represented in terms of the
   ones present. *)
Inductive statement : Set :=
  | Label : label -> statement
  | Assign : arith -> arith -> statement
  | JIF : cond -> value -> statement
  | Read : arith -> statement
  | Print : value -> statement
with cond : Set :=
  | And : cond -> cond -> cond
  | Not : cond -> cond
  | LT : arith -> arith -> cond
with arith : Set :=
  | Val : value -> arith
  | Add : arith -> arith -> arith
  | Sub : arith -> arith -> arith
with value : Set :=
  | Num : nat -> value
  | Ref : arith -> value
  | Location : label -> value.
  
Definition program := list statement.

End AST.
