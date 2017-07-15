
Require Import AST.
Require Import State.

Require Import Arith.
Require Import Nat.


Module Export Exec.


(* Big-step evaluation semantics for statements, conditionals,
   arithmetic, and values. *)
Inductive exec_statement : statement -> state -> state -> Prop :=
  (* Labels are noops. *)
  | ExecLabel (lbl : label) (st : state) :
      exec_statement (Label lbl) st (step_program st)
  | ExecAssign (e1 e2 : arith) (st : state) (loc n : nat) :
      eval_arith e1 st loc -> eval_arith e2 st n
      -> exec_statement (Assign e1 e2) st (step_program (set_memory loc n st))
  | ExecJIFFalse (e : cond) (v : value) (st : state) :
      eval_cond e st false
      -> exec_statement (JIF e v) st (step_program st)
  | ExecJIFValid (e : cond) (v : value) (st : state)
      (loc : nat) (valid : valid_loc loc st) :
        eval_cond e st true -> eval_value v st loc
        -> exec_statement (JIF e v) st (set_code_loc loc st)
  (* This rule states that jumping to an invalid location in the code
     results in a completely nondeterministic state. This rule is
     included to open up the possibility of compiling to assembly,
     were jumping to an arbitrary address would lead to
     a nondeterministic state. *)
  | ExecJIFInvalid (e : cond) (v : value) (st : state)
      (loc : nat) (valid : ~(valid_loc loc st)) :
        eval_cond e st true -> eval_value v st loc ->
        forall (st' : state), exec_statement (JIF e v) st st'
  (* Here the value stored is nondeterministic. *)
  | ExecRead (e : arith) (st : state) (loc : nat) :
      forall (n : nat), eval_arith e st loc
      -> exec_statement (Read e) st (step_program (set_memory loc n st))
  (* As far as execution is concerned, print statements are noops. *)
  | ExecPrint (v : value) (st : state) (n : nat) :
      eval_value v st n -> exec_statement (Print v) st (step_program st)
with eval_cond : cond -> state -> bool -> Prop :=
  | EvalAnd (e1 e2 : cond) (st : state) (b1 b2 : bool) :
      eval_cond e1 st b1 -> eval_cond e2 st b2
      -> eval_cond (And e1 e2) st (andb b1 b2)
  | EvalNot (e : cond) (st : state) (b : bool) :
      eval_cond e st b -> eval_cond (Not e) st (negb b)
  | EvalLT (e1 e2 : arith) (st : state) (n m : nat) :
      eval_arith e1 st n -> eval_arith e2 st m
      -> eval_cond (LT e1 e2) st (ltb n m)
with eval_arith : arith -> state -> nat -> Prop :=
  | EvalVal (v : value) (st : state) (n : nat) :
      eval_value v st n -> eval_arith (Val v) st n
  | EvalAdd (e1 e2 : arith) (st : state) (n m : nat) :
      eval_arith e1 st n -> eval_arith e2 st m
      -> eval_arith (Add e1 e2) st (n + m)
  | EvalSub (e1 e2 : arith) (st : state) (n m : nat) :
      eval_arith e1 st n -> eval_arith e2 st m
      -> eval_arith (Sub e1 e2) st (n - m)
with eval_value : value -> state -> nat -> Prop :=
  | EvalNum (n : nat) (st : state) : eval_value (Num n) st n
  | EvalRef (expr : arith) (st : state) (loc : nat) :
      eval_arith expr st loc -> eval_value (Ref expr) st (get_memory loc st)
  | EvalLoc (l : label) (st : state) (valid : valid_label l st) :
      eval_value (Location l) st (get_label l st valid).


Definition nth_lt {A : Type} (n : nat) (xs : list A) (ev : n < length xs) : A.
  induction n.
  induction xs.
  
  - simpl in ev. exfalso. inversion ev.
  
  - exact a.
  
  - apply IHn.
    apply le_Sn_le.
    exact ev.
Qed.

(* This proposition represents the a single execution step of a
   program from one state to the next. *)
Inductive step_program : state -> state -> Prop :=
  Step (st st' : state) (ev : st.(code_loc) < (length st.(prog))) :
    exec_statement (nth_lt st.(code_loc) st.(prog) ev) st st'
    -> step_program st st'.
    
    
End Exec.