
Require Import AST.

Require Import Arith.
Require Import List.
Import ListNotations.


Module Export State.

(* A location in MImp code. *)
Definition location := nat.

Record state : Set := MkState {
  prog : program ;
  memory : list nat ;
  label_locs : list (label * location) ;
  code_loc : location ;
}.


Definition step_program (st : state) : state := match st with
  MkState p mem lls cl => MkState p mem lls (S cl)
end.


Definition get_memory' (n : nat) (memory : list nat) :=
  nth n memory 0.
  
Fixpoint set_memory' (n : nat) (x : nat) (memory : list nat) : list nat := 
  match n, memory with
    | 0, [] => [x]
    | S m, [] => 0 :: (set_memory' m x [])
    | 0, (_ :: ys) => x :: ys
    | S m, (y :: ys) => y :: (set_memory' m x ys)
  end.
  
Definition get_memory (n : nat) (st : state) := get_memory' n (st.(memory)).

Definition set_memory (n : nat) (x : nat) (st : state) := match st with
  MkState p mem lls cl => MkState p (set_memory' n x mem) lls cl
end.


Definition valid_label (l : label) (st : state) :=
  In l (map fst st.(label_locs)).
  
Definition valid_loc (loc : location) (st : state) :=
  In loc (map snd st.(label_locs)).
  
  
Theorem or_not {A B : Prop} (neg : ~A) (ev : A \/ B) : B.
  destruct ev ; try assumption. exfalso. auto.
Qed.
  
Definition get_label (l : label) (st : state) (valid : valid_label l st)
  : location.

  unfold valid_label in valid.
  set (lls := label_locs st) in valid.
  induction lls.
  
  - simpl in valid. auto.
  
  - simpl in valid.
    destruct (eq_nat_dec (fst a) l).
    
    * exact (snd a).
    * apply (or_not n) in valid.
      apply IHlls.
      exact valid.
Defined.


Definition set_code_loc (l : location) (st : state) : state :=
  match st with MkState p mem lls _ => MkState p mem lls l end.


(* Builds a list of labels and their corresponding locations. *)
Fixpoint build_label_locs (prog : program) (l : location)
  (lls : list (label * location)) : list (label * location) :=
  match prog with
    | [] => lls
    | (Label lbl) :: prog' => build_label_locs prog' (S l) ((lbl, l) :: lls)
    | _ :: prog' => build_label_locs prog' (S l) lls
  end.

(* Creates an initial state from the starting program. *)
Definition init_state (prog : program) : state :=
  MkState prog [] (build_label_locs prog 0 []) 0.


End State.