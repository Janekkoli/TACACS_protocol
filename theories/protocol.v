(* factorial.v *)

Require Import Arith.

Fixpoint fact (n : nat) : nat :=
  match n with
  | O => 1
  | S n' => n * fact n'
  end.

(* Example: check fact 5 = 120 *)
Compute fact 5.