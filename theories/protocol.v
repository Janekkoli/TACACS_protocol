(* factorial.v *)

Require Import Arith.
Require Import Coq.NArith.NArith.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.
Require Import Coq.Numbers.DecimalString.
From Coq Require Import String Ascii List.
Import ListNotations.
Require Import   ExtrOcamlString.

Open Scope string_scope.

From Coq Require Import String NArith.
Open Scope string_scope.

Definition string_of_nat (n : nat) : string :=
  NilEmpty.string_of_uint (Nat.to_uint n).


Fixpoint fact (n : nat) : nat :=
  match n with
  | O => 1
  | S n' => n * fact n'
  end.




Record auth_request := {
  version : nat;
  req_type : string;
  username : string;
  password : string;
  line     : nat
}.


Definition make_package (req : auth_request) : list string :=
  [ string_of_nat req.(version) ++ " " ++ req.(req_type) ;
    req.(username) ;
    req.(password) ;
    string_of_nat req.(line) ].




(* Force string to be extracted as OCaml string *)
(* Extract Inductive string => "string" [ """" "(^)" ].
Extract Constant append => "(^)".  *)

Require Extraction.
Set Extraction Output Directory "../../theories".
Extraction Language OCaml.
Extraction "tacacs_extracted.ml" fact auth_request make_package. 
