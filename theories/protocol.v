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
From Coq Require Import String Ascii List Arith.
Import ListNotations.
Open Scope string_scope.

Open Scope string_scope.

From Coq Require Import String NArith.
Open Scope string_scope.

Definition byte := nat.  (* 0..255 *)

From Coq Require Import List String Ascii.
Import ListNotations.

Definition crlf : list byte := [13;10].
Definition space : list byte := [32].

(* Definitions *)
Module Auth.
  Record t := {
    username : string;
    password : string;
    line : nat;
    style : string
  }.
End Auth.

Module Login.
  Record t := {
    username : string;
    password : string;
    line : nat
  }.
End Login.

Module Connect.
  Record t := {
    username : string;
    password : string;
    line : nat;
    destination_ip : string;
    destination_port : nat
  }.
End Connect.

Module Superuser.
  Record t := {
    username : string;
    password : string;
    line : nat
  }.
End Superuser.

Module Logout.
  Record t := {
    username : string;
    password : string;
    line : nat;
    reason : string
  }.
End Logout.

Module Slipon.
  Record t := {
    username : string;
    password : string;
    line : nat;
    slip_address : string
  }.
End Slipon.

Module Slipoff.
  Record t := {
    username : string;
    password : string;
    line : nat;
    reason : string
  }.
End Slipoff.


Inductive request :=
| Auth (a : Auth.t)
| Login (l : Login.t)
| Connect (c : Connect.t)
| Superuser (s : Superuser.t)
| Logout (lo : Logout.t)
| Slipon (so : Slipon.t)
| Slipoff (sf : Slipoff.t).



(* Request to byte list *)

Class Serializable (A : Type) := {
  encode : A -> list byte
}.

Instance nat_serializable : Serializable nat := {
  encode s := [45;56]
}.

Instance string_serializable : Serializable string := {
  encode s := [78;54] (*List.map (fun c => Nat.of_ascii c) (list_ascii_of_string s)*)
}.

Fixpoint encode_fields (ll : list (list byte)) : list byte :=
  match ll with
  | [] => []
  | [x] => x           (* last element: no trailing space *)
  | x :: xs => x ++ space ++ encode_fields xs
  end.

(* Definition encode_record {A} `{Serializable A} (fields : list A) : list byte :=
  encode_fields fields ++ crlf.  CRLF *)

Definition encode_variant (tag : byte) (fields : list (list byte)) : list byte :=
  [tag] ++ encode_fields fields ++ crlf.


Definition encode_request_auto (r : request) : list byte :=
  match r with
  | Auth a      => encode_variant 0 [encode (Auth.username a); encode (Auth.password a); encode (Auth.line a); encode (Auth.style a)]
  | Login l     => encode_variant 1 [encode (Login.username l); encode (Login.password l); encode (Login.line l)]
  | Connect c   => encode_variant 2 [encode (Connect.username c); encode (Connect.password c); encode (Connect.line c); encode (Connect.destination_ip c); encode (Connect.destination_port c)]
  | Superuser s => encode_variant 3 [encode (Superuser.username s); encode (Superuser.password s); encode (Superuser.line s)]
  | Logout lo   => encode_variant 4 [encode (Logout.username lo); encode (Logout.password lo); encode (Logout.line lo); encode (Logout.reason lo)]
  | Slipon so   => encode_variant 5 [encode (Slipon.username so); encode (Slipon.password so); encode (Slipon.line so); encode (Slipon.slip_address so)]
  | Slipoff sf  => encode_variant 6 [encode (Slipoff.username sf); encode (Slipoff.password sf); encode (Slipoff.line sf); encode (Slipoff.reason sf)]
  end.



(* byte to request *)



(* Extraction stuff *)
Require Extraction.

Set Extraction Output Directory "../../theories".
Extraction Language OCaml.
Extraction "tacacs_extracted.ml" request encode_request_auto. 
