(* factorial.v *)

(* Core imports *)
From Coq Require Import String Ascii List Arith NArith.
Import ListNotations.

(* For extraction *)
Require Import ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlString.

(* Open scopes *)
Open Scope string_scope.
Open Scope nat_scope.

Definition CRLF : string :=
  String (Ascii.ascii_of_nat 13)
    (String (Ascii.ascii_of_nat 10) EmptyString).


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



(* byte to request *)

Require Import Coq.Numbers.DecimalString.

Definition string_of_nat (n : nat) : string :=
  NilEmpty.string_of_uint (Nat.to_uint n).

Definition encode_request (r : request) : string :=
  match r with
  | Auth a =>
      "1 AUTH " ++ Auth.style a ++ CRLF ++
      Auth.username a ++ CRLF ++
      Auth.password a ++ CRLF ++
      string_of_nat (Auth.line a) ++ CRLF

  | Login l =>
      "1 LOGIN" ++ CRLF ++
      Login.username l ++ CRLF ++
      Login.password l ++ CRLF ++
      string_of_nat (Login.line l) ++ CRLF

  | Connect c =>
      "1 CONNECT " ++ Connect.destination_ip c ++ " " ++
      string_of_nat (Connect.destination_port c) ++ CRLF ++
      Connect.username c ++ CRLF ++
      Connect.password c ++ CRLF ++
      string_of_nat (Connect.line c) ++ CRLF

  | Superuser s =>
      "1 SUPERUSER" ++ CRLF ++
      Superuser.username s ++ CRLF ++
      Superuser.password s ++ CRLF ++
      string_of_nat (Superuser.line s) ++ CRLF

  | Logout lo =>
      "1 LOGOUT " ++ Logout.reason lo ++ CRLF ++
      Logout.username lo ++ CRLF ++
      Logout.password lo ++ CRLF ++
      string_of_nat (Logout.line lo) ++ CRLF

  | Slipon so =>
      "1 SLIPON " ++ Slipon.slip_address so ++ CRLF ++
      Slipon.username so ++ CRLF ++
      Slipon.password so ++ CRLF ++
      string_of_nat (Slipon.line so) ++ CRLF

  | Slipoff sf =>
      "1 SLIPOFF " ++ Slipoff.reason sf ++ CRLF ++
      Slipoff.username sf ++ CRLF ++
      Slipoff.password sf ++ CRLF ++
      string_of_nat (Slipoff.line sf) ++ CRLF
  end.


(* Extraction stuff *)
(* Require Extraction.

Set Extraction Output Directory "../../theories".
Extraction Language OCaml.
Extraction "tacacs_extracted.ml" request encode_request_auto.  *)
