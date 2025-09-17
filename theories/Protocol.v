(* Core imports *)
From Coq Require Import String Ascii List Arith NArith.
Import ListNotations.

(* For extraction *)
Require Import ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlString.

(* Open scopes *)
Open Scope string_scope.
Open Scope nat_scope.

Require Import Coq.Numbers.DecimalString.
From Coq Require Import Program.Wf String Ascii.
Open Scope string_scope.
Require Import Coq.Numbers.DecimalString.

Require Import Coq.Program.Wf.
Require Import Coq.Arith.Wf_nat.
Require Import Coq.Arith.PeanoNat.
Require Import Coq.micromega.Lia.
Require Import Coq.Program.Wf.
Require Import Coq.Arith.Wf_nat.
(* Definition cR := Ascii.ascii_of_nat 13.
Definition lF := Ascii.ascii_of_nat 10. *)

Definition cR := Ascii true false true true false false false false.
Definition lF := Ascii false true false true false false false false.

Definition cRlF : string :=
  String (cR)
    (String (lF) EmptyString).

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



(* request to string *)

Definition string_of_nat (n : nat) : string :=
  NilEmpty.string_of_uint (Nat.to_uint n).

Definition encode_request (r : request) : string :=
  match r with
  | Auth a =>
      "1 AUTH " ++ Auth.style a ++ cRlF ++
      Auth.username a ++ cRlF ++
      Auth.password a ++ cRlF ++
      string_of_nat (Auth.line a) ++ cRlF

  | Login l =>
      "1 LOGIN" ++ cRlF ++
      Login.username l ++ cRlF ++
      Login.password l ++ cRlF ++
      string_of_nat (Login.line l) ++ cRlF

  | Connect c =>
      "1 CONNECT " ++ Connect.destination_ip c ++ " " ++ string_of_nat (Connect.destination_port c) ++ cRlF ++
      Connect.username c ++ cRlF ++
      Connect.password c ++ cRlF ++
      string_of_nat (Connect.line c) ++ cRlF

  | Superuser s =>
      "1 SUPERUSER" ++ cRlF ++
      Superuser.username s ++ cRlF ++
      Superuser.password s ++ cRlF ++
      string_of_nat (Superuser.line s) ++ cRlF

  | Logout lo =>
      "1 LOGOUT " ++ Logout.reason lo ++ cRlF ++
      Logout.username lo ++ cRlF ++
      Logout.password lo ++ cRlF ++
      string_of_nat (Logout.line lo) ++ cRlF

  | Slipon so =>
      "1 SLIPON " ++ Slipon.slip_address so ++ cRlF ++
      Slipon.username so ++ cRlF ++
      Slipon.password so ++ cRlF ++
      string_of_nat (Slipon.line so) ++ cRlF

  | Slipoff sf =>
      "1 SLIPOFF " ++ Slipoff.reason sf ++ cRlF ++
      Slipoff.username sf ++ cRlF ++
      Slipoff.password sf ++ cRlF ++
      string_of_nat (Slipoff.line sf) ++ cRlF
  end.







(* Function that returns (f,r) - f - first part of s till first CRLF , r - part after CRLF *)
Fixpoint tillFirstcRlF (s : string): string * string:=
  match s with
  | EmptyString => (EmptyString,EmptyString)
  | String (Ascii true false true true false false false false) (String (Ascii false true false true false false false false) rest) => (EmptyString, rest)
  | String c rest => let (f,r) := tillFirstcRlF rest in (String c (f), r)
  end.


(* Takes string and returns list string; parts of s beetween CRLF *)

Program Fixpoint splitincRlF (s:string) {measure (length s)} : list string :=
  match s with
  | EmptyString => []
  | _ => let (f,r) := tillFirstcRlF s in f :: splitincRlF r
  end.

Next Obligation.
  admit.
Admitted.


(* Here i split string in spaces *)

(* Function that returns (f,r) - f - first part of s till first CRLF , r - part after CRLF *)
Fixpoint tillFirstSP (s : string): string * string:=
  match s with
  | EmptyString => (EmptyString,EmptyString)
  | String (Ascii false false false false false true false false) rest => (EmptyString, rest)
  | String c rest => let (f,r) := tillFirstSP rest in (String c (f), r)
  end.


(* Takes string and returns list string; parts of s beetween CRLF *)

Program Fixpoint splitinSP (s:string) {measure (length s)} : list string :=
  match s with
  | EmptyString => []
  | _ => let (f,r) := tillFirstSP s in f :: splitinSP r
  end.

Next Obligation.
  admit.
Admitted.

Fixpoint splitSpacesList (s : list string) : list (list string) :=
  match s with
  | [] => []
  | f :: r => splitinSP f :: splitSpacesList r
  end.


Definition splitonCRLFandSpaces (s:string) : list (list string) :=
  let splitedList := splitincRlF s in splitSpacesList splitedList.





Record response := {
      number : string; 
      text : string
}.
  
  
  Definition encode_response (r : response) : string :=
    number r ++ " " ++ text r ++ cRlF.
  
  
  (* TODO: this sould check if string ends with CRLF and not put it in text *)
  Definition parse_response (s : string) : option response :=
    match s with
    | String a (String b (String c (String (Ascii false false false false false true false false) rest))) => let (f,_) := tillFirstcRlF rest in 
        match a with
        | "2"%char => Some {| number := String a (String b (String c EmptyString)); text := f|}
        | "4"%char => Some {| number := String a (String b (String c EmptyString)); text := f |}
        | "5"%char => Some {| number := String a (String b (String c EmptyString)); text := f |}
        | _ => None
        end
    | _ => None
    end.