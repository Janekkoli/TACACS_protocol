
type nat =
| O
| S of nat



type uint =
| Nil
| D0 of uint
| D1 of uint
| D2 of uint
| D3 of uint
| D4 of uint
| D5 of uint
| D6 of uint
| D7 of uint
| D8 of uint
| D9 of uint

val revapp : uint -> uint -> uint

val rev : uint -> uint

module Little :
 sig
  val succ : uint -> uint
 end

val add : nat -> nat -> nat

val mul : nat -> nat -> nat

module Nat :
 sig
  val to_little_uint : nat -> uint -> uint

  val to_uint : nat -> uint
 end

val append : char list -> char list -> char list

module NilEmpty :
 sig
  val string_of_uint : uint -> char list
 end

val string_of_nat : nat -> char list

val fact : nat -> nat

type auth_request = { version : nat; req_type : char list;
                      username : char list; password : char list; line : 
                      nat }

val make_package : auth_request -> char list list
