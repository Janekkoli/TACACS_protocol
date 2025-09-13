
type nat =
| O
| S of nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type byte = nat

val crlf : byte list

val space : byte list

module Auth :
 sig
  type t = { username : char list; password : char list; line : nat;
             style : char list }

  val username : t -> char list

  val password : t -> char list

  val line : t -> nat

  val style : t -> char list
 end

module Login :
 sig
  type t = { username : char list; password : char list; line : nat }

  val username : t -> char list

  val password : t -> char list

  val line : t -> nat
 end

module Connect :
 sig
  type t = { username : char list; password : char list; line : nat;
             destination_ip : char list; destination_port : nat }

  val username : t -> char list

  val password : t -> char list

  val line : t -> nat

  val destination_ip : t -> char list

  val destination_port : t -> nat
 end

module Superuser :
 sig
  type t = { username : char list; password : char list; line : nat }

  val username : t -> char list

  val password : t -> char list

  val line : t -> nat
 end

module Logout :
 sig
  type t = { username : char list; password : char list; line : nat;
             reason : char list }

  val username : t -> char list

  val password : t -> char list

  val line : t -> nat

  val reason : t -> char list
 end

module Slipon :
 sig
  type t = { username : char list; password : char list; line : nat;
             slip_address : char list }

  val username : t -> char list

  val password : t -> char list

  val line : t -> nat

  val slip_address : t -> char list
 end

module Slipoff :
 sig
  type t = { username : char list; password : char list; line : nat;
             reason : char list }

  val username : t -> char list

  val password : t -> char list

  val line : t -> nat

  val reason : t -> char list
 end

type request =
| Auth of Auth.t
| Login of Login.t
| Connect of Connect.t
| Superuser of Superuser.t
| Logout of Logout.t
| Slipon of Slipon.t
| Slipoff of Slipoff.t

type 'a serializable =
  'a -> byte list
  (* singleton inductive, whose constructor was Build_Serializable *)

val nat_serializable : nat serializable

val string_serializable : char list serializable

val encode_fields : byte list list -> byte list

val encode_variant : byte -> byte list list -> byte list

val encode_request_auto : request -> byte list
