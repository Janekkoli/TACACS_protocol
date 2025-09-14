
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

module Nat :
 sig
  val to_little_uint : int -> uint -> uint

  val to_uint : int -> uint
 end

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Pos :
 sig
  val succ : positive -> positive

  val of_succ_nat : int -> positive
 end

module N :
 sig
  val of_nat : int -> n
 end

val zero : char

val one : char

val shift : bool -> char -> char

val ascii_of_pos : positive -> char

val ascii_of_N : n -> char

val ascii_of_nat : int -> char

val append : char list -> char list -> char list

module NilEmpty :
 sig
  val string_of_uint : uint -> char list
 end

val cRLF : char list

module Auth :
 sig
  type t = { username : char list; password : char list; line : int;
             style : char list }

  val username : t -> char list

  val password : t -> char list

  val line : t -> int

  val style : t -> char list
 end

module Login :
 sig
  type t = { username : char list; password : char list; line : int }

  val username : t -> char list

  val password : t -> char list

  val line : t -> int
 end

module Connect :
 sig
  type t = { username : char list; password : char list; line : int;
             destination_ip : char list; destination_port : int }

  val username : t -> char list

  val password : t -> char list

  val line : t -> int

  val destination_ip : t -> char list

  val destination_port : t -> int
 end

module Superuser :
 sig
  type t = { username : char list; password : char list; line : int }

  val username : t -> char list

  val password : t -> char list

  val line : t -> int
 end

module Logout :
 sig
  type t = { username : char list; password : char list; line : int;
             reason : char list }

  val username : t -> char list

  val password : t -> char list

  val line : t -> int

  val reason : t -> char list
 end

module Slipon :
 sig
  type t = { username : char list; password : char list; line : int;
             slip_address : char list }

  val username : t -> char list

  val password : t -> char list

  val line : t -> int

  val slip_address : t -> char list
 end

module Slipoff :
 sig
  type t = { username : char list; password : char list; line : int;
             reason : char list }

  val username : t -> char list

  val password : t -> char list

  val line : t -> int

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

val string_of_nat : int -> char list

val encode_request : request -> char list
