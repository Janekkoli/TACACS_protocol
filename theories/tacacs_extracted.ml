
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

(** val revapp : uint -> uint -> uint **)

let rec revapp d d' =
  match d with
  | Nil -> d'
  | D0 d0 -> revapp d0 (D0 d')
  | D1 d0 -> revapp d0 (D1 d')
  | D2 d0 -> revapp d0 (D2 d')
  | D3 d0 -> revapp d0 (D3 d')
  | D4 d0 -> revapp d0 (D4 d')
  | D5 d0 -> revapp d0 (D5 d')
  | D6 d0 -> revapp d0 (D6 d')
  | D7 d0 -> revapp d0 (D7 d')
  | D8 d0 -> revapp d0 (D8 d')
  | D9 d0 -> revapp d0 (D9 d')

(** val rev : uint -> uint **)

let rev d =
  revapp d Nil

module Little =
 struct
  (** val succ : uint -> uint **)

  let rec succ = function
  | Nil -> D1 Nil
  | D0 d0 -> D1 d0
  | D1 d0 -> D2 d0
  | D2 d0 -> D3 d0
  | D3 d0 -> D4 d0
  | D4 d0 -> D5 d0
  | D5 d0 -> D6 d0
  | D6 d0 -> D7 d0
  | D7 d0 -> D8 d0
  | D8 d0 -> D9 d0
  | D9 d0 -> D0 (succ d0)
 end

(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val mul : nat -> nat -> nat **)

let rec mul n m =
  match n with
  | O -> O
  | S p -> add m (mul p m)

module Nat =
 struct
  (** val to_little_uint : nat -> uint -> uint **)

  let rec to_little_uint n acc =
    match n with
    | O -> acc
    | S n0 -> to_little_uint n0 (Little.succ acc)

  (** val to_uint : nat -> uint **)

  let to_uint n =
    rev (to_little_uint n (D0 Nil))
 end

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

module NilEmpty =
 struct
  (** val string_of_uint : uint -> char list **)

  let rec string_of_uint = function
  | Nil -> []
  | D0 d0 -> '0'::(string_of_uint d0)
  | D1 d0 -> '1'::(string_of_uint d0)
  | D2 d0 -> '2'::(string_of_uint d0)
  | D3 d0 -> '3'::(string_of_uint d0)
  | D4 d0 -> '4'::(string_of_uint d0)
  | D5 d0 -> '5'::(string_of_uint d0)
  | D6 d0 -> '6'::(string_of_uint d0)
  | D7 d0 -> '7'::(string_of_uint d0)
  | D8 d0 -> '8'::(string_of_uint d0)
  | D9 d0 -> '9'::(string_of_uint d0)
 end

(** val string_of_nat : nat -> char list **)

let string_of_nat n =
  NilEmpty.string_of_uint (Nat.to_uint n)

(** val fact : nat -> nat **)

let rec fact n = match n with
| O -> S O
| S n' -> mul n (fact n')

type auth_request = { version : nat; req_type : char list;
                      username : char list; password : char list; line : 
                      nat }

(** val make_package : auth_request -> char list list **)

let make_package req =
  (append (string_of_nat req.version) (append (' '::[]) req.req_type)) :: (req.username :: (req.password :: (
    (string_of_nat req.line) :: [])))
