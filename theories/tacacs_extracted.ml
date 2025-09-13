
type nat =
| O
| S of nat

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type byte = nat

(** val crlf : byte list **)

let crlf =
  (S (S (S (S (S (S (S (S (S (S (S (S (S O))))))))))))) :: ((S (S (S (S (S (S
    (S (S (S (S O)))))))))) :: [])

(** val space : byte list **)

let space =
  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S O)))))))))))))))))))))))))))))))) :: []

module Auth =
 struct
  type t = { username : char list; password : char list; line : nat;
             style : char list }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> nat **)

  let line t0 =
    t0.line

  (** val style : t -> char list **)

  let style t0 =
    t0.style
 end

module Login =
 struct
  type t = { username : char list; password : char list; line : nat }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> nat **)

  let line t0 =
    t0.line
 end

module Connect =
 struct
  type t = { username : char list; password : char list; line : nat;
             destination_ip : char list; destination_port : nat }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> nat **)

  let line t0 =
    t0.line

  (** val destination_ip : t -> char list **)

  let destination_ip t0 =
    t0.destination_ip

  (** val destination_port : t -> nat **)

  let destination_port t0 =
    t0.destination_port
 end

module Superuser =
 struct
  type t = { username : char list; password : char list; line : nat }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> nat **)

  let line t0 =
    t0.line
 end

module Logout =
 struct
  type t = { username : char list; password : char list; line : nat;
             reason : char list }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> nat **)

  let line t0 =
    t0.line

  (** val reason : t -> char list **)

  let reason t0 =
    t0.reason
 end

module Slipon =
 struct
  type t = { username : char list; password : char list; line : nat;
             slip_address : char list }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> nat **)

  let line t0 =
    t0.line

  (** val slip_address : t -> char list **)

  let slip_address t0 =
    t0.slip_address
 end

module Slipoff =
 struct
  type t = { username : char list; password : char list; line : nat;
             reason : char list }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> nat **)

  let line t0 =
    t0.line

  (** val reason : t -> char list **)

  let reason t0 =
    t0.reason
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

(** val nat_serializable : nat serializable **)

let nat_serializable _ =
  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))) :: ((S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S O)))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: [])

(** val string_serializable : char list serializable **)

let string_serializable _ =
  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S
    O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: ((S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S
    O)))))))))))))))))))))))))))))))))))))))))))))))))))))) :: [])

(** val encode_fields : byte list list -> byte list **)

let rec encode_fields = function
| [] -> []
| x :: xs ->
  (match xs with
   | [] -> x
   | _ :: _ -> app x (app space (encode_fields xs)))

(** val encode_variant : byte -> byte list list -> byte list **)

let encode_variant tag fields =
  app (tag :: []) (app (encode_fields fields) crlf)

(** val encode_request_auto : request -> byte list **)

let encode_request_auto = function
| Auth a ->
  encode_variant O
    ((string_serializable a.Auth.username) :: ((string_serializable
                                                 a.Auth.password) :: (
    (nat_serializable a.Auth.line) :: ((string_serializable a.Auth.style) :: []))))
| Login l ->
  encode_variant (S O)
    ((string_serializable l.Login.username) :: ((string_serializable
                                                  l.Login.password) :: (
    (nat_serializable l.Login.line) :: [])))
| Connect c ->
  encode_variant (S (S O))
    ((string_serializable c.Connect.username) :: ((string_serializable
                                                    c.Connect.password) :: (
    (nat_serializable c.Connect.line) :: ((string_serializable
                                            c.Connect.destination_ip) :: (
    (nat_serializable c.Connect.destination_port) :: [])))))
| Superuser s ->
  encode_variant (S (S (S O)))
    ((string_serializable s.Superuser.username) :: ((string_serializable
                                                      s.Superuser.password) :: (
    (nat_serializable s.Superuser.line) :: [])))
| Logout lo ->
  encode_variant (S (S (S (S O))))
    ((string_serializable lo.Logout.username) :: ((string_serializable
                                                    lo.Logout.password) :: (
    (nat_serializable lo.Logout.line) :: ((string_serializable
                                            lo.Logout.reason) :: []))))
| Slipon so ->
  encode_variant (S (S (S (S (S O)))))
    ((string_serializable so.Slipon.username) :: ((string_serializable
                                                    so.Slipon.password) :: (
    (nat_serializable so.Slipon.line) :: ((string_serializable
                                            so.Slipon.slip_address) :: []))))
| Slipoff sf ->
  encode_variant (S (S (S (S (S (S O))))))
    ((string_serializable sf.Slipoff.username) :: ((string_serializable
                                                     sf.Slipoff.password) :: (
    (nat_serializable sf.Slipoff.line) :: ((string_serializable
                                             sf.Slipoff.reason) :: []))))
