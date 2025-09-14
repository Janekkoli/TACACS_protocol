
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

module Nat =
 struct
  (** val to_little_uint : int -> uint -> uint **)

  let rec to_little_uint n0 acc =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> acc)
      (fun n1 -> to_little_uint n1 (Little.succ acc))
      n0

  (** val to_uint : int -> uint **)

  let to_uint n0 =
    rev (to_little_uint n0 (D0 Nil))
 end

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val of_succ_nat : int -> positive **)

  let rec of_succ_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> XH)
      (fun x -> succ (of_succ_nat x))
      n0
 end

module N =
 struct
  (** val of_nat : int -> n **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> N0)
      (fun n' -> Npos (Pos.of_succ_nat n'))
      n0
 end

(** val zero : char **)

let zero = '\000'

(** val one : char **)

let one = '\001'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)

(** val ascii_of_pos : positive -> char **)

let ascii_of_pos =
  let rec loop n0 p =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> zero)
      (fun n' ->
      match p with
      | XI p' -> shift true (loop n' p')
      | XO p' -> shift false (loop n' p')
      | XH -> one)
      n0
  in loop (succ (succ (succ (succ (succ (succ (succ (succ 0))))))))

(** val ascii_of_N : n -> char **)

let ascii_of_N = function
| N0 -> zero
| Npos p -> ascii_of_pos p

(** val ascii_of_nat : int -> char **)

let ascii_of_nat a =
  ascii_of_N (N.of_nat a)

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

(** val cRLF : char list **)

let cRLF =
  (ascii_of_nat (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ
    (succ (succ (succ 0))))))))))))))::((ascii_of_nat (succ (succ (succ (succ
                                          (succ (succ (succ (succ (succ (succ
                                          0)))))))))))::[])

module Auth =
 struct
  type t = { username : char list; password : char list; line : int;
             style : char list }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> int **)

  let line t0 =
    t0.line

  (** val style : t -> char list **)

  let style t0 =
    t0.style
 end

module Login =
 struct
  type t = { username : char list; password : char list; line : int }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> int **)

  let line t0 =
    t0.line
 end

module Connect =
 struct
  type t = { username : char list; password : char list; line : int;
             destination_ip : char list; destination_port : int }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> int **)

  let line t0 =
    t0.line

  (** val destination_ip : t -> char list **)

  let destination_ip t0 =
    t0.destination_ip

  (** val destination_port : t -> int **)

  let destination_port t0 =
    t0.destination_port
 end

module Superuser =
 struct
  type t = { username : char list; password : char list; line : int }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> int **)

  let line t0 =
    t0.line
 end

module Logout =
 struct
  type t = { username : char list; password : char list; line : int;
             reason : char list }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> int **)

  let line t0 =
    t0.line

  (** val reason : t -> char list **)

  let reason t0 =
    t0.reason
 end

module Slipon =
 struct
  type t = { username : char list; password : char list; line : int;
             slip_address : char list }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> int **)

  let line t0 =
    t0.line

  (** val slip_address : t -> char list **)

  let slip_address t0 =
    t0.slip_address
 end

module Slipoff =
 struct
  type t = { username : char list; password : char list; line : int;
             reason : char list }

  (** val username : t -> char list **)

  let username t0 =
    t0.username

  (** val password : t -> char list **)

  let password t0 =
    t0.password

  (** val line : t -> int **)

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

(** val string_of_nat : int -> char list **)

let string_of_nat n0 =
  NilEmpty.string_of_uint (Nat.to_uint n0)

(** val encode_request : request -> char list **)

let encode_request = function
| Auth a ->
  append ('1'::(' '::('A'::('U'::('T'::('H'::(' '::[])))))))
    (append a.Auth.style
      (append cRLF
        (append a.Auth.username
          (append cRLF
            (append a.Auth.password
              (append cRLF (append (string_of_nat a.Auth.line) cRLF)))))))
| Login l ->
  append ('1'::(' '::('L'::('O'::('G'::('I'::('N'::[])))))))
    (append cRLF
      (append l.Login.username
        (append cRLF
          (append l.Login.password
            (append cRLF (append (string_of_nat l.Login.line) cRLF))))))
| Connect c ->
  append
    ('1'::(' '::('C'::('O'::('N'::('N'::('E'::('C'::('T'::(' '::[]))))))))))
    (append c.Connect.destination_ip
      (append (' '::[])
        (append (string_of_nat c.Connect.destination_port)
          (append cRLF
            (append c.Connect.username
              (append cRLF
                (append c.Connect.password
                  (append cRLF (append (string_of_nat c.Connect.line) cRLF)))))))))
| Superuser s ->
  append
    ('1'::(' '::('S'::('U'::('P'::('E'::('R'::('U'::('S'::('E'::('R'::[])))))))))))
    (append cRLF
      (append s.Superuser.username
        (append cRLF
          (append s.Superuser.password
            (append cRLF (append (string_of_nat s.Superuser.line) cRLF))))))
| Logout lo ->
  append ('1'::(' '::('L'::('O'::('G'::('O'::('U'::('T'::(' '::[])))))))))
    (append lo.Logout.reason
      (append cRLF
        (append lo.Logout.username
          (append cRLF
            (append lo.Logout.password
              (append cRLF (append (string_of_nat lo.Logout.line) cRLF)))))))
| Slipon so ->
  append ('1'::(' '::('S'::('L'::('I'::('P'::('O'::('N'::(' '::[])))))))))
    (append so.Slipon.slip_address
      (append cRLF
        (append so.Slipon.username
          (append cRLF
            (append so.Slipon.password
              (append cRLF (append (string_of_nat so.Slipon.line) cRLF)))))))
| Slipoff sf ->
  append
    ('1'::(' '::('S'::('L'::('I'::('P'::('O'::('F'::('F'::(' '::[]))))))))))
    (append sf.Slipoff.reason
      (append cRLF
        (append sf.Slipoff.username
          (append cRLF
            (append sf.Slipoff.password
              (append cRLF (append (string_of_nat sf.Slipoff.line) cRLF)))))))
