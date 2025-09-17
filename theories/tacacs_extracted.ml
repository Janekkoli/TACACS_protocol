
type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)

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

  let rec to_little_uint n acc =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> acc)
      (fun n0 -> to_little_uint n0 (Little.succ acc))
      n

  (** val to_uint : int -> uint **)

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

(** val fix_F_sub : ('a1 -> ('a1 -> 'a2) -> 'a2) -> 'a1 -> 'a2 **)

let rec fix_F_sub f_sub x =
  f_sub x (fun x0 -> fix_F_sub f_sub x0)

(** val fix_sub : ('a1 -> ('a1 -> 'a2) -> 'a2) -> 'a1 -> 'a2 **)

let fix_sub =
  fix_F_sub

(** val cR : char **)

let cR =
  '\r'

(** val lF : char **)

let lF =
  '\n'

(** val cRlF : char list **)

let cRlF =
  cR::(lF::[])

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

let string_of_nat n =
  NilEmpty.string_of_uint (Nat.to_uint n)

(** val encode_request : request -> char list **)

let encode_request = function
| Auth a ->
  append ('1'::(' '::('A'::('U'::('T'::('H'::(' '::[])))))))
    (append a.Auth.style
      (append cRlF
        (append a.Auth.username
          (append cRlF
            (append a.Auth.password
              (append cRlF (append (string_of_nat a.Auth.line) cRlF)))))))
| Login l ->
  append ('1'::(' '::('L'::('O'::('G'::('I'::('N'::[])))))))
    (append cRlF
      (append l.Login.username
        (append cRlF
          (append l.Login.password
            (append cRlF (append (string_of_nat l.Login.line) cRlF))))))
| Connect c ->
  append
    ('1'::(' '::('C'::('O'::('N'::('N'::('E'::('C'::('T'::(' '::[]))))))))))
    (append c.Connect.destination_ip
      (append (' '::[])
        (append (string_of_nat c.Connect.destination_port)
          (append cRlF
            (append c.Connect.username
              (append cRlF
                (append c.Connect.password
                  (append cRlF (append (string_of_nat c.Connect.line) cRlF)))))))))
| Superuser s ->
  append
    ('1'::(' '::('S'::('U'::('P'::('E'::('R'::('U'::('S'::('E'::('R'::[])))))))))))
    (append cRlF
      (append s.Superuser.username
        (append cRlF
          (append s.Superuser.password
            (append cRlF (append (string_of_nat s.Superuser.line) cRlF))))))
| Logout lo ->
  append ('1'::(' '::('L'::('O'::('G'::('O'::('U'::('T'::(' '::[])))))))))
    (append lo.Logout.reason
      (append cRlF
        (append lo.Logout.username
          (append cRlF
            (append lo.Logout.password
              (append cRlF (append (string_of_nat lo.Logout.line) cRlF)))))))
| Slipon so ->
  append ('1'::(' '::('S'::('L'::('I'::('P'::('O'::('N'::(' '::[])))))))))
    (append so.Slipon.slip_address
      (append cRlF
        (append so.Slipon.username
          (append cRlF
            (append so.Slipon.password
              (append cRlF (append (string_of_nat so.Slipon.line) cRlF)))))))
| Slipoff sf ->
  append
    ('1'::(' '::('S'::('L'::('I'::('P'::('O'::('F'::('F'::(' '::[]))))))))))
    (append sf.Slipoff.reason
      (append cRlF
        (append sf.Slipoff.username
          (append cRlF
            (append sf.Slipoff.password
              (append cRlF (append (string_of_nat sf.Slipoff.line) cRlF)))))))

(** val tillFirstcRlF : char list -> char list * char list **)

let rec tillFirstcRlF = function
| [] -> ([], [])
| c::rest ->
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun b b0 b1 b2 b3 b4 b5 b6 ->
    if b
    then if b0
         then let (f, r) = tillFirstcRlF rest in ((c::f), r)
         else if b1
              then if b2
                   then if b3
                        then let (f, r) = tillFirstcRlF rest in ((c::f), r)
                        else if b4
                             then let (f, r) = tillFirstcRlF rest in
                                  ((c::f), r)
                             else if b5
                                  then let (f, r) = tillFirstcRlF rest in
                                       ((c::f), r)
                                  else if b6
                                       then let (f, r) = tillFirstcRlF rest in
                                            ((c::f), r)
                                       else (match rest with
                                             | [] ->
                                               let (f, r) = tillFirstcRlF rest
                                               in
                                               ((c::f), r)
                                             | a::rest0 ->
                                               (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                 (fun b7 b8 b9 b10 b11 b12 b13 b14 ->
                                                 if b7
                                                 then let (f, r) =
                                                        tillFirstcRlF rest
                                                      in
                                                      ((c::f), r)
                                                 else if b8
                                                      then if b9
                                                           then let (
                                                                  f, r) =
                                                                  tillFirstcRlF
                                                                    rest
                                                                in
                                                                ((c::f), r)
                                                           else if b10
                                                                then 
                                                                  if b11
                                                                  then 
                                                                    let (
                                                                    f, r) =
                                                                    tillFirstcRlF
                                                                    rest
                                                                    in
                                                                    ((c::f),
                                                                    r)
                                                                  else 
                                                                    if b12
                                                                    then 
                                                                    let (
                                                                    f, r) =
                                                                    tillFirstcRlF
                                                                    rest
                                                                    in
                                                                    ((c::f),
                                                                    r)
                                                                    else 
                                                                    if b13
                                                                    then 
                                                                    let (
                                                                    f, r) =
                                                                    tillFirstcRlF
                                                                    rest
                                                                    in
                                                                    ((c::f),
                                                                    r)
                                                                    else 
                                                                    if b14
                                                                    then 
                                                                    let (
                                                                    f, r) =
                                                                    tillFirstcRlF
                                                                    rest
                                                                    in
                                                                    ((c::f),
                                                                    r)
                                                                    else 
                                                                    ([],
                                                                    rest0)
                                                                else 
                                                                  let (
                                                                    f, r) =
                                                                    tillFirstcRlF
                                                                    rest
                                                                  in
                                                                  ((c::f), r)
                                                      else let (f, r) =
                                                             tillFirstcRlF
                                                               rest
                                                           in
                                                           ((c::f), r))
                                                 a)
                   else let (f, r) = tillFirstcRlF rest in ((c::f), r)
              else let (f, r) = tillFirstcRlF rest in ((c::f), r)
    else let (f, r) = tillFirstcRlF rest in ((c::f), r))
    c

(** val splitincRlF : char list -> char list list **)

let splitincRlF =
  fix_sub (fun recarg splitincRlF' ->
    match recarg with
    | [] -> []
    | _::_ -> let (f, r) = tillFirstcRlF recarg in f :: (splitincRlF' r))

(** val tillFirstSP : char list -> char list * char list **)

let rec tillFirstSP = function
| [] -> ([], [])
| c::rest ->
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun b b0 b1 b2 b3 b4 b5 b6 ->
    if b
    then let (f, r) = tillFirstSP rest in ((c::f), r)
    else if b0
         then let (f, r) = tillFirstSP rest in ((c::f), r)
         else if b1
              then let (f, r) = tillFirstSP rest in ((c::f), r)
              else if b2
                   then let (f, r) = tillFirstSP rest in ((c::f), r)
                   else if b3
                        then let (f, r) = tillFirstSP rest in ((c::f), r)
                        else if b4
                             then if b5
                                  then let (f, r) = tillFirstSP rest in
                                       ((c::f), r)
                                  else if b6
                                       then let (f, r) = tillFirstSP rest in
                                            ((c::f), r)
                                       else ([], rest)
                             else let (f, r) = tillFirstSP rest in ((c::f), r))
    c

(** val splitinSP : char list -> char list list **)

let splitinSP =
  fix_sub (fun recarg splitinSP' ->
    match recarg with
    | [] -> []
    | _::_ -> let (f, r) = tillFirstSP recarg in f :: (splitinSP' r))

(** val splitSpacesList : char list list -> char list list list **)

let rec splitSpacesList = function
| [] -> []
| f :: r -> (splitinSP f) :: (splitSpacesList r)

(** val splitonCRLFandSpaces : char list -> char list list list **)

let splitonCRLFandSpaces s =
  let splitedList = splitincRlF s in splitSpacesList splitedList

type response = { number : char list; text : char list }

(** val encode_response : response -> char list **)

let encode_response r =
  append r.number (append (' '::[]) (append r.text cRlF))

(** val parse_response : char list -> response option **)

let parse_response = function
| [] -> None
| a::s0 ->
  (match s0 with
   | [] -> None
   | b::s1 ->
     (match s1 with
      | [] -> None
      | c::s2 ->
        (match s2 with
         | [] -> None
         | a0::rest ->
           (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b0 b1 b2 b3 b4 b5 b6 b7 ->
             if b0
             then None
             else if b1
                  then None
                  else if b2
                       then None
                       else if b3
                            then None
                            else if b4
                                 then None
                                 else if b5
                                      then if b6
                                           then None
                                           else if b7
                                                then None
                                                else let (f, _) =
                                                       tillFirstcRlF rest
                                                     in
                                                     (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                       (fun b8 b9 b10 b11 b12 b13 b14 b15 ->
                                                       if b8
                                                       then if b9
                                                            then None
                                                            else if b10
                                                                 then 
                                                                   if b11
                                                                   then None
                                                                   else 
                                                                    if b12
                                                                    then 
                                                                    if b13
                                                                    then 
                                                                    if b14
                                                                    then None
                                                                    else 
                                                                    if b15
                                                                    then None
                                                                    else 
                                                                    Some
                                                                    { number =
                                                                    (a::(b::(c::[])));
                                                                    text = f }
                                                                    else None
                                                                    else None
                                                                 else None
                                                       else if b9
                                                            then if b10
                                                                 then None
                                                                 else 
                                                                   if b11
                                                                   then None
                                                                   else 
                                                                    if b12
                                                                    then 
                                                                    if b13
                                                                    then 
                                                                    if b14
                                                                    then None
                                                                    else 
                                                                    if b15
                                                                    then None
                                                                    else 
                                                                    Some
                                                                    { number =
                                                                    (a::(b::(c::[])));
                                                                    text = f }
                                                                    else None
                                                                    else None
                                                            else if b10
                                                                 then 
                                                                   if b11
                                                                   then None
                                                                   else 
                                                                    if b12
                                                                    then 
                                                                    if b13
                                                                    then 
                                                                    if b14
                                                                    then None
                                                                    else 
                                                                    if b15
                                                                    then None
                                                                    else 
                                                                    Some
                                                                    { number =
                                                                    (a::(b::(c::[])));
                                                                    text = f }
                                                                    else None
                                                                    else None
                                                                 else None)
                                                       a
                                      else None)
             a0)))
