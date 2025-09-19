(* Definicja rekordu użytkownika *)
type user = {
  login : string;
  password : string;
  active : bool;
  info : string;
}

(* Typ bazy danych użytkowników *)
type t = (string, user) Hashtbl.t

(* Utworzenie pustej bazy *)
let create () : t =
  Hashtbl.create 18

(* Dodanie nowego użytkownika *)
let add_user db login password active info =
  let u = { login; password; active; info } in
  Hashtbl.replace db login u

(* Pobranie użytkownika *)
let get_user db login =
  Hashtbl.find_opt db login

(* Zalogowanie *)
let log_in db login =
  match Hashtbl.find_opt db login with
  | Some u -> Hashtbl.replace db login { u with active = true }
  | None -> ()

(* Wylogowanie*)
let log_out db login =
  match Hashtbl.find_opt db login with
  | Some u -> Hashtbl.replace db login { u with active = false }
  | None -> ()

(* Usunięcie użytkownika *)
let remove_user db login =
  Hashtbl.remove db login

(* Sprawdzenie hasła *)
let check_password db login password =
  match Hashtbl.find_opt db login with
  | Some u -> u.password = password
  | None -> false

(* sprawdzenie, czy jest zalogowany*)
let is_active db login =
  match Hashtbl.find_opt db login with
  | Some u -> u.active
  | None -> false

(* Wypisanie wszystkich użytkowników *)
let print_all db =
  Hashtbl.iter
    (fun _ u ->
       Printf.printf "login=%s, active=%b, info=%s\n"
         u.login u.active u.info)
    db
