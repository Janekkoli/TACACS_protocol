(* Definicja rekordu użytkownika *)
type user = {
  login : string;
  password : string;
  state : string;
  info : string;
}

(* Typ bazy danych użytkowników *)
type t = (string, user) Hashtbl.t

(* Utworzenie pustej bazy *)
let create () : t =
  Hashtbl.create 16

(* Dodanie nowego użytkownika *)
let add_user db login password state info =
  let u = { login; password; state; info } in
  Hashtbl.replace db login u

(* Pobranie użytkownika *)
let get_user db login =
  Hashtbl.find_opt db login

(* Aktualizacja stanu użytkownika *)
let update_state db login new_state =
  match Hashtbl.find_opt db login with
  | Some u -> Hashtbl.replace db login { u with state = new_state }
  | None -> ()

(* Usunięcie użytkownika *)
let remove_user db login =
  Hashtbl.remove db login

(* Sprawdzenie hasła *)
let check_password db login password =
  match Hashtbl.find_opt db login with
  | Some u -> u.password = password
  | None -> false

(* Wypisanie wszystkich użytkowników *)
let print_all db =
  Hashtbl.iter
    (fun _ u ->
       Printf.printf "login=%s, state=%s, info=%s\n"
         u.login u.state u.info)
    db
