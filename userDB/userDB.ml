(* Definicja rekordu użytkownika *)
type user = {
  login : string;
  password : string;
  active : bool;
  info : string;
  slipcon : bool;
}

(* Typ bazy danych użytkowników *)
type t = (string, user) Hashtbl.t

(* Utworzenie pustej bazy *)
let create () : t =
  Hashtbl.create 18

(* Dodanie nowego użytkownika *)
let add_user db login password active info slipcon =
  let u = { login; password; active; info; slipcon} in
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

  (* sprawdzenie, czy jest adminem*)
let is_admin db login =
  match Hashtbl.find_opt db login with
  | Some u -> u.info = "admin"
  | None -> false

(* sprawdzenie, czy jest zalogowany*)
let is_slip_connection_active db login =
  match Hashtbl.find_opt db login with
  | Some u -> u.slipcon
  | None -> false

(* Slip Connection ON *)
let slipcon_on db login =
  match Hashtbl.find_opt db login with
  | Some u -> Hashtbl.replace db login { u with slipcon = true }
  | None -> ()

(* Slip Connection OFF*)
let slipcon_off db login =
  match Hashtbl.find_opt db login with
  | Some u -> Hashtbl.replace db login { u with slipcon = false }
  | None -> ()

(* Wypisanie wszystkich użytkowników *)
let print_all db =
  Printf.printf "--- Użytkownicy w bazie ---\n";

  Hashtbl.iter
    (fun _ u ->
       Printf.printf "login=%s, active=%b, info=%s\n"
         u.login u.active u.info) db;
  Printf.printf "----------------------------\n"
