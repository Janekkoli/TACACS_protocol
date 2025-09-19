(* Simple TCP echo server *)
open Unix
open Functions
open Tacacs_extracted

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    Printf.printf "Usage: %s <port>\n" Sys.argv.(0)
  else
    let port = int_of_string Sys.argv.(1) in
  let sockaddr = ADDR_INET (inet_addr_any, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock sockaddr;
  listen sock 5;
  Printf.printf "Server listening on port %d\n%!" port;
  Random.self_init ();  (* inicjalizacja generatora losowego *)


  let db = UserDB.create () in (* inicjalizacja bazy użytkowników *)

  (* Dodanie użytkowników *)
  UserDB.add_user db "alice" "secret" false "admin";
  UserDB.add_user db "bob" "qwerty" false "guest";
  UserDB.add_user db "charlie" "hunter2" false "developer";
  UserDB.add_user db "david" "letmein" false "tester";
  UserDB.add_user db "eve" "12345" false "attacker";
  UserDB.add_user db "frank" "password" false "support";
  UserDB.add_user db "grace" "iloveyou" false "manager";
  UserDB.add_user db "heidi" "trustno1" false "intern";
  UserDB.add_user db "ivan" "qazwsx" false "devops";
  UserDB.add_user db "judy" "welcome" false "hr";
  UserDB.add_user db "mallory" "abc123" false "analyst";
  UserDB.add_user db "oscar" "dragon" false "researcher";
  UserDB.add_user db "peggy" "sunshine" false "student";
  UserDB.add_user db "trent" "shadow" false "architect";
  UserDB.add_user db "victor" "master" false "consultant";
  UserDB.add_user db "walter" "monkey" false "support";
  UserDB.add_user db "yvonne" "flower" false "designer";
  UserDB.add_user db "zara" "star123" false "marketing";


  (* Sprawdzenie hasła *)
  Printf.printf "Logowanie alice/secret: %b\n"
    (UserDB.check_password db "alice" "secret");

  Printf.printf "Logowanie alice/wrong: %b\n"
    (UserDB.check_password db "alice" "wrong");

  Printf.printf "Logowanie ghost/test: %b\n"
    (UserDB.check_password db "ghost" "test");

  (* Wypisanie wszystkich *)
  Printf.printf "--- Użytkownicy w bazie ---\n";
  UserDB.print_all db;


  while true do
    let (client_sock, client_addr) = accept sock in
    Printf.printf "Client connected: %s\n%!" (string_of_inet_addr (match client_addr with
      | ADDR_INET (addr, _) -> addr
      | _ -> inet_addr_any));
    let buf = Bytes.create 1024 in
    let n = read client_sock buf 0 1024 in
    if n > 0 then begin
      let response = 
        ignore (Printf.printf "Received: %s\n%!" (Bytes.sub_string buf 0 n));
        if Random.int 8 = 0 then (* 12.5% chance there is no response *)
          ("401", "no response; retry")
        else
          let parsed = parse_request (string_to_char_list (Bytes.sub_string buf 0 n)) in
            match parsed with
            | None ->
              ignore (Printf.printf "Received unknown request type\n%!");
              ("501", "invalid format")
            | Some pac -> 
                match pac with
                | Auth a ->
                  ignore (Printf.printf "Recived Auth request %s\n%!" (char_list_to_string a.username));
                  if UserDB.check_password db (char_list_to_string a.username) (char_list_to_string a.password) then (* Checking password *)
                    List.nth [("201", "accepted: 0 0 0"); ("202", "accepted, password is expiring: 0 0 0")] (Random.int 5 / 4) (* 25% chance that the password is expiring*)
                  else
                    ("503", "access denied, wrong password")
                | Login l ->
                  ignore (Printf.printf "Recived Login request %s\n%!" (char_list_to_string l.username));
                  if UserDB.check_password db (char_list_to_string l.username) (char_list_to_string l.password) then (* Checking password *)
                    begin
                    ignore (UserDB.log_in db (char_list_to_string l.username));
                    ignore (Printf.printf "Logged in %s\n%!" (char_list_to_string l.username));
                    List.nth [("201", "accepted: 0 0 0"); ("202", "accepted, password is expiring: 0 0 0")] (Random.int 5 / 4) (* 25% chance that the password is expiring*)
                    end
                  else
                    ("503", "access denied, wrong password")
                | Connect c ->
                  ignore (Printf.printf "Recived Connect request %s\n%!" (char_list_to_string c.username));
                  if UserDB.is_active db (char_list_to_string c.username) then (* Checking if user is logged in *)
                    if Random.int 4 < 3 then (* 75% chance for accept*)
                      ("201", "accepted: 0 0 0")
                    else
                      ("502", "access denied")
                  else
                    ("504", "access denied, no existing connection")
                | Superuser s ->
                  ignore (Printf.printf "Recived Superuser request %s\n%!" (char_list_to_string s.username));
                  if UserDB.is_active db (char_list_to_string s.username) then (* Checking if user is logged in *)
                    if UserDB.is_admin db (char_list_to_string s.username) then
                      begin
                      ignore (Printf.printf "%s activated superuser mode\n%!" (char_list_to_string s.username));
                      ("201", "accepted: 0 0 0")
                      end
                    else
                      ("505", "access denied, you are not admin!")
                  else
                    ("504", "access denied, no existing connection")
                | _ ->
                  ignore (Printf.printf "Received unknown request type, but not None: %s\n%!" (char_list_to_string (encode_request pac)));
                  ("501", "invalid format") in
      
      let (res_code, res_text) = response in
      let res = { number = string_to_char_list res_code; text = string_to_char_list res_text } in
      let charlistrequest = encode_response res in
      let n = List.length charlistrequest in
      ignore (Printf.printf "Server response: %s\n%!" (char_list_to_string charlistrequest));
      ignore (write client_sock (Bytes.of_string (char_list_to_string charlistrequest) ) 0 n);
    end;
    close client_sock
  done