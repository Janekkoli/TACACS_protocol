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
  UserDB.add_user db "alice" "secret" "offline" "admin";
  UserDB.add_user db "bob" "qwerty" "offline" "guest";
  UserDB.add_user db "charlie" "hunter2" "offline" "developer";
  UserDB.add_user db "david" "letmein" "offline" "tester";
  UserDB.add_user db "eve" "12345" "offline" "attacker";
  UserDB.add_user db "frank" "password" "offline" "support";
  UserDB.add_user db "grace" "iloveyou" "offline" "manager";
  UserDB.add_user db "heidi" "trustno1" "offline" "intern";
  UserDB.add_user db "ivan" "qazwsx" "offline" "devops";
  UserDB.add_user db "judy" "welcome" "offline" "hr";
  UserDB.add_user db "mallory" "abc123" "offline" "analyst";
  UserDB.add_user db "oscar" "dragon" "offline" "researcher";
  UserDB.add_user db "peggy" "sunshine" "offline" "student";
  UserDB.add_user db "trent" "shadow" "offline" "architect";
  UserDB.add_user db "victor" "master" "offline" "consultant";
  UserDB.add_user db "walter" "monkey" "offline" "support";
  UserDB.add_user db "yvonne" "flower" "offline" "designer";
  UserDB.add_user db "zara" "star123" "offline" "marketing";


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
        let parsed = parse_request (string_to_char_list (Bytes.sub_string buf 0 n)) in
          match parsed with
          | None ->
            ignore (Printf.printf "Received unknown request type\n%!");
            ("501", "invalid format")
          | Some pac -> 
              match pac with
              | Auth a ->
                ignore (Printf.printf "Recived Auth request %s\n%!" (char_list_to_string a.username));
                ("201", "accepted: # # #")
              | _ ->
                ignore (Printf.printf "Received unknown request type, but not None: %s\n%!" (char_list_to_string (encode_request pac)));
                ("501", "invalid format") in
      
      let (x, y) = response in
      ignore (Printf.printf "Chosen response: %s %s\n%!" x y);

      let responses = [
        ("201", "accepted: # # #");
        ("202", "accepted, password is expiring: # # #");
        ("401", "no response; retry");
        ("501", "invalid format");
        ("502", "access denied")
      ] in
      let (res_code, res_text) = List.nth responses (Random.int (List.length responses)) in
      let res = { number = string_to_char_list res_code; text = string_to_char_list res_text } in
      let charlistrequest = encode_response res in
      let n = List.length charlistrequest in
      ignore (Printf.printf "Server response: %s\n%!" (char_list_to_string charlistrequest));
      ignore (write client_sock (Bytes.of_string (char_list_to_string charlistrequest) ) 0 n);
    end;
    close client_sock
  done