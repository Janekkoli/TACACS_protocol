open Functions
open Tacacs_extracted

let add_to_db db =
  UserDB.add_user db "alice" "secret" false "admin" false;
  UserDB.add_user db "bob" "qwerty" false "guest" false;
  UserDB.add_user db "charlie" "hunter2" false "developer" false;
  UserDB.add_user db "david" "letmein" false "tester" false;
  UserDB.add_user db "eve" "12345" false "attacker" false;
  UserDB.add_user db "frank" "password" false "support" false;
  UserDB.add_user db "grace" "iloveyou" false "manager" false;
  UserDB.add_user db "heidi" "trustno1" false "intern" false;
  UserDB.add_user db "ivan" "qazwsx" false "devops" false;
  UserDB.add_user db "judy" "welcome" false "hr" false;
  UserDB.add_user db "mallory" "abc123" false "analyst" false;
  UserDB.add_user db "oscar" "dragon" false "researcher" false;
  UserDB.add_user db "peggy" "sunshine" false "student" false;
  UserDB.add_user db "trent" "shadow" false "architect" false;
  UserDB.add_user db "victor" "master" false "consultant" false;
  UserDB.add_user db "walter" "monkey" false "support" false;
  UserDB.add_user db "yvonne" "flower" false "designer" false;
  UserDB.add_user db "zara" "star123" false "marketing" false

(* 25% chance that the password is expiring*)
let resp_random_positive () =  List.nth [resp_accepted; resp_accepted_expiring] (Random.int 5 / 4) 


let generete_response (req : request option) db: response = 
  (* ignore (Printf.printf "Received: %s\n%!" (Bytes.sub_string buf 0 n)); *)
  if Random.int 8 = 0 then (* 12.5% chance there is no response *)
    resp_no_response
  else
      match req with
      | None ->
        ignore (Printf.printf "Received unknown request type\n%!");
        resp_invalid_format
      | Some pac -> 
          match pac with
          | Auth a ->
            ignore (Printf.printf "Recived Auth request %s\n%!" (char_list_to_string a.username));
            if UserDB.check_password db (char_list_to_string a.username) (char_list_to_string a.password) then (* Checking password *)
              resp_random_positive ()
            else
              resp_wrong_password
          | Login l ->
            ignore (Printf.printf "Recived Login request %s\n%!" (char_list_to_string l.username));
            if UserDB.check_password db (char_list_to_string l.username) (char_list_to_string l.password) then (* Checking password *)
              begin
              ignore (UserDB.log_in db (char_list_to_string l.username));
              ignore (Printf.printf "Logged in %s\n%!" (char_list_to_string l.username));
              resp_random_positive ()
              end
            else
              resp_wrong_password
          | Connect c ->
            ignore (Printf.printf "Recived Connect request %s\n%!" (char_list_to_string c.username));
            if UserDB.is_active db (char_list_to_string c.username) then (* Checking if user is logged in *)
              if Random.int 4 < 3 then (* 75% chance for accept*)
                resp_accepted
              else
                resp_access_denied
            else
              resp_no_connection
          | Superuser s ->
            ignore (Printf.printf "Recived Superuser request %s\n%!" (char_list_to_string s.username));
            if UserDB.is_active db (char_list_to_string s.username) then (* Checking if user is logged in *)
              if UserDB.is_admin db (char_list_to_string s.username) then
                begin
                ignore (Printf.printf "%s activated superuser mode\n%!" (char_list_to_string s.username));
                resp_accepted
                end
              else
                resp_not_admin
            else
              resp_no_connection
          | Logout l ->
            ignore (Printf.printf "Recived Logout request %s\n%!" (char_list_to_string l.username));
            if UserDB.is_active db (char_list_to_string l.username) then (* Checking if user is logged in *)
              begin
              ignore (UserDB.log_out db (char_list_to_string l.username));
              ignore (Printf.printf "Logged out %s, reason: %s\n%!" (char_list_to_string l.username) (char_list_to_string l.reason));
              resp_accepted
              end
            else
              resp_no_connection
          | Slipon s ->
            ignore (Printf.printf "Recived Slipon request %s\n%!" (char_list_to_string s.username));
            if UserDB.is_active db (char_list_to_string s.username) then (* Checking if user is logged in *)
              if UserDB.is_slip_connection_active db (char_list_to_string s.username) = false then (* Checking if user is already in slip mode *)
                begin
                ignore (UserDB.slipcon_on db (char_list_to_string s.username));
                ignore (Printf.printf "Slip conection on %s slipaddress %s\n%!" (char_list_to_string s.username) (char_list_to_string s.slip_address));
                resp_accepted
                end
              else
                resp_slip_mode
            else
              resp_no_connection
          | Slipoff s ->
            ignore (Printf.printf "Recived Slipoff request %s\n%!" (char_list_to_string s.username));
            if UserDB.is_slip_connection_active db (char_list_to_string s.username) then (* Checking if user is logged in *)
              begin
              ignore (UserDB.slipcon_off db (char_list_to_string s.username));
              ignore (Printf.printf "Slip conection off %s reason %s\n%!" (char_list_to_string s.username) (char_list_to_string s.reason));
              resp_accepted
              end
            else
              resp_no_slip_connection







let ( let* ) = Lwt.bind

(* Handles communication with a connected client by echoing received lines back. *)
let handle_client (input, output) db =
    let* req = Lwt_io.read input ~count:1024 in
    (* let* () = Lwt_io.printlf "Recived request: %s" req in *)
(* `Lwt_io.read_line_opt`: Asynchronously reads a line from the input. Returns None if the client disconnects. *)
    let reps = generete_response (parse_request (string_to_char_list req)) db in
    let response =char_list_to_string (encode_response reps) in
    let* () = Lwt_io.write_line output response in
    let* () = Lwt_io.printlf "Sent response: %s" response in
    Lwt.return_unit
(* Accepts incoming client connections and starts handling them. *)
let rec accept_connections server_socket db=
  let* (client_socket, _addr) =
(* `Lwt_unix.accept`: Asynchronously accepts a new connection on the server socket. *)
  Lwt_unix.accept server_socket in
  let input =
    Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let output =
    Lwt_io.of_fd ~mode:Lwt_io.output client_socket in

(* `Lwt.async`: Schedules the handling of the client in a separate thread. *)
  Lwt.async (fun () -> (handle_client (input, output) db));
  accept_connections server_socket db

(* Initializes the server socket and starts accepting connections on the specified port. *)
let start_server port db=
  let sockaddr =
    Unix.(ADDR_INET (inet_addr_any, port)) in
  let server_socket =
    Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  Lwt_unix.setsockopt
    server_socket Unix.SO_REUSEADDR true;
(* `Lwt_unix.bind`: Asynchronously binds the server socket to the specified address.
   `Lwt_unix.listen`: Prepares the socket to accept incoming connections. *)
  let* () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 10;
  let* () =
    Lwt_io.printlf "Server started on port %d" port
  in
  accept_connections server_socket db

(* `Lwt_main.run`: Runs the Lwt main loop, starting the server. *)
let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    Printf.printf "Usage: %s <port>\n" Sys.argv.(0)
  else
    let port = int_of_string Sys.argv.(1) in

  Random.self_init (); 
  (* create db *)
  let db = UserDB.create () in (* inicjalizacja bazy użytkowników *)
  add_to_db db;
  UserDB.print_all db;
  
  Lwt_main.run (start_server port db)
