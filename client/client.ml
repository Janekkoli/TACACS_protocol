open Unix
open Tacacs_extracted
open Functions

let send_request (req : request) (sock : file_descr) (server_ip : string) (port : int) : response option =
  connect sock (ADDR_INET (inet_addr_of_string server_ip, port));
  let package_string = char_list_to_string (encode_request req) in
  ignore (write sock (Bytes.of_string package_string) 0 (String.length package_string)); (*TODO here should not be ignore but i dont know yet what should be*)
  let buf = Bytes.create 1024 in
  match read sock buf 0 1024, close sock with
  | 0, _ -> None
  | n ,_ -> parse_response (string_to_char_list (Bytes.sub_string buf 0 n))


let input_with_message(s) = 
  print_string s;
  flush Stdlib.stdout;
  try
    read_line ()
  with End_of_file ->
    print_endline "\nEnd of input – program ends.";
    exit 1

let prepare_auth_request username password line = 
  Auth {
    Auth.username = string_to_char_list username;
    Auth.password = string_to_char_list password;
    Auth.line =  line;
    Auth.style = string_to_char_list "ascii";
    }
     
let prepare_login_request username password line = 
  Login {
    Login.username = string_to_char_list username;
    Login.password = string_to_char_list password;
    Login.line =  line;
    }
    
let prepare_connect_request username line = 
  let dip = input_with_message "Type destinationIP: " in
  let dp = input_with_message "Type destinationPort: " in
  Connect {
    Connect.username = string_to_char_list username;
    Connect.password = string_to_char_list "-"; (* According to documentation this should be empty string, but COQ is too complicated *)
    Connect.line =  line;
    Connect.destination_ip = string_to_char_list dip;
    Connect.destination_port = int_of_string dp;
    }
    
let prepare_superuser_request username line = 
  Superuser {
    Superuser.username = string_to_char_list username;
    Superuser.password = string_to_char_list "-"; (* According to documentation this should be empty string, but COQ is too complicated *)
    Superuser.line =  line;
    }
    
let prepare_logout_request username line = 
  Logout {
    Logout.username = string_to_char_list username;
    Logout.password = string_to_char_list "-"; (* According to documentation this should be empty string, but COQ is too complicated *)
    Logout.line =  line;
    Logout.reason = string_to_char_list "user-logged-out";
    }
    
let prepare_slipon_request username line = 
  let slip_address = input_with_message "Type slipaddress: " in
  Slipon {
    Slipon.username = string_to_char_list username;
    Slipon.password = string_to_char_list "-"; (* According to documentation this should be empty string, but COQ is too complicated *)
    Slipon.line =  line;
    Slipon.slip_address = string_to_char_list slip_address;
    }
    
let prepare_slipoff_request username line = 
  Slipoff {
    Slipoff.username = string_to_char_list username;
    Slipoff.password = string_to_char_list "-"; (* According to documentation this should be empty string, but COQ is too complicated *)
    Slipoff.line =  line;
    Slipoff.reason = string_to_char_list "user-terminated";
    }

let rec yes_or_no question =
  let answer = input_with_message question in
  match answer with
    | "Y" | "y" | "" -> true
    | "N" | "n" -> false
    | _ -> yes_or_no question

let auth_once username password line server_ip port =
  let request = prepare_auth_request username password line in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let resp = send_request request sock server_ip port in
  match resp with
    | None ->
      ignore (Printf.printf "Failed to receive response.\n");
      true
    | Some response ->
      ignore (Printf.printf "    (Recived: %s %s)\n" (char_list_to_string response.number) (char_list_to_string response.text));
      if List.nth response.number 0 = '2' then
        begin
        ignore (Printf.printf "Auth accepted\n\n%!");
        true
        end
      else
        true

let rec login_util_succ username password line server_ip port = 
  let request = prepare_login_request username password line in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let resp = send_request request sock server_ip port in
  match resp with
    | None ->
      ignore (Printf.printf "Failed to receive response.\n");
      if yes_or_no "Do you want to try again?(Y/n): " then
        login_util_succ username password line server_ip port
      else false
    | Some response ->
      ignore (Printf.printf "    (Recived: %s %s)\n" (char_list_to_string response.number) (char_list_to_string response.text));
      if List.nth response.number 0 = '2' then
        begin
        ignore (Printf.printf "Login accepted\n\n%!");
        true
        end
      else
        if yes_or_no "Do you want to try again?(Y/n): " then
          login_util_succ username password line server_ip port
        else false

let rec connect_until_no username line server_ip port = 
  if yes_or_no "Do you want to send connect packet?(Y/n): " = false then
    true
  else
    let request = prepare_connect_request username line in
    let sock = socket PF_INET SOCK_STREAM 0 in
    let resp = send_request request sock server_ip port in
    match resp with
      | None ->
        ignore (Printf.printf "Failed to receive response.\n");
        connect_until_no username line server_ip port
      | Some response ->
        ignore (Printf.printf "    (Recived: %s %s)\n" (char_list_to_string response.number) (char_list_to_string response.text));
        if List.nth response.number 0 = '2' then
          begin
          ignore (Printf.printf "Connect accepted\n\n%!");
          connect_until_no username line server_ip port
          end
        else
          connect_until_no username line server_ip port
      
let logout_once username line server_ip port =
  let request = prepare_logout_request username line in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let resp = send_request request sock server_ip port in
  match resp with
    | None ->
      ignore (Printf.printf "Failed to receive response.\n");
      true
    | Some response ->
      ignore (Printf.printf "    (Recived: %s %s)\n" (char_list_to_string response.number) (char_list_to_string response.text));
      if List.nth response.number 0 = '2' then
        begin
        ignore (Printf.printf "Logout accepted\n\n%!");
        true
        end
      else
        true

let rec slipon_until_succ username line server_ip port =
  let request = prepare_slipon_request username line in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let resp = send_request request sock server_ip port in
  match resp with
    | None ->
      ignore (Printf.printf "Failed to receive response.\n");
      if yes_or_no "Do you want to try again?(Y/n): " then
        slipon_until_succ username line server_ip port
      else false
    | Some response ->
      ignore (Printf.printf "    (Recived: %s %s)\n" (char_list_to_string response.number) (char_list_to_string response.text));
      if List.nth response.number 0 = '2' then
        begin
        ignore (Printf.printf "Slipon accepted\n\n%!");
        true
        end
      else
        if yes_or_no "Do you want to try again?(Y/n): " then
          slipon_until_succ username line server_ip port
        else false

let slipoff_once username line server_ip port = 
let request = prepare_slipoff_request username line in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let resp = send_request request sock server_ip port in
  match resp with
    | None ->
      ignore (Printf.printf "Failed to receive response.\n");
      true
    | Some response ->
      ignore (Printf.printf "    (Recived: %s %s)\n" (char_list_to_string response.number) (char_list_to_string response.text));
      if List.nth response.number 0 = '2' then
        begin
        ignore (Printf.printf "Slipoff accepted\n\n%!");
        true
        end
      else
        true

let rec superuser_until_succ username line server_ip port =
  let request = prepare_superuser_request username line in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let resp = send_request request sock server_ip port in
  match resp with
    | None ->
      ignore (Printf.printf "Failed to receive response.\n");
      if yes_or_no "Do you want to try again?(Y/n): " then
        superuser_until_succ username line server_ip port
      else false
    | Some response ->
      ignore (Printf.printf "    (Recived: %s %s)\n" (char_list_to_string response.number) (char_list_to_string response.text));
      if List.nth response.number 0 = '2' then
        begin
        ignore (Printf.printf "Superuser accepted\n\n%!");
        true
        end
      else
        if yes_or_no "Do you want to try again?(Y/n): " then
          superuser_until_succ username line server_ip port
        else false

let () =
  let argc = Array.length Sys.argv in
  if argc != 3 then
    Printf.printf "Usage: %s <server_ip> <port>\n" Sys.argv.(0)
  else
    let server_ip = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
  
  let connection_type = input_with_message "What type of connection would you like to establish?\n1 - Authenticate only\n2 - Login connection\n3 - SLIP connection\n4 - Login connection with superuser\n: " in
  if connection_type <> "1" && connection_type <> "2" && connection_type <> "3" && connection_type <> "4" then
    begin
      Printf.printf "\nUnknown connection type \"%s\"\n%!" connection_type;
      exit 1
    end
  else
  let username = input_with_message "Type username: " in
  let password = input_with_message "Type password: " in
  let line = int_of_string (input_with_message "Type line: ") in

  match connection_type with

    | "1" ->
      begin
      ignore (auth_once username password line server_ip port)
      end

    | "2" ->
      if login_util_succ username password line server_ip port then
        begin
        ignore (connect_until_no username line server_ip port);
        ignore (logout_once username line server_ip port);
        exit 0
        end
      else
        exit 1
      
    | "3" ->
      if login_util_succ username password line server_ip port then
        begin
        ignore (connect_until_no username line server_ip port);
        ignore (slipon_until_succ username line server_ip port);
        ignore (logout_once username line server_ip port);
        ignore (slipoff_once username line server_ip port);
        exit 0
        end
      else
        exit 1

    | "4" ->
      if login_util_succ username password line server_ip port then
        begin
        ignore (connect_until_no username line server_ip port);
        ignore (superuser_until_succ username line server_ip port);
        ignore (connect_until_no username line server_ip port);
        ignore (logout_once username line server_ip port);
        exit 0
        end
      else
        exit 1
        
    | _ ->
      ignore (Printf.printf "\nUnknown connection type \"%s\"\n%!" connection_type);
      exit 1
    