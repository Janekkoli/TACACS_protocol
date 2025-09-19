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
    print_endline "\nKoniec wejścia – program kończy działanie.";
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

let () =
  let argc = Array.length Sys.argv in
  if argc != 3 then
    Printf.printf "Usage: %s <server_ip> <port>\n" Sys.argv.(0)
  else
    let server_ip = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
  
  let connection_type = input_with_message "What type of connection would you like to establish?\n1 - Authenticate only\n2 - Login connection\n3 - SLIP connection\n: " in
  if connection_type <> "1" && connection_type <> "2" && connection_type <> "3" then
    begin
      Printf.printf "\nUnknown connection type \"%s\"\n%!" connection_type;
      exit 1
    end
  else
  let username = input_with_message "Type username: " in
  let password = input_with_message "Type password: " in
  let line = int_of_string (input_with_message "Type line: ") in

  (* match connection_type with
    | "1" ->
    | "2" ->
    | "3" -> *)
    
  
  let request_type = input_with_message "Request type\na - auth\nli - login\nc - connect\nsu - superuser\nlo - logout\nso - slipon\nsf - slipoff\n: " in
  let request = 
    match request_type with
      | "a" -> 
        prepare_auth_request username password line
      | "li" -> 
        prepare_login_request username password line
      | "c" -> 
        prepare_connect_request username line
      | "su" -> 
        prepare_superuser_request username line
      | "lo" -> 
        prepare_logout_request username line
      | "so" -> 
        prepare_slipon_request username line
      | "sf" -> 
        prepare_slipoff_request username line
      | _ -> 
        Printf.printf "\nUnknown request type %s\n%!" request_type;
        exit 1
  in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let resp = send_request ((*Auth auth_data*) request)  sock server_ip port in
  match resp with
  |None -> Printf.printf ":()"
  | Some resp -> let resp_string = char_list_to_string (encode_response resp) in
  Printf.printf "recived: %s\n" resp_string