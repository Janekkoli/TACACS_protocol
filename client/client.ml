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

let () =
  let argc = Array.length Sys.argv in
  if argc != 3 then
    Printf.printf "Usage: %s <server_ip> <port>\n" Sys.argv.(0)
  else
    let server_ip = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
  
  let request_type = input_with_message "Request type\na - auth\nli - login\nc - connect\nsu - superuser\nlo - logout\nso - slipon\nsf - slipoff\n: " in
  let request = 
    match request_type with
      | "a" -> 
        let user = input_with_message "Type username: " in
        let password = input_with_message "Type password: " in
        Auth {
          Auth.username = string_to_char_list user;
          Auth.password = string_to_char_list password;
          Auth.line =  12368;
          Auth.style = string_to_char_list "ascii";
          }
        | "li" -> 
        let user = input_with_message "Type username: " in
        let password = input_with_message "Type password: " in
        Login {
          Login.username = string_to_char_list user;
          Login.password = string_to_char_list password;
          Login.line =  12368;
          }
        | "c" -> 
        let user = input_with_message "Type username: " in
        let dip = input_with_message "Type destinationIP: " in
        let dp = input_with_message "Type destinationPort: " in
        Connect {
          Connect.username = string_to_char_list user;
          Connect.password = string_to_char_list "-"; (* According to documentation this should be empty string, but COQ is too complicated *)
          Connect.line =  12368;
          Connect.destination_ip = string_to_char_list dip;
          Connect.destination_port = int_of_string dp;
          }
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