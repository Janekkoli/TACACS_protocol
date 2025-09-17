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
  
  let user = input_with_message "Podaj nazwe uzytkownika: " in
  let password = input_with_message "Podaj haslo: " in
  let sock = socket PF_INET SOCK_STREAM 0 in
  
  let auth_data = {
    Auth.username = string_to_char_list user;
    Auth.password = string_to_char_list password;
    Auth.line =  12368;
    Auth.style = string_to_char_list "ascii";
  } in

  let resp = send_request (Auth auth_data)  sock server_ip port in
  match resp with
  |None -> Printf.printf ":()"
  | Some resp -> let resp_string = char_list_to_string (encode_response resp) in
  Printf.printf "recived: %s\n" resp_string