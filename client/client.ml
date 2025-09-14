open Unix
open Tacacs_extracted

let string_to_char_list s =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i-1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []

let () =
  let argc = Array.length Sys.argv in
  if argc != 3 then
    Printf.printf "Usage: %s <server_ip> <port>\n" Sys.argv.(0)
  else
    let server_ip = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
  
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock (ADDR_INET (inet_addr_of_string server_ip, port));
  Printf.printf "Connected to %s:%d\n%!" server_ip port;

  let auth_data = {
    Auth.username = string_to_char_list "alice";
    Auth.password = string_to_char_list "secret123";
    Auth.line =  12368;
    Auth.style = string_to_char_list "ascii";
  } in

  let package = encode_request(Auth auth_data) in
  let package_string = (String.concat "" (List.map (String.make 1) package)) in
  Printf.printf "Encoded package: %s\n%!" package_string;

  ignore (write sock (Bytes.of_string package_string) 0 (String.length package_string));
  let buf = Bytes.create 1024 in
  let n = read sock buf 0 1024 in
  if n > 0 then Printf.printf "Received echo: %s\n%!" (Bytes.sub_string buf 0 n);
  close sock