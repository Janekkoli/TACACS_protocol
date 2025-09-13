(* Simple TCP client *)
open Unix
open Tacacs_extracted

(* let rec nat_to_int = function
  | O -> 0
  | S n -> 1 + nat_to_int n *)

let rec int_to_nat n = 
  if n <= 0 then O 
  else S (int_to_nat (n - 1))

let string_to_char_list s =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i-1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []


let char_list_to_string chars =
  let bytes = Bytes.create (List.length chars) in
  let rec aux i = function
    | [] -> Bytes.to_string bytes
    | c :: rest -> 
        Bytes.set bytes i c;
        aux (i + 1) rest
  in
aux 0 chars

let () =
  let argc = Array.length Sys.argv in
  if argc != 3 then
    Printf.printf "Usage: %s <server_ip> <port>\n" Sys.argv.(0)
  else
    let server_ip = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in

  
  (* let result = Tacacs_extracted.fact (S (S (S (S (S O))))) in
  Printf.printf "5! = %d\n" (nat_to_int result); *)
  
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock (ADDR_INET (inet_addr_of_string server_ip, port));
  Printf.printf "Connected to %s:%d\n%!" server_ip port;


  let sample_request = {
    version = int_to_nat 1;           (* TACACS+ version 1 *)
    req_type = string_to_char_list "authenticate";        (* request type *)
    username = string_to_char_list "alice";               (* username *)
    password = string_to_char_list "secret123";           (* password *)
    line = int_to_nat 1;             (* line number *)
  } in
  

  let package_list = make_package sample_request in
  let string_list = List.map char_list_to_string package_list in
  
  Printf.printf "Package contents:\n";
  List.iteri (fun i s -> Printf.printf "  Field %d: %s\n" i s) string_list;

  let msg = "xdddddd" in

  ignore (write sock (Bytes.of_string msg) 0 (String.length msg));
  let buf = Bytes.create 1024 in
  let n = read sock buf 0 1024 in
  if n > 0 then Printf.printf "Received echo: %s\n%!" (Bytes.sub_string buf 0 n);
  close sock