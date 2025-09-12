(* Simple TCP client *)
open Unix
open Tacacs_extracted

let rec nat_to_int = function
  | O -> 0
  | S n -> 1 + nat_to_int n

let () =
  let result = Tacacs_extracted.fact (S (S (S (S (S O))))) in
  Printf.printf "5! = %d\n" (nat_to_int result);
  let server_ip = "127.0.0.1" in
  let port = 9000 in
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock (ADDR_INET (inet_addr_of_string server_ip, port));
  Printf.printf "Connected to %s:%d\n%!" server_ip port;
  let msg = "Hello from client!" in
  ignore (write sock (Bytes.of_string msg) 0 (String.length msg));
  let buf = Bytes.create 1024 in
  let n = read sock buf 0 1024 in
  if n > 0 then Printf.printf "Received echo: %s\n%!" (Bytes.sub_string buf 0 n);
  close sock