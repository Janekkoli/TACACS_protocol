(* Simple TCP client *)
open Unix

let () =
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