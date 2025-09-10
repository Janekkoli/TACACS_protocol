(* Simple TCP echo server *)
open Unix

let () =
  let port = 9000 in
  let sockaddr = ADDR_INET (inet_addr_any, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock sockaddr;
  listen sock 5;
  Printf.printf "Server listening on port %d\n%!" port;
  while true do
    let (client_sock, client_addr) = accept sock in
    Printf.printf "Client connected: %s\n%!" (string_of_inet_addr (match client_addr with
      | ADDR_INET (addr, _) -> addr
      | _ -> inet_addr_any));
    let buf = Bytes.create 1024 in
    let n = read client_sock buf 0 1024 in
    if n > 0 then begin
      Printf.printf "Received: %s\n%!" (Bytes.sub_string buf 0 n);
      ignore (write client_sock buf 0 n);
    end;
    close client_sock
  done