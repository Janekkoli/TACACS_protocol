(* Simple TCP echo server *)
open Unix
open Functions
open Tacacs_extracted

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    Printf.printf "Usage: %s <port>\n" Sys.argv.(0)
  else
    let port = int_of_string Sys.argv.(1) in
  let sockaddr = ADDR_INET (inet_addr_any, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock sockaddr;
  listen sock 5;
  Printf.printf "Server listening on port %d\n%!" port;
  Random.self_init ();  (* inicjalizacja generatora losowego *)
  while true do
    let (client_sock, client_addr) = accept sock in
    Printf.printf "Client connected: %s\n%!" (string_of_inet_addr (match client_addr with
      | ADDR_INET (addr, _) -> addr
      | _ -> inet_addr_any));
    let buf = Bytes.create 1024 in
    let n = read client_sock buf 0 1024 in
    if n > 0 then begin
      Printf.printf "Received: %s\n%!" (Bytes.sub_string buf 0 n);
      let responses = [
        ("201", "accepted: # # #");
        ("202", "accepted, password is expiring: # # #");
        ("401", "no response; retry");
        ("501", "invalid format");
        ("502", "access denied")
      ] in
      let (res_code, res_text) = List.nth responses (Random.int (List.length responses)) in
      let res = { number = string_to_char_list res_code; text = string_to_char_list res_text } in
      let charlistrequest = encode_response res in
      let n = List.length charlistrequest in
      ignore (Printf.printf "Server response: %s\n%!" (char_list_to_string charlistrequest));
      ignore (write client_sock (Bytes.of_string (char_list_to_string charlistrequest) ) 0 n);
    end;
    close client_sock
  done