open Functions
open Tacacs_extracted



let test_request (name : string) (request : request) =
  Printf.printf "\n=== Testing %s ===\n" name;
  
  let package = encode_request request in
  let package_string = char_list_to_string package in
  Printf.printf "Encoded package: %s\n%!" package_string;

  let parsed = parse_request (string_to_char_list package_string) in
  match parsed with
  | None -> Printf.printf "❌ Parse failed: None\n%!"
  | Some pac -> 
      let re_encoded = char_list_to_string (encode_request pac) in
      Printf.printf "✅ Parse success: %s\n%!" re_encoded;
      if package_string = re_encoded then
        Printf.printf "✅ Round-trip successful!\n%!"
      else
        Printf.printf "❌ Round-trip failed - strings don't match\n%!"

let () = 
  Printf.printf "TACACS Protocol Testing Suite\n";
  Printf.printf "============================\n";

  (* Test 1: Auth *)
  let auth_data = {
    Auth.username = string_to_char_list "alice";
    Auth.password = string_to_char_list "secret123";
    Auth.line = 12368;
    Auth.style = string_to_char_list "ascii";
  } in
  test_request "Auth" (Auth auth_data);

  (* Test 2: Login *)
  let login_data = {
    Login.username = string_to_char_list "bob";
    Login.password = string_to_char_list "password456";
    Login.line = 1001;
  } in
  test_request "Login" (Login login_data);

  (* Test 3: Connect *)
  let connect_data = {
    Connect.username = string_to_char_list "charlie";
    Connect.password = string_to_char_list "connect789";
    Connect.line = 2002;
    Connect.destination_ip = string_to_char_list "192.168.1.100";
    Connect.destination_port = 80;
  } in
  test_request "Connect" (Connect connect_data);

  (* Test 4: Superuser *)
  let superuser_data = {
    Superuser.username = string_to_char_list "admin";
    Superuser.password = string_to_char_list "adminpass";
    Superuser.line = 3003;
  } in
  test_request "Superuser" (Superuser superuser_data);

  (* Test 5: Logout *)
  let logout_data = {
    Logout.username = string_to_char_list "dave";
    Logout.password = string_to_char_list "logout123";
    Logout.line = 4004;
    Logout.reason = string_to_char_list "session_timeout";
  } in
  test_request "Logout" (Logout logout_data);

  (* Test 6: Slipon *)
  let slipon_data = {
    Slipon.username = string_to_char_list "eve";
    Slipon.password = string_to_char_list "slip456";
    Slipon.line = 5005;
    Slipon.slip_address = string_to_char_list "10.0.0.50";
  } in
  test_request "Slipon" (Slipon slipon_data);

  (* Test 7: Slipoff *)
  let slipoff_data = {
    Slipoff.username = string_to_char_list "frank";
    Slipoff.password = string_to_char_list "slipoff789";
    Slipoff.line = 6006;
    Slipoff.reason = string_to_char_list "user_disconnect";
  } in
  test_request "Slipoff" (Slipoff slipoff_data);

  Printf.printf "\n=== Test Summary ===\n";
  Printf.printf "All 7 request types tested\n";
  Printf.printf "Check above for any failures\n%!"