open Tacacs_extracted

let string_to_char_list s =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i-1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []

let char_list_to_string cl = 
  (String.concat "" (List.map (String.make 1) cl))

let () = 
  let auth_data = {
    Auth.username = string_to_char_list "alice";
    Auth.password = string_to_char_list "secret123";
    Auth.line =  12368;
    Auth.style = string_to_char_list "ascii";
  } in

  let package = encode_request(Auth auth_data) in
  let package_string = char_list_to_string package in
  Printf.printf "Encoded package: %s\n%!" package_string;

  let parsed = parse_request (string_to_char_list package_string) in
  match parsed with
  | None -> Printf.printf "None :("
  | Some value -> Printf.printf "Hm: %s\n%!" (char_list_to_string value)