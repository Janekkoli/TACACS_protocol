open Functions
open Tacacs_extracted

let () =
  let req = { number = string_to_char_list "211"; text = string_to_char_list "somedataxddd" } in
  let charlistrequest = encode_response req in
  Printf.printf "request: %s\n" (char_list_to_string charlistrequest);
  let req = parse_response charlistrequest in 
  match req with
  |None -> Printf.printf ":("
  |_ -> Printf.printf "uey"
   

