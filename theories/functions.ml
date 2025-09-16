open Tacacs_extracted

let string_to_char_list s =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i-1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []

let char_list_to_string cl = 
  (String.concat "" (List.map (String.make 1) cl))

let charlist_to_int (l : char list) : int option =
  Stdlib.int_of_string_opt (char_list_to_string l)

let parse_request (s : char list) : request option =
  let splitted_s = splitonCRLFandSpaces s in 
  match splitted_s with
  | [[['1'];['A';'U';'T';'H'];style];[user];[pass];[stringLine]] -> begin
      match charlist_to_int stringLine with
      | Some line -> Some (Auth {
          Auth.username = user;
          Auth.password = pass;
          Auth.line = line;
          Auth.style = style
        })
      | _ -> None
    end
  | [[['1'];['L';'O';'G';'I';'N']];[user];[pass];[stringLine]] -> begin
      match charlist_to_int stringLine with
      | Some line -> Some (Login {
          Login.username = user;
          Login.password = pass;
          Login.line = line;
        })
      | _ -> None
    end
    | [[['1'];['C';'O';'N';'N';'E';'C';'T'];dest_ip;string_dest_port];[user];[pass];[stringLine]] -> begin
      match (charlist_to_int(stringLine),charlist_to_int(string_dest_port))  with
      | Some line, Some dest_port -> Some (Connect {
          Connect.username = user;
          Connect.password = pass;
          Connect.line = line;
          Connect.destination_ip = dest_ip;
          Connect.destination_port = dest_port;
        })
      | _ -> None
    end
  | [[['1'];['S';'U';'P';'E';'R';'U';'S';'E';'R']];[user];[pass];[stringLine]] -> begin
      match charlist_to_int stringLine with
      | Some line -> Some (Superuser {
          Superuser.username = user;
          Superuser.password = pass;
          Superuser.line = line;
        })
      | _ -> None
    end
  | [[['1'];['L';'O';'G';'O';'U';'T'];reason];[user];[pass];[stringLine]] -> begin
      match charlist_to_int stringLine with
      | Some line -> Some (Logout {
          Logout.username = user;
          Logout.password = pass;
          Logout.line = line;
          Logout.reason = reason;
        })
      | _ -> None
    end
  | [[['1'];['S';'L';'I';'P';'O';'N'];adres];[user];[pass];[stringLine]] -> begin
      match charlist_to_int stringLine with
      | Some line -> Some (Slipon {
          Slipon.username = user;
          Slipon.password = pass;
          Slipon.line = line;
          Slipon.slip_address = adres;
        })
      | _ -> None
    end
  | [[['1'];['S';'L';'I';'P';'O';'F';'F'];reason];[user];[pass];[stringLine]] -> begin
      match charlist_to_int stringLine with
      | Some line -> Some (Slipoff {
          Slipoff.username = user;
          Slipoff.password = pass;
          Slipoff.line = line;
          Slipoff.reason = reason;
        })
      | _ -> None
    end
  | _ -> None
