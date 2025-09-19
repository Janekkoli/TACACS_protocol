Require Export Protocol.
Require Extraction.

(* So string is string *)
Require Import ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlString.

(* So no int is not nat *)
Extract Inductive nat => "int" [ "0" "succ" ].



Set Extraction Output Directory "../../theories".
Extraction Language OCaml.
Extraction "tacacs_extracted.ml" request encode_request tillFirstcRlF splitincRlF splitonCRLFandSpaces response encode_response parse_response resp_accepted resp_accepted_expiring resp_access_denied resp_invalid_format resp_no_connection resp_no_response resp_no_slip_connection resp_not_admin resp_slip_mode resp_wrong_password.

