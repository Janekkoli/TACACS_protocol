Require Export Protocol.
Require Extraction.

(* So string is string *)
Require Import ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlString.

(* So no int is not nat *)
Extract Inductive nat => "int" [ "0" "succ" ].



Set Extraction Output Directory "../../theories".
Extraction Language OCaml.
Extraction "tacacs_extracted.ml" request encode_request tillFirstcRlF splitincRlF splitonCRLFandSpaces response encode_response parse_response.

