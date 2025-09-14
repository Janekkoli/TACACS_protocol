Require Export Protocol.
(* Require Export Networking. *)
Require Extraction.

(* So no int is not nat *)
Extract Inductive nat => "int" [ "0" "succ" ].

Set Extraction Output Directory "../../theories".
Extraction Language OCaml.
Extraction "tacacs_extracted.ml" request encode_request.

