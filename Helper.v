Require Import ProtocolDefinitions.
Require Import String.


(** These functions are realised in OCaml *)
Parameter init_upload_connection : ProtocolDefinitions.file_descr ->
  string ->
  string ->
  string ->
  ProtocolDefinitions.sockaddr ->
  ProtocolDefinitions.PCState * ProtocolDefinitions.POutput.

Parameter init_download_connection :
  ProtocolDefinitions.file_descr ->
  string ->
  string ->
  string ->
  ProtocolDefinitions.sockaddr ->
  ProtocolDefinitions.PCState * ProtocolDefinitions.POutput.

Parameter communication_timeout :
  ProtocolDefinitions.CProtoData ->
  ProtocolDefinitions.PCState * ProtocolDefinitions.POutput.
