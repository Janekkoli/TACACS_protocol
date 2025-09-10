open Unix
open Uint63
open String
open ProtocolDefinitions

(** Get the initial communication state on upload. *)
let init_upload_connection conn (lfn:string)
      (rfn:string) (mname:string) (server:sockaddr) =
  let payload = (lfn ^ "-" ^ rfn ^ "-" ^ mname) in
  (* This time we use UDP communication
    let _ = print_string "Connecting\n"; connect conn server;
          print_string "Connected\n" in
   *)
  let three = Uint63.of_int 3 in
  (FComm ((conn, three), false), OPacket payload)
;;

(** Get the initial communication state on download. *)
let init_download_connection conn lfn rfn mname server =
  (SError "File download is not implemented", Nothing)

(** Handle communication timeout. *)
let communication_timeout pd =
  (SError "Timeout handling is not implemented", Nothing)


