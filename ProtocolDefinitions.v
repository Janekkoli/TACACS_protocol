Require Import String.
Require Import ZArith.
Require Import Int63.Sint63.
Require Import PArray.
Require Import Ascii String Coq.Strings.Byte.
Require Import Lists.List.
Import ListNotations.
Open Scope list_scope.

(** The type used to represent file descriptors. *)
Parameter file_descr : Set.


(** The type used to represent addresses. *)
Parameter sockaddr : Set.

(** The type used to represent addresses. *)
Parameter eqb_sockaddr : sockaddr -> sockaddr -> bool.


(** Transmission direction of the clinent. *)
Inductive Direction : Set :=
| Download
| Upload.

(** Transformation of the abstract Direction type to string representation. *)
Definition dir_to_string (dir:Direction) : string :=
  match dir with
  | Download => "download"%string
  | Upload => "upload"%string
  end.


(** Inputs for the next protocol step generator. *)
Inductive CPInput : Set :=
| Init (dir:Direction) (conn:file_descr)
    (lfname:string) (rfname:string) (mode:string)
    (saport:sockaddr)
| IPacket (content:string) (plen:int)
| PIError (reason:string).

(** Inputs for the next protocol step generator on server side. *)
Inductive SPInput : Set :=
| SIPacket (addr:sockaddr) (buf:string)
| SPIError (reason:string)
| SNothing.

(** The status of the receiving operation. *)
Inductive receive_status : Set :=
| RCVOK      (** Data is received as a result of the communication. *)
| RCVTIMEOUT. (** Data was not received before timeout. *)


(** Internal state of the protocol on the side of the client. *)
Definition CProtoData : Set := file_descr * int.


(** Generic states of a protocol. *)
Inductive PCState : Set :=
(** Protocol is not started yet. *)
| NoOp
(** Protocol is communicating some data. *)
| FComm (pd:CProtoData) (is_timeout:bool)
(** Protocol already finished its operation. *)
| Final
(** Protocol arrived at an error. *)
| SError (reason:string).



(** Internal state of the protocol on the side of the server.
    Very simple version that keeps the map from sockaddresses
    to number of times interaction took place. *)
Definition SProtoData : Set :=
  ((list (sockaddr * int)) * (list file_descr) * sockaddr * file_descr).
(* map from client addresses to the number of iterations *
   the list of sockets to receive data from *
   sockaddr of the current server *
   file descriptior connected to the sockaddr
 *)

(** Generic states of a protocol. *)
Inductive PSState : Set :=
(** Protocol is not started yet. *)
| SNoOp (conn:file_descr) (addr:sockaddr)
(** Protocol is communicating some data. *)
| SFComm (pd:SProtoData) (is_timeout:bool)
(** Protocol already finished its operation. *)
| SFinal
(** Protocol arrived at an error. *)
| SSError (reason:string).


(** Outputs from the next protocol step generator. *)
Inductive POutput : Set :=
| OPacket (content:string)
| Nothing.

(** Return status of the protocol module. *)
Inductive Status : Set :=
| OK
| RunError (reason:string).

Inductive Packet : Set :=
(** First format of packets *)
| Divided (first:string) (second:string)
| NonDivided (data:string).
