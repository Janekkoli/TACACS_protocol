Require Import ZArith. (** Necessary for Lia *)
Require Import Lia. (** Necessary for lia *)
Require Import Int63.Sint63.
Require Import PArray.
Require Import Ascii String Coq.Strings.Byte.
Require Import Lists.List.
Import ListNotations.
Open Scope list_scope.


Require Import ssreflect ssrbool.

(** We need compiled versions of the files with the modules.
Therefore we make the compilation first. *)
Require Import ProtocolDefinitions.
Require Import Helper.
Include Helper.

Lemma string_len_zero:
  forall s,
    String.length s = 0 -> s = ""%string.
Proof.
  intros.
  (* Proof by cases depending on s *)
  destruct s.
  * admit.
  * admit.
Admitted.

Lemma prefix_empty:
  forall s,
    prefix "" s = true.
Proof.
  intros.
  (* Proof by cases; all cases are very simple so one
     way to prove them is enough. *)
  admit.
Admitted.


Lemma full_substring:
  forall str,
    str = substring 0 (String.length str) str.
Proof.
  induction str.
  * simpl;trivial.
  * simpl.
    (* We use f_equal when a constructor (String this time) is applied
       on both sides of an equality *)
    f_equal.
    trivial.
Qed.

Lemma substring_string:
  forall str n a,
    substring 0 (S n) (String a str) =
      String a (substring 0 n str).
Proof.
  destruct str.
  * intros.
    simpl.
    destruct n; auto.
  * intros.
    unfold substring at 1.
    fold substring.
    destruct n.
    ** admit.
    ** admit.
Admitted.

Lemma string_app:
  forall str str1 a,
    ((String a str) ++ str1  = String a (str ++ str1))%string.
Proof.
  induction str; intros; now simpl.
Qed.

Lemma string_app_assoc:
  forall a b c,
    (a ++ b ++ c = (a ++ b) ++ c)%string.
Proof.
  induction a.
  * now simpl.
  * intros. rewrite string_app.
    admit.
Admitted.

Lemma substring_zero_zero:
  forall str,
    substring 0 0 str = ""%string.
Proof.
  destruct str; now simpl.
Qed.

Lemma substring_length:
  forall str str1,
    str = substring 0 (String.length str) (str ++ str1).
Proof.
  induction str.
  * intros.
    simpl.
    admit.
  * intros. simpl.
    admit.
Admitted.


Lemma string_app_length:
  forall str str1,
    String.length (str ++ str1)  = String.length str + String.length str1.
Proof.
  induction str.
  * intros.
    admit.
  * intros.
    simpl.
    admit.
Admitted.

Lemma substring_app_skip:
  forall fs sn k,
    substring (String.length fs) k (fs ++ sn)%string =
      substring 0 k sn.
Proof.
  induction fs.
  * intros.
    admit.
  * intros.
    simpl.
    admit.
Admitted.

Lemma index_app:
  forall fs sep sn,
    String.length sep = 1 ->
    index 0 sep fs = None ->
    index 0 sep (fs ++ sep ++ sn) = Some (String.length fs).
Proof.
  induction fs.
  * intros.
    destruct sep;simpl in H0; try congruence.
    simpl.
    simpl in H.
    destruct (ascii_dec a a); try congruence.
    destruct sep; simpl in H; try lia.
    simpl.
    admit.
  * intros. 
    destruct sep; simpl in H; try lia.
    destruct sep; simpl in H; try lia.
    simpl.
    destruct (ascii_dec a0 a).
    ** subst a.
       simpl in H0.
       destruct (ascii_dec a0 a0); try congruence.
       admit.
    ** simpl in H0.
       destruct (ascii_dec a0 a); try congruence.
       destruct (index 0 (String a0 "") fs) eqn: Idx; try congruence.
       apply (IHfs (String a0 "") sn)%string in Idx; try simpl; auto.
       simpl in Idx.
       admit.
Admitted.



Lemma divide:
  forall c str n,
    index 0 c str = Some n ->
    1 = String.length c ->
    str = ((substring 0 n str) ++
             c ++
             (substring (n+1)
                ((String.length str) - n - 1) str))%string.
Proof.
  induction str.
  * intros. 
    simpl in H.
    ** destruct c.
       *** inversion H.
           subst n.
           simpl;auto.
       *** inversion H.
  * intros. 
    destruct c.
    ** simpl in H0.
       congruence.
    ** simpl in H0.
       destruct (String.length c) eqn: lc.
       *** clear H0.
           apply string_len_zero in lc. 
           subst c.   
           destruct (ascii_dec a0 a).
           **** subst a0.
                (* Tactics with e in front are applied when not all
                   universally quantified variables are knonw. These
                   unknown variables will be fixed in the course of
                   the proof. *)
                assert (prefix (String a "") (String a str) = true). {
                  admit.
                }
                assert (n=0). {
                  admit.
                }
                subst n.
                simpl.
                replace (String.length str - 0) with (String.length str) by lia.
                now rewrite <- full_substring.
           **** destruct n.
                simpl in H.
                destruct (ascii_dec a0 a); try congruence.
                + destruct (index 0 (String a0 "") str) eqn: Idx;
                    try inversion H. 
                + rewrite substring_string. 
                  rewrite string_app.
                  simpl in H.
                  destruct (ascii_dec a0 a); try congruence.
                  destruct (index 0 (String a0 "") str) eqn: Idx; try congruence.
                  f_equal.
                  inversion H.
                  admit.
       *** admit.
Admitted.



(** There are two forms of packet:
  - with separator ":" that divides data
    to chunk before the first ":" and after the first ":"
  - with no occurrence of ":" that contains only one
    chunk of data *)
Definition parse_content (content:string) :=
  let poso := index 0 ":"%string content in
  match poso with
  | Some pos =>
      let f := substring 0 pos content in
      let s := substring (pos+1) ((String.length content) - pos - 1) content in
          Divided f s
  | None => NonDivided content
end.

Definition print_content (pckt:Packet) :=
  match pckt with
  | Divided f s => (f ++ ":" ++ s)%string
  | NonDivided d => d
  end.

Lemma print_content_parse:
  forall s,
    print_content (parse_content s) = s.
Proof.
  intros.
  unfold parse_content.
  destruct (index 0 ":" s) eqn: Idx.
  * simpl.
    apply divide in Idx; try now simpl.
  * now simpl.
Qed.    

Lemma parse_content_print:
  forall f s,
    parse_content (print_content (Divided f s)) = Divided f s.
Proof.
  intros.
  simpl.
  unfold parse_content.
  destruct (index 0 ":" (f ++ String ":" s)) eqn: Idx.
  * f_equal.
    ** (* apply (index_app f ":"%string s) in H; try now simpl.
       simpl in H.
       rewrite H in Idx.
       inversion Idx.
       subst n.
       now rewrite <- substring_length.
        *)
      admit.
    ** assert (String.length (f ++ ":")%string = n+1). {
         admit.
       }
       admit.
       (*
       rewrite <- H0.
       replace (f ++ String ":" s)%string with ((f ++ ":") ++ s)%string by
         (rewrite <- string_app_assoc; now simpl).
       rewrite substring_app_skip.
       rewrite string_app_length.
       rewrite H0.
       replace (n + 1 + String.length s - n - 1) with (String.length s) by lia.
       now rewrite <- full_substring.
       *)
  *  admit.
Admitted.

(** Tactics Simple https://www.cs.cornell.edu/courses/cs3110/2018sp/a5/coq-tactics-cheatsheet.html *)

Definition mystring_of_num (num:int) :string :=
  if (num =? 0)%uint63
  then "0"%string
  else if (num =? 1)%uint63
       then "1"%string
       else if (num =? 2)%uint63
            then "2"%string
            else "other"%string.


(** Generate reply in client *)
Definition communication_reply (pd:CProtoData) (inp:CPInput) :=
  match inp with
  | IPacket content plen =>
      let pcontent := parse_content content in
      match pcontent with
      | NonDivided data =>
          let (desc, num) := pd in
          let mione := (num-1)%uint63 in 
          if (num =? 0)%uint63
          then (Final, Nothing)
          else (FComm (desc, mione) false,
                 OPacket (data ++ ":" ++ (mystring_of_num mione))%string)
      | Divided f s => (Final, Nothing)
      end
  | PIError reason => (SError reason, Nothing)
  | Init _ _ _ _ _ _ =>
    (SError "Initialisation data not allowed durign communication",
      Nothing)
  end.


           
(** Function makes the transfer from current state and input of the
    protocol to the nest state and relevant output in case of the
    client.*)
Definition proto_next (st : PCState) (inp:CPInput) : (PCState * POutput) :=
  match st with
  | NoOp => (** initialisation of the communication *)
      (match inp with
       | Init Upload conn lfn rfn mname saddr =>
          init_upload_connection conn lfn rfn mname saddr
       | Init Download conn lfn rfn mname saddr =>
           init_download_connection conn lfn rfn mname saddr
       | IPacket _ _ =>
          (SError "Packet is invalid at initialisation", Nothing)
       | PIError msg => (SError msg, Nothing)
       end)
  | FComm pd tm => (** normal communication *)
     if tm
     then (* handling of the timeout case *)
       communication_timeout pd 
     else (* handling protocol reply *)
       communication_reply pd inp
  | Final => (* We assume that the decision to finalize is on the side of
                protocol step, so this case should not take place  *)
     (SError "Wrong protocol handling", Nothing) 
  | SError msg => (** error *)
      (* Nothing can be done here. *)
      (SError msg, Nothing)
  end.

Definition update_condata_raw
  (condata:list (ProtocolDefinitions.sockaddr * int))
  (sad:ProtocolDefinitions.sockaddr) :=
  flat_map (fun (x:(ProtocolDefinitions.sockaddr * int)) =>
              let (sa, i) := x in
              if ProtocolDefinitions.eqb_sockaddr sa sad
              then [(sa, (i+1)%uint63)]
              else [(sa, i)]) condata.

Definition remove_condata
  (condata:list (ProtocolDefinitions.sockaddr * int))
  (sad:ProtocolDefinitions.sockaddr) :=
  fold_left (fun (acc:list (ProtocolDefinitions.sockaddr * int))
                 (el:(ProtocolDefinitions.sockaddr * int)) =>
               let (sa, i) := el in
               if ProtocolDefinitions.eqb_sockaddr sa sad
               then acc
               else (sa,i) :: acc)
    condata [].


      
Definition update_condata :=
  fun
    (condata:list (ProtocolDefinitions.sockaddr * int))
    (conns:list ProtocolDefinitions.file_descr)
    (sad:ProtocolDefinitions.sockaddr)
    (dat:string) =>
    let pcnt := parse_content dat in
    match pcnt with
    | Divided f s => 
        (* this means we end the transmission *)
        let ucondata := remove_condata condata sad in
        ((ucondata, conns), print_content pcnt)
    | NonDivided st =>
        (* this means we repeat the transmission *)
        let ucondata := update_condata_raw condata sad in
        ((ucondata, conns), print_content pcnt)
    end.
    
(** Generate reply *)
Definition communication_srvr_reply
      (pd:SProtoData) (inp:SPInput) :=
  let (co, scon) := pd in
  let (co1, saddr) := co in
  let (condata, conns) := co1 in
  match inp with
  | SIPacket sad rdata =>
      let (cc, data) := update_condata condata conns sad rdata in
      let (ncondata, nconns) := cc in
      let pst := SFComm (((ncondata, nconns), saddr), scon) false in
      (pst, OPacket data)
  | SPIError msg => (SSError msg, Nothing)
  | SNothing => (SSError "Empty input data", Nothing)
  end.



Definition proto_srvr_next (st : PSState) (inp:SPInput)  : (PSState * POutput) :=
  match st with
  | SNoOp conn saddr => (** initialisation of the communication *)
      (match inp with
       | SIPacket addr content =>
           communication_srvr_reply ([(addr,1%uint63)],[], saddr, conn) inp
       | SPIError msg => (SSError msg, Nothing)
       | SNothing => (SSError "Initialisation is elsewhere", Nothing)
       end)
  | SFComm pd tm => (** normal communication *)
      communication_srvr_reply pd inp
  | SFinal => (* We assume that the decision to finalize is on the side of
                protocol step, so this case should not take place  *)
     (SSError "Wrong protocol handling", Nothing) 
  | SSError msg => (** error *)
      (* Nothing can be done here. *)
      (SSError msg, Nothing)
  end.
