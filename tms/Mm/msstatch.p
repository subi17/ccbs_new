/* ----------------------------------------------------------------------
  MODULE .......: msstatch.p
  TASK .........: Change subscription status in Mobsub->Supervisor Actions
  APPLICATION ..: TMS
  AUTHOR .......: mvi
  CREATED ......: 25.05.05
  CHANGED ......: 26.01.06 jt nam = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                   BUFFER Customer).
                                         
  VERSION ......: TF
  ---------------------------------------------------------------------- */

 
{Syst/commali.i} 
{Syst/eventval.i}

IF llDoEvent THEN DO:

  &GLOBAL-DEFINE STAR_EVENT_USER katun
   
  {lib/eventlog.i}
      
  DEFINE VARIABLE lhMobsub AS HANDLE NO-UNDO.
  lhMobsub = BUFFER Mobsub:HANDLE.
  RUN StarEventInitialize(lhMobsub).
               
END.

DEF INPUT  PARAMETER msseq  AS INT . 

FIND mobsub NO-LOCK WHERE mobsub.msseq = msseq NO-ERROR.

IF NOT AVAIL mobsub THEN RETURN.
FIND Customer OF MobSub.

DEF VAR nam AS C NO-UNDO.

nam = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                        BUFFER Customer).
                                       

DEF VAR msstatus LIKE mobsub.msstatus NO-UNDO FORMAT ">9".

DEF BUFFER xms FOR mobsub.

FORM
SKIP(2)
"  Change status of subscription" SKIP(2)
"  CLI      : " mobsub.cli SKIP
"  Name     : " nam FORMAT "x(40)" SKIP(2)
"  Status   : " msstatus FORMAT ">9" "View list of options by pressing <F9>" SKIP(4)
WITH
   CENTERED WIDTH 60 OVERLAY COLOR VALUE(cfc) TITLE COLOR VALUE(ctc)
   " Mobsub status change " + mobsub.cli + " "
   NO-LABELS FRAME stat.
   


DEF VAR s AS INT NO-UNDO.


/* dont allow to change mobsub.msstatus for subscriptions
   with statuses 2,10,11,12,13,14 */
s =  mobsub.msstatus.
IF katun = "marikav" then do: end.
ELSE IF s = 2  OR 
   s = 10 OR
   s = 11 OR
   s = 12 OR
   s = 13 OR
   s = 14 THEN DO:
   
   MESSAGE "Change not allowed for subscriptions in status:" mobsub.msstatus
       VIEW-AS ALERT-BOX.
   
   RETURN.
END.

ASSIGN msstatus = mobsub.msstatus.

loop:
REPEAT WITH FRAME stat:
   ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 0 ehto = 9 .
   RUN Syst/ufkey.
   DISPLAY
      mobsub.cli
      nam 
   WITH FRAME stat.
   
   UPDATE
      msstatus
   WITH FRAME stat. 

   action:
   REPEAT WITH FRAME stat:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[8] = 8 ufk[5] = 9033  ehto = 0.
      RUN Syst/ufkey.
      IF toimi = 1 THEN LEAVE action.
      IF toimi = 8 THEN DO:
         LEAVE loop.
      END.
      IF toimi = 5 THEN DO:
         MESSAGE "Status changed from " mobsub.msstatus " to " msstatus 
         VIEW-AS ALERT-BOX.
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobsub).
         FIND xms EXCLUSIVE-LOCK WHERE
              xms.msseq = mobsub.msseq.
            ASSIGN xms.msstatus = msstatus.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobsub).
         LEAVE loop.
      END.
   END.
END.

HIDE FRAME stat.
    

  

 
   
