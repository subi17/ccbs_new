/* ----------------------------------------------------------------------
  MODULE .......: chKillMs.p
  TASK .........: check IF a mskill request exists FOR a MobSub
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 14.01.01
  CHANGED ......: 05.09.03 jp Brand (checked)
                  04.03.03 tk eventlog
                  
  Version ......: M15
  ---------------------------------------------------------------------- */
{Syst/commali.i}

 DEF INPUT PARAMETER  MsSeq LIKE MobSub.MsSeq   NO-UNDO.
 DEF OUTPUT PARAMETER rc     AS   I             NO-UNDO.

{Syst/eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhKillMS AS HANDLE NO-UNDO.
   lhKillMS = BUFFER KillMS:HANDLE.
   RUN StarEventInitialize(lhKillMS).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhKillMS).
   END.
END.


 /* rc values:  0: KillMs NOT found; no actions were taken
                1: KillMs found AND ramained unchanged
                2: KillMs found AND was removed
                9: No MobSub was found  
*/                

 DEF VAR ok      AS LO NO-UNDO INIT FALSE.

 FIND MobSub WHERE MobSub.MsSeq = MsSeq NO-LOCK NO-ERROR.
 IF NOT AVAIL MobSub THEN DO:
    MESSAGE
    "There is no mobile subscription" SKIP
    "with MsSeq value" MsSeq "!" SKIP(1)
    "Contact system support"
    VIEW-AS ALERT-BOX TITLE " SYSTEM ERROR ".
    rc = 9.
END.
ELSE DO:

   /* check AND warn IF a KillMs record exists */
   FIND FIRST KillMs WHERE 
              KillMs.MsSeq = MobSub.MsSeq AND
              KillMs.Stat < 2 /* pending OR failed */
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL KillMs THEN DO:
      MESSAGE 
      "There is already a scheduled KILL request" SKIP
      "for Mobile Subscription" KillMs.CLI      SKIP
      "Saved by user '" + KillMs.UserCode + "'"     SKIP
      "Proposed day of deactivation" STRING(KillMs.KillDate,"99.99.9999") SKIP
      "Status" ENTRY(KillMs.Stat,"PENDING,FAILED") SKIP(1)

      "DO YOU WANT TO CANCEL THIS REQUEST ?"
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.

      IF ok THEN DO:
         IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhKillMS).
         DELETE KillMs .
         MESSAGE "KILL request was succesfully deleted"
         VIEW-AS ALERT-BOX.
         rc = 2.
      END.
      ELSE DO:
         MESSAGE "KILL request was not deleted"
         VIEW-AS ALERT-BOX.
         rc = 1.
      END.          
   END.   
   ELSE rc = 0.
END. /* MobSub was found */

