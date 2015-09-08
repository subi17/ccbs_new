/* ----------------------------------------------------------------------
  MODULE .......: singlefeelogdump.p 
  TASK .........: Dumps SingleFee events and FixedFee events to DWH YDR-1663
  APPLICATION ..: TMS
  AUTHOR .......: jannetou
  CREATED ......: 19.03.2015
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
/*
The information (fields) we need are:
 Date
 Time
 TMS_username
 MSISDN
 Subscription ID
 Customer number
 Action  --> Field: Create, Delete, Modify
 Details --> Fields:
    When Action=New: All fields of new line
    When Action=Delete: All fields of line deleted
    When Action=Modify: All fields that have changed (old value and new value)
*/

{commali.i}
{timestamp.i}
{cparam2.i}
{tmsconst.i}
{dumpfile_run.i}

DEF INPUT PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF STREAM sout.
OUTPUT STREAM sout TO VALUE(icFile).
DEF STREAM slog.
OUTPUT STREAM slog TO "/scratch/log/fixsinglefee/wrong_hostable.txt" APPEND.

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

DEFINE VARIABLE lcChanges AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liEntries AS INTEGER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMsseq AS INTEGER NO-UNDO. 
DEF VAR  litem AS INT NO-UNDO. 
DEF VAR lcFeeType AS CHAR NO-UNDO INIT "FixedFee,SingleFee".
DEF VAR lcHostTable AS CHAR NO-UNDO. 
DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO.

DO litem = 1 TO NUM-ENTRIES(lcFeeType,","):

   FOR EACH EventLog NO-LOCK WHERE
            EventLog.eventdate = TODAY - 1 AND
            EventLog.tablename = ENTRY(litem,lcFeeType,",") USE-INDEX EventDate:

      IF EventLog.TableName = "SingleFee" THEN DO:
         ASSIGN
            liMsSeq = INT(ENTRY(3,EventLog.Key,CHR(255)))
            lcHostTable = ENTRY(2,EventLog.Key,CHR(255)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN NEXT.
         
      END.

      IF EventLog.TableName = "FixedFee" THEN DO:
         ASSIGN
            liMsSeq = INT(ENTRY(4,EventLog.key,CHR(255)))
            lcHostTable = ENTRY(3,EventLog.Key,CHR(255)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN NEXT.
      END.

      IF lcHostTable NE "Mobsub" THEN DO:
         PUT STREAM slog UNFORMATTED
          lcHostTable ";" liMsSeq ";" TODAY SKIP.
         NEXT.
      END.
      FIND FIRST MobSub NO-LOCK WHERE
                 MobSub.MsSeq = liMsSeq NO-ERROR.
      IF NOT AVAIL MobSub THEN DO:
         FIND FIRST TermMobSub NO-LOCK WHERE
                    TermMobSub.MsSeq = liMsSeq NO-ERROR.
         IF NOT AVAIL TermMobSub THEN NEXT.
         ELSE
            ASSIGN
               lcCLI = TermMobSub.CLI
               liCustNum = TermMobSub.CustNum.
      END.
      ELSE
         ASSIGN
            lcCLI = MobSub.CLI
            liCustNum = MobSub.CustNum.
      
      PUT STREAM sout UNFORMATTED
         EventLog.EventDate "|"
         EventLog.EventTime "|"
         EventLog.UserCode "|"
         lcCLI "|"
         liMsSeq "|"
         liCustNum "|"
         EventLog.Action "|".

      ASSIGN
         lcChanges = ""
         liEntries  = num-entries(EventLog.DataValues, CHR(255)) / 3.

      DO i = 0 TO liEntries  - 1 :
         lcChanges = lcChanges + ";" +
            entry(3 * i + 1,EventLog.DataValues,CHR(255)) + ":" +
            entry(3 * i + 2,EventLog.DataValues,CHR(255)) + ":" +
            entry(3 * i + 3,EventLog.DataValues,CHR(255)).
      END.

      IF lcChanges > "" THEN lcChanges = SUBSTRING(lcChanges,2).

      PUT STREAM sout UNFORMATTED 
         lcChanges SKIP.
      
      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents WITH FRAME fColl.
      END.

   END.  /*  FOR EACH EventLog NO-LOCK WHERE */

END. /*  DO litem = 1 TO NUM-ENTRIES(lcFeeType,","):  */

OUTPUT STREAM sout CLOSE.
OUTPUT STREAM slog CLOSE.
IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE.
