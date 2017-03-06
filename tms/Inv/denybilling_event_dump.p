/* ----------------------------------------------------------------------
  MODULE .......: denybilling_event_dump.p
  TASK .........: Dumps billing permission create/modify events. YOT-1995
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 15.10.2012
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
{Syst/tmsconst.i}
{Syst/dumpfile_run.i}
{Func/date.i}
{Func/cparam2.i}
{Func/ftransdir.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF STREAM sdump.

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

OUTPUT STREAM sdump TO value (icFile).

DEFINE VARIABLE lcChanges AS CHARACTER NO-UNDO.
DEFINE VARIABLE liEntries AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

FOR EACH EventLog NO-LOCK USE-INDEX EventDate WHERE
         EventLog.EventDate = TODAY - 1 AND
         EventLog.TableName = "Limit" :

   IF ENTRY(2,EventLog.Key,CHR(255)) NE "3" THEN NEXT.

   lcChanges = "".

   IF eventlog.action EQ "Modify" THEN DO:

      liEntries  = num-entries(Eventlog.DataValues, CHR(255)) / 3.

      DO i = 0 TO liEntries  - 1 :
         lcChanges = lcChanges + ";" +
            entry(3 * i + 1,Eventlog.DataValues,CHR(255)) + ":" +
            entry(3 * i + 2,Eventlog.DataValues,CHR(255)) + ":" +
            entry(3 * i + 3,Eventlog.DataValues,CHR(255)).
      END.

      IF lcChanges > "" THEN lcChanges = SUBSTRING(lcChanges,2).
   END.

   PUT STREAM sdump UNFORMATTED
      EventLog.UserCode "|"
      EventLog.TableNAme "|"
      EventLog.Action "|"
      STRING(EventLog.EventDate,"99.99.9999") "|"
      EventLog.EventTime "|"
      ENTRY(1,EventLog.Key,CHR(255)) "|"
      lcChanges SKIP.

   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents WITH FRAME fColl.
   END.
END.

OUTPUT STREAM sdump close.
HIDE FRAME fColl NO-PAUSE.
