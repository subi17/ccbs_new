/* ----------------------------------------------------------------------
MODULE .......: extapi_dump.p
TASK .........: Exernal API log dump for the Track. SER-465, YOT-1223
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 13.02.2012
Version ......: yoigo
----------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{cparam2.i}
{forderstamp.i}
{ftransdir.i}
{tmsconst.i}
{dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR ldtFrom AS DATETIME NO-UNDO.
DEF VAR ldtTo AS DATETIME NO-UNDO.

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

def stream sout.
output stream sout to value(icFile).

ldtFrom = DATETIME(TODAY - 1,0).
ldtTo = DATETIME(TODAY,0).

FOR EACH authlog NO-LOCK where
         authlog.timestamp >= ldtFrom AND
         authlog.timestamp < ldtTo:

   if authlog.username ne "selfservice" then next.
   if authlog.enduserid = "Monitoring" then next.

   PUT STREAM SOUT UNFORMATTED 
      authlog.username "|"
      authlog.methodname "|"
      authlog.enduserid "|"
      authlog.timestamp "|"
      (IF authlog.responsets ne ? then
         STRING(authlog.responsets) else "") "|"
      (IF authlog.responsets ne ? then
         STRING((authlog.responsets - authlog.timestamp)) else "") "|"
      authlog.errorcode "|"
      authlog.errormsg "|"
      authlog.transactionid skip.

   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents WITH FRAME fColl.
   END.
end.
