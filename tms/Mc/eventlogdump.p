{Syst/commali.i}
{Func/timestamp.i}
{Syst/dumpfile_run.i}
{Func/cparam2.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE VARIABLE liAmtMod AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcMod    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcKey    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSep    AS CHARACTER NO-UNDO.

DEFINE STREAM sDump.

FIND FIRST DumpFile NO-LOCK WHERE
           DumpFile.DumpId = iiDumpId NO-ERROR.
IF AVAIL DumpFile AND DumpFile.DumpDelimiter > ""
THEN lcSep = DumpFile.DumpDelimiter.
ELSE lcSep = CHR(9).
   

OUTPUT STREAM sDump TO VALUE(icFile).

FOR EACH EventLog NO-LOCK WHERE
         EventLog.EventDate = TODAY - 1 AND
         EventLog.Action    = "Modify":

   ASSIGN
      lcMod = ""
      lcKey = "".
   
   DO liAmtMod = 1 TO NUM-ENTRIES(EventLog.DataValues,CHR(255)) BY 3:

      lcMod = lcMod + ENTRY(liAmtMod,EventLog.DataValues,CHR(255))     + ":"  +
                      ENTRY(liAmtMod + 1,EventLog.DataValues,CHR(255)) + "=>" +
                      ENTRY(liAmtMod + 2,EventLog.DataValues,CHR(255)).
   
     IF liAmtMod + 2 < NUM-ENTRIES(EventLog.DataValues,CHR(255)) THEN
        lcMod = lcMod + "<>".

   END.

   DO liAmtMod = 1 TO NUM-ENTRIES(EventLog.Key,CHR(255)):
      lcKey = lcKey + ENTRY(liAmtMod,EventLog.Key,CHR(255)) + " ".
   END.
   
   PUT STREAM sDump UNFORMATTED 
      EventLog.UserCode  lcSep
      EventLog.Action    lcSep
      EventLog.TableName lcSep
      EventLog.EventTime lcSep
      lcKey              lcSep
      lcMod              CHR(10).
   
   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "EventLog" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.

END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

OUTPUT STREAM sDump CLOSE.
