/* ----------------------------------------------------------------------
  MODULE .......: limitdump.p 
  TASK .........: Dumps whole limit table (YCM-614)
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 09.06.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/date.i}
{Func/cparam2.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE VARIABLE liLoop     as i  NO-UNDO.
DEFINE VARIABLE liLoopMax  as i  NO-UNDO.
DEFINE VARIABLE lhLimit    AS HANDLE  NO-UNDO.
DEFINE VARIABLE lhField    AS HANDLE  NO-UNDO.
DEFINE VARIABLE lcDelimiter AS CHARACTER NO-UNDO. 


FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN lcDelimiter = DumpFile.DumpDelimiter.

def stream sout.
output stream sout to value(icFile).

FOR EACH Limit NO-LOCK:
   lhLimit = BUFFER Limit:HANDLE.
   liLoopMax = lhLimit:NUM-FIELDS.
   DO liLoop = 1 TO liLoopMax:
      lhField = lhLimit:BUFFER-FIELD(liLoop).
      PUT STREAM sout UNFORMATTED lhField:BUFFER-VALUE.
      IF liLoop < liLoopMax THEN
         PUT STREAM sout UNFORMATTED lcDelimiter.
      ELSE PUT STREAM sout UNFORMATTED SKIP.
   END.
   oiEvents = oiEvents + 1.
END.

output stream sout close.

