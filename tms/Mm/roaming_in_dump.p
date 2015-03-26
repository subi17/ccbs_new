/* ----------------------------------------------------------------------
  MODULE .......: roaming_in_dump.p 
  TASK .........: Dumps whole roamcdr and roamgprs tables (YCM-798)
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 14.08.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{date.i}
{cparam2.i}
{tsformat.i}

DEF INPUT PARAMETER idaEventDate AS DATE NO-UNDO.

DEFINE VARIABLE filename   as c  NO-UNDO.
DEFINE VARIABLE lcBaseDir  as c  NO-UNDO.
DEFINE VARIABLE lcOutDir   as c  NO-UNDO.
DEFINE VARIABLE lcSpoolDir as c  NO-UNDO.
DEFINE VARIABLE numform    as c  NO-UNDO.
DEFINE VARIABLE liLoop     as i  NO-UNDO.
DEFINE VARIABLE liLoopMax  as i  NO-UNDO.
DEFINE VARIABLE lhRoamCDR  AS HANDLE  NO-UNDO.
DEFINE VARIABLE lhField    AS HANDLE  NO-UNDO.
DEFINE VARIABLE lcTime     AS CHARACTER NO-UNDO.

assign
   lcTime = fTSFormat("yyyymmddHHMMss",fMakeTS())
   lcBaseDir  = fCparam("dumpdir","roamoutdump.p")
   lcSpoolDir = lcBaseDir + "spool/"
   lcOutDir   = lcBaseDir + "outgoing/"
   filename   = "daily_roamingout_" + lcTime + ".dump"
   numform    = session:numeric-format
   session:numeric-format = "AMERICAN".

def stream sout.
output stream sout to value(lcSpoolDir + filename).

FOR EACH RoamCDR WHERE RoamCDR.DateRead = idaEventDate NO-LOCK:
   lhRoamCDR = BUFFER RoamCDR:HANDLE.
   liLoopMax = lhRoamCDR:NUM-FIELDS.
   DO liLoop = 1 TO liLoopMax:
      lhField = lhRoamCDR:BUFFER-FIELD(liLoop).
      PUT STREAM sout UNFORMATTED lhField:BUFFER-VALUE.
      IF liLoop < liLoopMax THEN
         PUT STREAM sout UNFORMATTED "\"|\"".
      ELSE PUT STREAM sout UNFORMATTED SKIP.
   END.
END.

output stream sout close.
unix silent value("mv " + lcSpoolDir + filename + " " + lcOutDir).
filename = "daily_roamingout_gprs_" + lcTime + ".dump".
output stream sout to value(lcSpoolDir + filename).

FOR EACH RoamGPRS WHERE RoamGPRS.DateRead = idaEventDate NO-LOCK:
   lhRoamCDR = BUFFER RoamGPRS:HANDLE.
   liLoopMax = lhRoamCDR:NUM-FIELDS.
   DO liLoop = 1 TO liLoopMax:
      lhField = lhRoamCDR:BUFFER-FIELD(liLoop).
      PUT STREAM sout UNFORMATTED lhField:BUFFER-VALUE.
      IF liLoop < liLoopMax THEN
         PUT STREAM sout UNFORMATTED "\"|\"".
      ELSE PUT STREAM sout UNFORMATTED SKIP.
   END.
END.

output stream sout close.

unix silent value("mv " + lcSpoolDir + filename + " " + lcOutDir).

session:numeric-format = numform.

