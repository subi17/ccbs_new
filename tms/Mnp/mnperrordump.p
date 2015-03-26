/* ----------------------------------------------------------------------
  MODULE .......: mnperrordump.p
  TASK .........: Report MNP IN/OUT error messages. YNC-77
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 24.02.10
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{date.i}
{cparam2.i}
{ftransdir.i}
{tmsconst.i}

DEF STREAM sdump.

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE VARIABLE lcmsisdns AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMSSeqs AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMNPType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFormRequest AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPortRequest AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDelimiter AS CHARACTER NO-UNDO. 


FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN lcDelimiter = DumpFile.DumpDelimiter.

OUTPUT STREAM sdump TO value (icFile).

ERROR_LOOP:
FOR EACH MNPOperation WHERE
         MNPOperation.ErrorHandled > 0 NO-LOCK:

   FIND MNPProcess WHERE
        MNPProcess.MNPSeq = MNPOperation.MnpSeq NO-LOCK NO-ERROR.

   ASSIGN
      lcmsisdns = ""
      lcMSSeqs = ""
      lcMNPType = ""
      lcFormRequest = ""
      lcPortRequest = "".

   IF AVAIL MNPProcess THEN DO:

      IF LOOKUP(STRING(MNPProcess.MNPType),"1,2") = 0 THEN NEXT ERROR_LOOP.
      
      ASSIGN
         lcFormRequest = MNPProcess.FormRequest
         lcMNPType = STRING(MNPProcess.MNPType).
         lcPortRequest = MNPProcess.PortRequest.
      
      FOR EACH mnpsub where
         mnpsub.mnpseq = mnpprocess.mnpseq NO-LOCK:
         lcmsisdns = lcmsisdns + mnpsub.cli + ",".
         lcMsSeqs = lcMsSeqs + string(mnpsub.msseq) + ",".
      END.
  
   END.
   ELSE DO:
      IF MNPOperation.mnpseq = {&MNP_PROCESS_DUMMY_IN} THEN lcMNPType = "1".
      ELSE IF MNPOperation.mnpseq = {&MNP_PROCESS_DUMMY_OUT} THEN lcMNPType = "2UT".
   END.

   put stream sdump unformatted 
      lcMNPType lcDelimiter
      lcFormRequest lcDelimiter
      lcPortRequest lcDelimiter
      SUBSTRING(lcMsSeqs,1,LENGTH(lcMsSeqs) - 1) lcDelimiter
      SUBSTRING(lcmsisdns,1,LENGTH(lcmsisdns) - 1) lcDelimiter
      fts2hms(mnpoperation.createdts) lcDelimiter
      mnpoperation.errorcode lcDelimiter
      mnpoperation.errordesc skip.

   oiEvents = oiEvents + 1.
END.

OUTPUT STREAM sdump close.
