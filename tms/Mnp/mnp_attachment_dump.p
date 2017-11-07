/* ----------------------------------------------------------------------
MODULE .......: mnp_attachment_dump.p
TASK .........: MNP cancellation attachments to DWH. YDR-233 
APPLICATION ..: TMS
AUTHOR .......: anttis
CREATED ......: 04.03.11
Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
{Syst/dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR liStart AS INTEGER NO-UNDO.
DEF VAR liEnd AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR lcStatuses AS CHAR NO-UNDO.
DEF VAR liStatusCode AS INT NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

lcStatuses = "0,1,2,3,4".

liStart = INT(STRING(YEAR(TODAY - 1)) +
              STRING(MONTH(TODAY - 1),"99") +
              STRING(DAY(TODAY - 1),"99")).

liEnd = INT(STRING(YEAR(TODAY)) +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99")).

FIND FIRST DumpFile WHERE
           DumpFile.DumpId = icDumpID NO-LOCK.

DO i = 1 TO NUM-ENTRIES(lcStatuses):

   liStatusCode = INT(ENTRY(i,lcStatuses)).

   FOR EACH MNPCancelProposal WHERE
            MNPCancelProposal.StatusCode = liStatusCode AND
            MNPCancelProposal.CreatedTs >= liStart AND
            MNPCancelProposal.CreatedTs < liEnd NO-LOCK:

      FIND FIRST MNPProcess WHERE
                 MNPProcess.MNPSeq = MNPCancelProposal.MNPSeq NO-LOCK NO-ERROR.
      IF NOT AVAIL MNPProcess THEN NEXT.

      IF liStatusCode = 4 THEN
         lcFileName = MNPCancelProposal.AttachmentFile.
      ELSE
         lcFileName = mnpprocess.portrequest + "_" +
                      mnpprocess.formrequest + ".zip".

      COPY-LOB FROM MNPCancelProposal.Pdf TO FILE
         (DumpFile.SpoolDir + "/" + lcFileName).

      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents WITH FRAME fColl.
      END.

   END.

END.

IF oiEvents > 0 THEN DO:
   UNIX SILENT VALUE("tar -cvf " + icFile + " -C " +
                     DumpFile.SpoolDir + " " + ".") 2>/dev/null.
   UNIX SILENT VALUE("rm " + DumpFile.SpoolDir + "/*.zip").
END. /* IF oiEvents > 0 THEN DO: */

