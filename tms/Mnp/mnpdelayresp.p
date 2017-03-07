/* ----------------------------------------------------------------------
  MODULE .......: mnpdelayresp.p
  TASK .........: Report MNP OUT delayed rejections/confirms. YNC-81
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 26.02.10
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/date.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/tmsconst.i}
{Mnp/mnp.i}
{Func/email.i}

DEF STREAM sdump.

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.


DEFINE VARIABLE lcReportFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcmsisdns AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOperName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProposal AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldaDueDate AS DATE NO-UNDO. 
DEFINE VARIABLE liSlotsAfterCreation AS INTEGER NO-UNDO.
DEFINE VARIABLE liNumErr AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcConfDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDelimiter AS CHARACTER NO-UNDO. 


FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN lcDelimiter = DumpFile.DumpDelimiter.

OUTPUT STREAM sdump TO value (icFile).

CHECK_LOOP:
FOR EACH MNPProcess WHERE
   MNPProcess.Brand = gcBrand AND
   MNPProcess.MNPType = {&MNP_TYPE_OUT} AND
   MNPProcess.StatusCode = {&MNP_ST_ASOL} NO-LOCK:

   liSlotsAfterCreation = fMNPPeriods(
      input MNPProcess.CreatedTS,
      input fMakeTS(),
      INPUT 0,
      OUTPUT ldaDueDate).
    
   IF liSlotsAfterCreation >= 4 THEN DO:
      
      lcmsisdns = "".
      FOR EACH mnpsub where
         mnpsub.mnpseq = mnpprocess.mnpseq NO-LOCK:
         lcmsisdns = lcmsisdns + mnpsub.cli + ",".
      END.
      
      CASE MNPProcess.StateFlag:
         WHEN {&MNP_STATEFLAG_CONFIRM_PROPOSAL} OR
            WHEN {&MNP_STATEFLAG_CONFIRM} THEN lcProposal = "ACON".
         WHEN {&MNP_STATEFLAG_REJECT_PROPOSAL} OR 
            WHEN {&MNP_STATEFLAG_REJECT} THEN lcProposal = "AREC".
         OTHERWISE lcProposal = "".
      END.

      FIND MNPOperator WHERE 
           MNPOperator.Brand = gcBrand AND
           MNPOperator.OperCode = MNPProcess.OperCode
      NO-LOCK NO-ERROR.
      IF AVAIL MNPOperator THEN lcOperName = MNPOperator.OperName.
      ELSE IF AMBIGUOUS(MNPOperator) THEN DO:
         FIND FIRST MNPOperator WHERE 
                    MNPOperator.Brand = gcBrand AND
                    MNPOperator.OperCode = MNPProcess.OperCode
         NO-LOCK.
         IF AVAIL MNPOperator THEN lcOperName = MNPOperator.OperBrand.
         ELSE lcOperName = "".
      END.
      ELSE lcOperName = "".
   
      put stream sdump unformatted 
         MNPProcess.PortRequest lcDelimiter
         SUBSTRING(lcmsisdns,1,LENGTH(lcmsisdns) - 1) lcDelimiter
         fTS2HMS(MNPProcess.CreatedTS) lcDelimiter
         fTS2HMS(MNPProcess.PortingTime) lcDelimiter
         lcProposal lcDelimiter
         MNPProcess.OperCode lcDelimiter
         lcOperName skip.

      liNumErr = liNumErr + 1.
      oiEvents = oiEvents + 1.
   END.

END.

OUTPUT STREAM sdump close.
   
IF liNumErr > 0 THEN DO:
   /* mail recipients */
   GetRecipients(lcConfDir + "mnpdelayresp.email").
   /* send via mail */
   SendMail(lcReportFile,"").
END.
