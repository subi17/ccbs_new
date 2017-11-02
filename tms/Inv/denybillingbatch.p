/* ----------------------------------------------------------------------
  MODULE .......: denybillingbatch 
  TASK .........: Reads and process deny billing files from cron 
----------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.CUICommon:katun = "Cron".
Syst.CUICommon:gcBrand = "1".
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/ftransdir.i}
{Syst/eventval.i}
{Func/multitenantfunc.i}

DEFINE VARIABLE lcIncDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcLogFile AS CHAR NO-UNDO. 
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFileDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTenant AS CHARACTER NO-UNDO. 

DEFINE VARIABLE liRead AS INTEGER NO-UNDO. 
DEFINE VARIABLE liErrors AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO. 
DEF STREAM sFile.

ASSIGN
   lcIncDir    = fCParam("Billing","DenyBillFiles") 
   lcProcDir   = fCParam("Billing","DenyBillArc")
   lcSpoolDir = fCParam("Billing","DenyBillSpool")
   lcOutDir   = fCParam("Billing","DenyBillLog"). 

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcIncDir + lcFileName.
   lcLogFile   = lcSpoolDir + lcFileName + ".log".

   IF SEARCH(lcInputFile) = ? THEN NEXT.
   /* Set effective tenant based on file name. If not regocniced go next file
   */
   lcTenant = ENTRY(1,ENTRY(1,lcFileName,"_"),"-").
   IF NOT fsetEffectiveTenantForAllDB(
         fConvertBrandToTenant(lcTenant)) THEN NEXT.
   fBatchLog("START", lcInputFile).

   liRead = 0.
   liErrors = 0. 

   RUN Inv/denybilling.p (lcInputFile,
                    lcLogFile,
                    OUTPUT liRead,
                    OUTPUT liErrors).

   /* move files */
   IF liRead = 0 THEN 
      lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 

   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).

   fBatchLog("FINISH", lcProcessedFile).  

END.

INPUT STREAM sFile CLOSE.


