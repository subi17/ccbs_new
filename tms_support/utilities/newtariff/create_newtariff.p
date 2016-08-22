/*------------------------------------------------------------------------
  MODULE .......: create_newtariff.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Sun Feb 15 15:43:03 EET 2015
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{commpaa.i}
katun = "Cron".
gcBrand = "1".
{cparam2.i}
{eventlog.i}
{ftransdir.i}
{tariffconfig.i}
{tariffcons.i}

DEFINE VARIABLE lcLine            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIncDir          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcDir         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessedFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReportFileOut   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBillCode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDBillCode       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLIType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDCLIType        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCliCount        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcPayType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRatePlan        AS CHARACTER NO-UNDO.
DEFINE VARIABLE llgMonFee         AS LOGICAL   NO-UNDO.

DEFINE STREAM sFile.
DEFINE STREAM sLog.

DEFINE TEMP-TABLE ttFiles No-UNDO
   FIELD FName  AS CHARACTER
   FIELD FOrder AS INTEGER
   INDEX FOrder AS PRIMARY FOrder.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "BillItem" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "ShaperConf" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "RatePlan" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "PListConf" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "Tariff" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "PriceList" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "BDest" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "ServiceLimitGroup" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "ServiceLimit" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "ServiceLimitTarget" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "FeeModel" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "FMItem" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "DayCampaign" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "DCServicePackage" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "DCServiceComponent" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "CLIType" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "CTServPac" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "RepText" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "CTServEl" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".

NEW-TARIFF:
DO TRANSACTION:
   ASSIGN
      lcIncDir   = fCParam("TariffCreation","IncDir")
      lcProcDir  = fCParam("TariffCreation","IncProcDir")
      lcSpoolDir = fCParam("TariffCreation","OutSpoolDir")
      lcOutDir   = fCParam("TariffCreation","OutDir").
   
   /* File reading and parsing */
   INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcIncDir).
   
   REPEAT:
   
      IMPORT STREAM sFile UNFORMATTED lcFileName.

      IF INDEX(lcFileName,"billingitem") > 0 THEN DO:
         CREATE ttFiles.
         ASSIGN
            ttFiles.FName  = lcFileName
            ttFiles.FOrder = 1.
      END.
      ELSE IF INDEX(lcFileName,"shaperconf") > 0 THEN DO:
         CREATE ttFiles.
         ASSIGN
            ttFiles.FName  = lcFileName
            ttFiles.FOrder = 2.
      END.
      ELSE IF INDEX(lcFileName,"rateplan") > 0 THEN DO:
         CREATE ttFiles.
         ASSIGN
            ttFiles.FName  = lcFileName
            ttFiles.FOrder = 3.
      END.
      ELSE IF INDEX(lcFileName,"tariffcreation") > 0 THEN DO:
         CREATE ttFiles.
         ASSIGN
            ttFiles.FName  = lcFileName
            ttFiles.FOrder = 4.
      END.  
   END.
   
   FOR EACH ttFiles NO-LOCK:
      DISP ttFiles WITH 2 COl.
   END.

   FOR EACH ttFiles NO-LOCK 
         BY ttFiles.FOrder:
      
      IF INDEX(ttFiles.FName,"billingitem") > 0 THEN 
         RUN billitemcreation.p(lcIncDir,
                                lcSpoolDir) NO-ERROR.
      ELSE IF INDEX(ttFiles.FName,"shaperconf") > 0 THEN
         RUN shaperconfcreation.p(lcIncDir,
                                  lcSpoolDir) NO-ERROR.      
      ELSE IF INDEX(ttFiles.FName,"rateplan") > 0 THEN 
         RUN rateplan.p(lcIncDir,
                        lcSpoolDir,
                        OUTPUT lcPayType,
                        OUTPUT lcRatePlan) NO-ERROR.
      ELSE IF INDEX(ttFiles.FName,"tariffcreation") > 0 THEN 
        RUN tariffcreation.p(lcIncDir,
                             lcSpoolDir,
                             lcPayType,
                             lcRatePlan) NO-ERROR.

      IF ERROR-STATUS:ERROR   OR
         RETURN-VALUE <> "OK" THEN DO:
         UNDO NEW-TARIFF, LEAVE.
      END.
   END.
      
   INPUT STREAM sFile CLOSE.

END. /* do */

