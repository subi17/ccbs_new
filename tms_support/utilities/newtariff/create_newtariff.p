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

{Func/cparam2.i}

DEFINE VARIABLE lcLine            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName        AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE lcIncDir          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessedFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReportFileOut   AS CHARACTER NO-UNDO.
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
   FIELD FBaseName  AS CHARACTER
   FIELD Program AS CHARACTER
   FIELD FOrder AS INTEGER
   INDEX FOrder AS PRIMARY FOrder.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/*
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
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "ProgLimit" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "Matrix" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "MXItem" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "RequestAction" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "RequestActionRule" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "CTServAttr" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "SLGAnalyse" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".
RUN /apps/yoigo/tms_support/utilities/tabledump/export_table.p "TMRItemValue" "/apps/yoigo/tms_support/utilities/newtariff/exportdata".

*/

FUNCTION fCreatettFiles RETURNS LOGICAL
   (icBaseName AS CHARACTER,
    icFileName AS CHARACTER,
    icProgram  AS CHARACTER,
    iiOrder    AS INTEGER):

   CREATE ttFiles.
   ASSIGN
      ttFiles.FName   = icFileName
      ttFiles.FBaseName = icBaseName
      ttFiles.Program = icProgram
      ttFiles.FOrder  = iiOrder.
      
   RETURN FALSE.

END FUNCTION.

ASSIGN
   lcIncDir   = fCParam("TariffCreation","IncDir")
   lcSpoolDir = fCParam("TariffCreation","OutSpoolDir").

NEW-TARIFF:
DO TRANSACTION:
   
   /* File reading and parsing */
   INPUT STREAM sFile FROM OS-DIR(lcIncDir).
   REPEAT:   
      IMPORT STREAM sFile lcFileName.
      
      IF INDEX(lcFileName[3],"F") EQ 0
      THEN NEXT.

      CASE ENTRY(1,lcFileName[1], "."):

         WHEN "billitem"
         THEN fCreatettFiles(lcFileName[1], lcFileName[2], "utilities/newtariff/billitemcreation.p", 1).
         WHEN "billitem_translation"
         THEN fCreatettFiles(lcFileName[1], lcFileName[2], "utilities/newtariff/billitemtrans.p", 2).
         WHEN "bundle"
         THEN fCreatettFiles(lcFileName[1], lcFileName[2], "utilities/newtariff/bundlecreation.p", 3). /* DayCampaign */
         WHEN "shaperconf"
         THEN fCreatettFiles(lcFileName[1], lcFileName[2], "utilities/newtariff/shaperconfcreation.p", 4).
         WHEN "tariff"
         THEN fCreatettFiles(lcFileName[1], lcFileName[2], "utilities/newtariff/tariffcreation.p", 5). /* Actually CLIType not Tariff! */
         WHEN "tariff_translation"
         THEN fCreatettFiles(lcFileName[1], lcFileName[2], "utilities/newtariff/tarifftrans.p", 6).
         WHEN "rptariff"
         THEN fCreatettFiles(lcFileName[1], lcFileName[2], "utilities/newtariff/rptariff.p", 7).
      END CASE.
   END.

   INPUT STREAM sFile CLOSE.

   FOR EACH ttFiles NO-LOCK:
      DISP ttFiles WITH 2 COl.
   END.

   FOR EACH ttFiles NO-LOCK 
         BY ttFiles.FOrder:
      
      RUN VALUE(ttFiles.Program) (ttFiles.FBaseName,ttFiles.FName,lcSpoolDir) NO-ERROR.
      
      IF ERROR-STATUS:ERROR   OR
         RETURN-VALUE <> "OK" THEN DO:
         MESSAGE RETURN-VALUE error-status:get-message(1) view-as ALERT-BOX.
         UNDO NEW-TARIFF, LEAVE.
      END.
   END.
END. /* do */
