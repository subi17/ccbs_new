/* ----------------------------------------------------------------------
  MODULE .......: bob_yoicard_discount.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: susanjee
  CREATED ......: 16.05.18
  Version ......: Yoigo
----------------------------------------------------------------------- */

/* ***************************  Definitions  ************************** */
{Syst/commpaa.i}
Syst.Var:katun   EQ "Cron".
Syst.Var:gcBrand EQ "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/coinv.i}
{Func/msisdn_prefix.i}
{Mc/dpmember.i}

DEFINE VARIABLE lcLine   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSep    AS CHARACTER NO-UNDO INITIAL ";".
DEFINE VARIABLE liNumOK  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liNumErr AS INTEGER   NO-UNDO.

/* files and dirs */
DEFINE VARIABLE lcLogFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIncDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessDir    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcDir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir        AS CHARACTER NO-UNDO.

/* field variables */
DEFINE VARIABLE liOrderID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE liActivFlag     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcStoreID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLeadFlag      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcYOICardLogMsg AS CHAR      NO-UNDO. 
DEFINE VARIABLE lcDiscLogMsg    AS CHAR      NO-UNDO. 

/* streams */
DEFINE STREAM sin.
DEFINE STREAM sFile.
DEFINE STREAM sLog.

DEFINE BUFFER lbOrder      FOR Order.
DEFINE BUFFER lbMobSub     FOR MobSub.
DEFINE BUFFER lbTermMobSub FOR TermMobSub.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN
   lcIncDir     = fCParam("YOICard","IncDirDisc")
   lcProcessDir = fCParam("YOICard","IncProcessDir")
   lcProcDir    = fCParam("YOICard","IncProcDir")
   lcSpoolDir   = fCParam("YOICard","OutSpoolDir")
   lcOutDir     = fCParam("YOICard","OutDir").

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
      icMessage SKIP.

END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).

   liNumErr = liNumErr + 1.

END FUNCTION.

FUNCTION fUpdateCreditCardStatus RETURNS LOGICAL
   (INPUT  iiMsSeq   AS INT,
    OUTPUT lcLogData AS CHAR):
   
   DEFINE BUFFER lbDCCLI FOR DCCLI.

   DEF VAR liPrevYOICardStatus AS INT NO-UNDO.  
   
   FIND FIRST lbDCCLI EXCLUSIVE-LOCK WHERE 
              lbDCCLI.Brand   EQ Syst.Var:gcBrand   AND
              lbDCCLI.DCEvent EQ {&YOICARD_DCEvent} AND 
              lbDCCLI.MsSeq   EQ iiMsSeq            AND 
              lbDCCLI.ValidTo >= TODAY              NO-ERROR.                    
                     
   IF NOT AVAILABLE lbDCCli THEN DO:
      fError("Inactive Periodical Contract").
      RETURN FALSE.
   END.

   ASSIGN liPrevYOICardStatus   = 0
          liPrevYOICardStatus   = lbDCCLi.ServiceStatus  
          lbDCCLi.ServiceStatus = {&YOICARD_STATUS_ACTIVADA}.
  
   lcLogData = "YOICard status changed from" + " " + 
               STRING(liPrevYOICardStatus)   + " " + 
               STRING(lbDCCLi.ServiceStatus).

   RETURN TRUE.
   
END FUNCTION.

FUNCTION fApplyYOICardDiscount RETURNS LOGICAL
   (INPUT iiMsSeq       AS INT,
    INPUT lcYOICardDisc AS CHAR,
    INPUT idtFromDate   AS DATE,
    INPUT iiOrderID     AS INT,
    OUTPUT lcLogData    AS CHAR):
   
   DEF VAR lcResult AS CHAR NO-UNDO.

   DEFINE BUFFER lbDiscountPlan FOR DiscountPlan.
   DEFINE BUFFER lbDPRate       FOR DPRate.

   IF NOT CAN-FIND(FIRST lbDiscountPlan NO-LOCK WHERE
                         lbDiscountPlan.Brand    EQ Syst.Var:gcBrand AND
                         lbDiscountPlan.DPRuleID EQ lcYOICardDisc    AND
                         lbDiscountPlan.ValidTo  >= idtFromDate)     THEN
   DO:
      fError("ERROR:Discount config not available").
      RETURN FALSE.
   END.                      

   FOR FIRST lbDiscountPlan NO-LOCK WHERE
             lbDiscountPlan.Brand    EQ Syst.Var:gcBrand AND
             lbDiscountPlan.DPRuleID EQ lcYOICardDisc    AND
             lbDiscountPlan.ValidTo  >= idtFromDate,
       FIRST lbDPRate NO-LOCK WHERE
             lbDPRate.DPId      EQ lbDiscountPlan.DPId AND
             lbDPRate.ValidFrom <= idtFromDate         AND
             lbDPRate.ValidTo   >= idtFromDate:

      fCloseDiscount(lbDiscountPlan.DPRuleID,
                     iiMsSeq,
                     idtFromDate - 1,
                     NO).

      lcResult = fAddDiscountPlanMember(iiMsSeq,
                                        lbDiscountPlan.DPRuleID,
                                        lbDPRate.DiscValue,
                                        idtFromDate,
                                        ?,
                                        lbDiscountPlan.ValidPeriods,
                                        iiOrderID).

      IF lcResult BEGINS "ERROR" THEN DO:
         fError("ERROR:YOICard Discount not created; " + lcResult).
         RETURN FALSE.
      END.
      ELSE 
         lcLogData = "YOICard "    + " " + 
                     lcYOICardDisc + " " + 
                     "request is created".

   END.

   RETURN TRUE.

END FUNCTION.    


/********************** Execution starts here ***************************/

INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.

      lcProcessFile = fMove2TransDir(lcInputFile, "", lcProcessDir).

      IF lcProcessFile EQ "" THEN NEXT.

      IF NOT SEARCH(lcProcessFile) EQ ? THEN
         INPUT STREAM sin FROM VALUE(lcProcessFile).
      ELSE NEXT.

   END.
   ELSE NEXT.

   ASSIGN
      liNumOk  = 0
      liNumErr = 0.

   fBatchLog("START", lcProcessFile).

   lcLogFile = lcSpoolDir + lcFileName + ".log".

   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.

   RUN pUpdateYOICardStatusAndDiscount.

   PUT STREAM sLog UNFORMATTED
       "input: "   STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK) ", "
       "errors: "  STRING(liNumErr) SKIP.

   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   ASSIGN lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir)
          lcProcessedFile = fMove2TransDir(lcProcessFile, "", lcProcDir).

   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).

END. /* REPEAT */

INPUT STREAM sFile CLOSE.

PROCEDURE pUpdateYOICardStatusAndDiscount:       
      
   REPEAT TRANSACTION:

      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.
 
      IF NUM-ENTRIES(lcLine,lcSep) < 4 THEN DO:
         fError("Invalid line format").
         NEXT.
      END.

       /*[Order ID];[ActivationFlag];[Store ID];[Lead Flag]*/
      ASSIGN
         lcYOICardLogMsg = "" 
         lcDiscLogMsg    = ""  
         liOrderID       = INT(ENTRY(1,lcLine,lcSep))
         liActivFlag     = INT(ENTRY(2,lcLine,lcSep))
         lcStoreID       = ENTRY(3,lcLine,lcSep)
         liLeadFlag      = INT(ENTRY(4,lcLine,lcSep)) NO-ERROR.

      FIND FIRST lbOrder NO-LOCK WHERE
                 lbOrder.Brand   EQ Syst.Var:gcBrand AND  
                 lbOrder.OrderId EQ liOrderID        NO-ERROR. 
  
      IF NOT AVAILABLE lbOrder THEN DO:
         fError("Invalid Order").
         NEXT.
      END.
      ELSE IF LOOKUP(lbOrder.Statuscode, {&ORDER_INACTIVE_STATUSES}) EQ 0 THEN DO:
         fError("Ongoing Order").
         NEXT.
      END. 
      ELSE  IF LOOKUP(lbOrder.Statuscode, {&ORDER_CLOSE_STATUSES}) > 0 THEN DO: 
         fError("Closed / Canceled Order").
         NEXT.
      END.
  
      FIND FIRST lbMobSub NO-LOCK WHERE 
                 lbMobSub.MsSeq EQ lbOrder.MsSeq NO-ERROR.

      IF NOT AVAILABLE lbMobSub THEN DO:  

         FIND FIRST lbTermMobSub NO-LOCK WHERE
                    lbTermMobSub.MsSeq EQ lbOrder.MsSeq NO-ERROR.

         IF AVAILABLE lbTermMobSub THEN DO:
            ferror("Inactive Subscription").
            NEXT.
         END.

         fError("Invalid Mobile Subscription").
         NEXT.

      END.
  
      CASE liActivFlag:
         WHEN 1 THEN     
            fUpdateCreditCardStatus(lbMobSub.MsSeq,
                                    OUTPUT lcYOICardLogMsg).           
         WHEN 2 THEN DO: 
            fUpdateCreditCardStatus(lbMobSub.MsSeq,
                                    OUTPUT lcYOICardLogMsg). 
                                   
            fApplyYOICardDiscount(lbMobSub.MsSeq,
                                  {&YOIGO_USE_DISCOUNT},
                                  TODAY,  
                                  lbOrder.OrderId, 
                                  OUTPUT lcDiscLogMsg).  
         END.
         WHEN 3 THEN 
            fApplyYOICardDiscount(lbMobSub.MsSeq,
                                  {&YOIGO_USE_DISCOUNT},
                                  TODAY,  
                                  lbOrder.OrderId, 
                                  OUTPUT lcDiscLogMsg).  
         WHEN 4 THEN DO: 
            fUpdateCreditCardStatus(lbMobSub.MsSeq,
                                    OUTPUT lcYOICardLogMsg). 

            fApplyYOICardDiscount(lbMobSub.MsSeq,
                                  {&YOIGO_ACTIVATION_DISCOUNT},
                                  TODAY,  
                                  lbOrder.OrderId, 
                                  OUTPUT lcDiscLogMsg).  
         END.  
         OTHERWISE .
      END CASE.

      fLogLine(lcYOICardLogMsg + lcSep + lcDiscLogMsg).

      IF lcDiscLogMsg > ""           AND 
         fIsMobileNumber(lbMobSub.CLI) THEN DO: 
         IF Mm.MManMessage:mGetMessage("SMS", "YOICardUpdateBOB", 1) EQ TRUE THEN DO:
            Mm.MManMessage:mCreateMMLogSMS(lbMobSub.CLI, FALSE).
            Mm.MManMessage:mClearData().
         END.
      END. 

   END. /* REPEAT TRANSACTION */ 

END PROCEDURE.

