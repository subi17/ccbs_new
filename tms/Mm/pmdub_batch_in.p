/* ----------------------------------------------------------------------
  MODULE .......: pmdub_batch_in.p 
  TASK .........: Prepaid Bono 8 renewal incoming result batch file
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.03.11
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{commpaa.i}
katun = "Cron".
gcBrand = "1".
{cparam2.i}
{date.i}
{ftaxdata.i}
{xmlfunction.i}
{ftransdir.i}
{tmsconst.i}
{fmakemsreq.i}

DEF VAR lcResultFile AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcOutProcDir AS CHAR NO-UNDO. 
DEF VAR lcIncomingDir AS CHAR NO-UNDO. 
DEF VAR lcInputFile AS CHAR NO-UNDO. 
DEF VAR lcLogFile AS CHAR NO-UNDO. 
DEF VAR lcSep AS CHAR NO-UNDO INIT ",".
DEF VAR liErrorCode AS INT NO-UNDO. 
DEF VAR liTmsErr AS INT NO-UNDO. 

DEF VAR liTotal AS INT NO-UNDO. 
DEF VAR lcLine AS CHAR NO-UNDO. 
DEF VAR lcCLI AS CHAR NO-UNDO. 
DEF VAR lcDoneDir AS CHAR NO-UNDO. 
DEF VAR ldThisRun AS DEC NO-UNDO. 

DEF VAR lcBatchStatus AS CHAR NO-UNDO. 
DEF VAR lcBatchErrorCode AS CHAR NO-UNDO. 
DEF VAR liProcessedRecords AS INT NO-UNDO. 

ASSIGN
   lcOutProcDir = fCParam("PrepaidBundle","OutProcDir")
   lcIncomingDir = fCParam("PrepaidBundle","InDir")
   lcDoneDir = fCParam("PrepaidBundle","InProcDir")
   ldThisRun = fMakeTS().

IF NOT lcOutProcDir > "" OR
   NOT lcIncomingDir > "" OR
   NOT lcDoneDir > "" THEN RETURN.

DEF STREAM sFile.
DEF STREAM sInputFile.
INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcIncomingDir + "/*.RPT").

DEF STREAM sLine.
DEF STREAM sLog.

FUNCTION fPos RETURNS CHARACTER
  (INPUT piPos AS INTEGER):

   RETURN ENTRY(piPos,lcLine,lcSep).

END.

DEFINE TEMP-TABLE ttBatchInputFile
   FIELD cli AS CHAR
   FIELD charge AS INT
INDEX cli IS PRIMARY UNIQUE cli. 

DEFINE TEMP-TABLE ttBatchOutputFile
   FIELD cli AS CHAR
   FIELD errorcode AS INT
INDEX cli IS PRIMARY UNIQUE cli. 
            
FIND ServiceLimit WHERE
     ServiceLimit.GroupCode = {&PMDUB} NO-LOCK NO-ERROR.
IF NOT AVAIL ServiceLimit THEN RETURN.

FILE_LOOP:
REPEAT:
   
   IMPORT STREAM sFile UNFORMATTED lcResultFile.
   
   lcFileName = ENTRY(NUM-ENTRIES(lcResultFile,"/"), lcResultFile, "/").
   
   IF SEARCH(lcResultFile) NE ? THEN DO:
      
      INPUT STREAM sLine FROM VALUE(lcResultFile).
      
      ASSIGN
         lcBatchStatus     = ""
         lcBatchErrorCode  = ""
         liProcessedRecords = 0 
         lcLogFile  = lcResultFile + ".LOG".
      
      EMPTY TEMP-TABLE ttBatchInputFile.
      EMPTY TEMP-TABLE ttBatchOutputFile.
    
      lcInputFile = lcOutProcDir + REPLACE(lcFileName,".RPT",".DAT").
      IF SEARCH(lcInputFile) EQ ? THEN NEXT FILE_LOOP.

      INPUT STREAM sInputFile FROM VALUE(lcInputFile).
      IMPORT STREAM sInputFile UNFORMATTED lcLine. /* skip header line */
      REPEAT:
         IMPORT STREAM sInputFile UNFORMATTED lcLine.
         IF lcLine EQ "" THEN NEXT.
         CREATE ttBatchInputFile.
         ASSIGN
            ttBatchInputFile.Cli = fPos(1)
            ttBatchInputFile.Charge = INT(fPos(2)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN NEXT FILE_LOOP.
      END.
      INPUT STREAM sInputFile CLOSE.

      RUN pMarkStarted.
      IF RETURN-VALUE EQ "NOK" THEN NEXT FILE_LOOP.
      
      OUTPUT STREAM sLog TO VALUE(lcLogFile).
            
      LINE_LOOP:
      REPEAT:
   
         IMPORT STREAM sLine UNFORMATTED lcLine.
         IF lcLine EQ "" THEN NEXT.

         /* check if trailer line */
         IF NUM-ENTRIES(lcLine,lcSep) < 5 THEN DO: 
            ASSIGN
               liProcessedRecords = INT(TRIM(fPos(1))) /* processed_records */
               lcBatchStatus = fPos(2)   /* batch_status */
               lcBatchErrorCode = fPos(3)/* batch_error_code */
            NO-ERROR.  
            
            IF ERROR-STATUS:ERROR THEN
               PUT STREAM sLog UNFORMATTED
                  lcLine "|" "Incorrect trailer format" SKIP.
            
            LEAVE LINE_LOOP.
         END.
         
         ASSIGN
            liErrorCode = int(TRIM(fPos(1))) /* error_code */
            lcCLI       = TRIM(fPos(2)) /* rejected_input_record */
         /* lcTransID   = TRIM(fPos(3)) */ /* transaction_id */
            liTotal  = liTotal + 1 NO-ERROR.

         IF ERROR-STATUS:ERROR THEN DO:
            PUT STREAM sLog UNFORMATTED
               lcLine "|" "Incorrect format" SKIP.
            liTMSErr = liTMSErr + 1.
            NEXT LINE_LOOP.
         END.

         FIND FIRST ttBatchOutputFile WHERE
                    ttBatchOutputFile.CLI = lcCLI NO-LOCK NO-ERROR.
         IF AVAIL ttBatchOutputFile THEN DO:
            PUT STREAM sLog UNFORMATTED
               lcLine "|" "Duplicate MSISDN" SKIP.
            liTMSErr = liTMSErr + 1.
            NEXT LINE_LOOP.
         END.

         CREATE ttBatchOutputFile.
         ASSIGN
            ttBatchOutputFile.cli = lcCLI
            ttBatchOutputFile.Errorcode = liErrorCode.

      END. /* LINE_LOOP */
            
      IF lcBatchStatus EQ "SUCCESS" THEN DO:

         FOR EACH ttBatchOutputFile NO-LOCK:
   
            /* OK to adjust balance */
            RUN pHandleRow(ttBatchOutputFile.CLI,
                           ttBatchOutputFile.ErrorCode).

            IF RETURN-VALUE BEGINS "ERROR" THEN DO:
            
               PUT STREAM sLog UNFORMATTED
                  ttBatchOutputFile.CLI "|"
                  RETURN-VALUE SKIP.
               liTMSErr = liTMSErr + 1.
            END.
         END.

         FOR EACH ttBatchInputFile NO-LOCK:
            IF NOT CAN-FIND(FIRST ttBatchOutputFile WHERE
                            ttBatchOutputFile.CLI = ttBatchInputFile.CLI)
            THEN DO:
               /* Create prepaidrequest records for successful cases */
               RUN pCreatePPRequest(ttBatchInputFile.CLI,
                                    ttBatchInputFile.Charge).
               IF RETURN-VALUE BEGINS "ERROR" THEN DO:

                  PUT STREAM sLog UNFORMATTED
                     ttBatchInputFile.CLI "|"
                     RETURN-VALUE SKIP.
                  liTMSErr = liTMSErr + 1.
               END.
            END.
         END.
      END.

      OUTPUT STREAM sLog CLOSE.

      fTransDir(lcLogFile,
               "",
               lcDoneDir).

      fTransDir(lcResultFile,
               "",
               lcDoneDir). 

      RUN pMarkFinished.
      
   END.

END.


PROCEDURE pHandleRow:
   
   DEF INPUT PARAM pcCLI AS CHAR NO-UNDO. 
   DEF INPUT PARAM piErrorCode AS INT NO-UNDO.

   DEF VAR ldeNow AS DEC NO-UNDO. 
   DEF VAR liRequest AS INT NO-UNDO. 
   DEF VAR lcResult AS CHAR NO-UNDO. 

   DEF BUFFER MobSub FOR MobSub.
   DEF BUFFER MServiceLimit FOR MServiceLimit.

   /* do not process possible batch level errors */
   IF piErrorCode < 20 THEN RETURN "OK".
   
   FIND FIRST MobSub WHERE
              MobSub.Brand = gcBrand AND
              MobSub.CLI = pcCLI NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN "ERROR:Subscription not found".
   
   ldeNow = fMakeTS().
   FIND FIRST MServiceLimit WHERE
              MserviceLimit.MsSeq = MobSub.MsSeq AND
              MServiceLimit.DialType = ServiceLimit.DialType AND
              MServiceLimit.SlSeq = ServiceLimit.SlSeq AND
              MServiceLimit.EndTS >= ldeNow AND
              MServiceLimit.FromTS <= ldeNow NO-LOCK NO-ERROR.
   IF NOT AVAIL MServiceLimit THEN RETURN "ERROR:Contract not found".
   
   liRequest = fPCActionRequest(MobSub.MsSeq,
                                ServiceLimit.GroupCode,
                                "term",
                                ldeNow,
                                TRUE,    /* fees */
                                {&REQUEST_SOURCE_SCRIPT},
                                "",   /* creator */
                                0,    /* no father request */
                                FALSE,
                                "PMDUBDeActBatch",
                                0,
                                0,
                                OUTPUT lcResult). 

   IF liRequest = 0 THEN RETURN 
      SUBST("ERROR:Contract termination request failed: &1", lcResult).

   RETURN "OK".
END.
   
PROCEDURE pCreatePPRequest:

   DEF INPUT PARAM icCLI AS CHAR NO-UNDO. 
   DEF INPUt PARAM iiCharge AS INT NO-UNDO. 

   DEF VAR liRequest AS INT NO-UNDO. 
   DEF VAR lcTaxZone AS CHAR NO-UNDO. 

   FIND FIRST MsOwner WHERE
              MsOwner.CLI = icCLI NO-LOCK USE-INDEX CLI_s NO-ERROR.
   IF NOT AVAIL MsOwner THEN RETURN "ERROR: MsOwner not found".

   FIND FIRST Customer WHERE
              Customer.Custnum = MsOwner.Custnum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN "ERROR: Customer not found".

   lcTaxZone  = fRegionTaxZone(Customer.Region).
      
   DO WHILE TRUE:
      liRequest = NEXT-VALUE(PrePaidReq).
   
      IF NOT CAN-FIND(FIRST PrePaidRequest WHERE
                            PrePaidRequest.Brand     = gcBrand AND
                            PrepaidRequest.PPRequest = liRequest)
      THEN LEAVE.
   END. /* DO WHILE TRUE: */
   
 
   CREATE PrePaidRequest.
   ASSIGN
      PrePaidRequest.TSRequest   = fMakeTS()
      PrePaidRequest.UserCode    = katun
      PrePaidRequest.Brand       = gcBrand
      PrePaidRequest.MsSeq       = MsOwner.MsSeq
      PrePaidRequest.CLI         = MsOwner.CLI
      PrePaidRequest.PPRequest   = liRequest
      PrePaidRequest.PPReqPrefix = "989" 
      PrePaidRequest.Request     = "AdjustmentTRequest"
      PrePaidRequest.CommLine    = "AdjustmentTRequest"
      PrePaidRequest.Source      = "CHARGE"
      PrePaidRequest.TopUpAmt    = iiCharge
      PrePaidRequest.VatAmt      = 0
      PrePaidRequest.TaxZone     = lcTaxZone
      PrePaidRequest.Response    = "" 
      PrePaidRequest.RespCode    = 0
      PrePaidRequest.TSResponse  = fMakeTS()
      PrePaidRequest.PPStatus    = 2.

   /* payment for adjustment */
   CREATE TopUpQueue.
   ASSIGN
      TopUpQueue.PPRequest = PrePaidRequest.PPRequest
      TopUpQueue.CLI       = PrePaidRequest.CLI
      TopUpQueue.TopUpAmt  = PrePaidRequest.TopUpAmt / 100
      TopUpQueue.VatAmt    = PrePaidRequest.VatAmt / 100
      TopUpQueue.Date      = TODAY
      TopUpQueue.Source    = PrePaidRequest.Source.

   RETURN "OK".

END PROCEDURE. 

PROCEDURE pMarkStarted:
   
   /* check that there isn't already another run for the same purpose */
   IF CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
                     ActionLog.Brand        = gcBrand     AND    
                     ActionLog.TableName    = "Cron"     AND    
                     ActionLog.KeyValue     = lcFileName AND
                     ActionLog.ActionID     = "PMDUB_IN" AND
                     ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE})
   THEN DO:
      
      DO TRANS:
         CREATE ActionLog.
         
         ASSIGN
            ActionLog.Brand        = gcBrand
            ActionLog.ActionID     = "PMDUB_IN"
            ActionLog.ActionTS     = ldThisRun
            ActionLog.TableName    = "Cron"
            ActionLog.KeyValue     = lcFileName
            ActionLog.UserCode     = katun
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY) 
            ActionLog.ActionChar   = "Batch not started due to ongoing run".
         RELEASE ActionLog.   
      END.
      RETURN "NOK". 
   END.

   /* mark this run started */
   DO TRANS:
      CREATE ActionLog.
      
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.ActionID     = "PMDUB_IN"
         ActionLog.ActionTS     = ldThisRun
         ActionLog.TableName    = "Cron"
         ActionLog.KeyValue     = lcFileName
         ActionLog.UserCode     = katun
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).
      RELEASE ActionLog.   
   END.
   RETURN "OK".

END PROCEDURE.
   
PROCEDURE pMarkFinished:

   /* mark this run finished */
   FOR FIRST ActionLog USE-INDEX ActionID WHERE
             ActionLog.Brand        = gcBrand AND    
             ActionLog.ActionID     = "PMDUB_IN" AND
             ActionLog.ActionTS     = ldThisRun AND
             ActionLog.TableName    = "Cron" AND
             ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE} AND
             ActionLog.KeyValue     = lcFileName 
   EXCLUSIVE-LOCK:
      ASSIGN 
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.ActionChar   = "Batch status: " + lcBatchStatus + 
                            ",Batch error code: " + lcBatchErrorCode + CHR(10) +
            SUBST("Processed: &1, Charge errors: &2, TMS errors: &3", 
                  liProcessedRecords, liTotal, liTMSErr).
   END.
   
END PROCEDURE.
