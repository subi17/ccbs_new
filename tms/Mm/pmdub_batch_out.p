/* ----------------------------------------------------------------------
  MODULE .......: pmdub_batch_out.p 
  TASK .........: Prepaid Bono 8 renewal outgoing batch file
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.03.11
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{commpaa.i}
katun = "Cron".
gcBrand = "1".
{date.i}
{ftransdir.i}
{cparam2.i}
{bundle_first_month_fee.i}
{tmsconst.i}
{matrix.i}

DEF VAR lcRespFile AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir AS CHAR NO-UNDO. 
DEF VAR lcSep AS CHAR NO-UNDO INIT ",".
DEF VAR liOk AS INT NO-UNDO. 
DEF VAR liNok AS INT NO-UNDO. 
DEF VAR liTotal AS INT NO-UNDO. 
DEF VAR lcLine AS CHAR NO-UNDO. 
DEF VAR ldeCharge AS DEC NO-UNDO. 
DEF VAR ldeMonthlyFee AS DEC NO-UNDO. 
DEF VAR lcOutDir AS CHAR NO-UNDO. 
DEF VAR i AS INT NO-UNDO. 
DEF VAR liTransactions AS INT NO-UNDO.  
DEF VAR liTotalAmount AS INT NO-UNDO. 
DEF VAR ldeNow AS DEC NO-UNDO. 
DEF VAR ldeFirstMonthBegin AS DEC NO-UNDO. 
DEF VAR ldeFirstMonthEnd AS DEC NO-UNDO. 
DEF VAR liFirstMonthPeriod AS INT NO-UNDO.
DEF VAR ldaFirstMonth AS DATE NO-UNDO. 
DEF VAR ldaCurrMonth AS DATE NO-UNDO. 
DEF VAR ldeFat AS DEC NO-UNDO. 
DEF VAR ldeTodayBegin AS DEC NO-UNDO. 
DEF VAR ldeTodayEnd AS DEC NO-UNDO. 
DEF VAR ldeFirstDayStart AS DEC NO-UNDO. 
DEF VAR ldeFirstDayEnd AS DEC NO-UNDO. 
DEF VAR lcAllowedTypes AS CHAR NO-UNDO. 
DEF VAR lcreqchar AS CHAR NO-UNDO. 
DEF VAR liOngoingSTC AS INT NO-UNDO. 
DEF VAR liOngoingTerm AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE ttCharge NO-UNDO
   FIELD CLI AS CHARACTER
   FIELD Charge AS INTEGER.

DEF STREAM sFile.

ASSIGN
   lcOutDir   = fCParam("PrepaidBundle","OutDir")
   lcSpoolDir = fCParam("PrepaidBundle","OutSpoolDir")
   ldeMonthlyFee  = fCParamDe("PMDUBFee")
   ldeNow = fMakeTS()
   ldaCurrMonth = DATE(MONTH(TODAY), 1, YEAR(TODAY))
   ldaFirstMonth = ldaCurrMonth - 1
   ldaFirstMonth = DATE(MONTH(ldaFirstMonth), 1, YEAR(ldaFirstMonth))
   ldeFirstMonthBegin = fMake2Dt(ldaFirstMonth,0) 
   ldeFirstMonthEnd = fMake2Dt(fLastDayOfMonth(ldaFirstMonth),86399)
   liFirstMonthPeriod = YEAR(ldaFirstMonth) * 100 + MONTH(ldaFirstMonth)
   ldeTodayBegin = fmake2Dt(TODAY,0)
   ldeTodayEnd = fmake2Dt(TODAY,86399)
   ldeFirstDayStart = fmake2Dt(ldaCurrMonth,0)
   ldeFirstDayEnd = fmake2Dt(ldaCurrMonth,86399)
   .

IF NOT lcOutDir > "" OR
   NOT lcSpoolDir > "" OR
   ldeMonthlyFee <= 0 THEN RETURN.

for each clitype where
         clitype.brand = gcBrand no-lock:
   if fmatrixanalyse(gcBrand,
                     "percontr",
                     "percontract;substypeto",
                     "pmdub;" + clitype.clitype,
                     output lcreqchar) eq 1 then do:
      lcAllowedTypes = lcAllowedTypes + "," + clitype.clitype.
   end.
end.

lcAllowedTypes = substring(lcAllowedTypes,2).
         
i = 1.
FOR EACH ActionLog WHERE
         ActionLog.Brand      = gcBrand AND
         ActionLog.ActionID   = "PMDUB_OUT" AND
         ActionLog.ActionTS  >= ldeTodayBegin AND
         ActionLog.ActionTS  <= ldeTodayEnd NO-LOCK:
   i = i + 1.
END.

lcFileName = "BATCH_ADJUST_" + STRING(YEAR(TODAY) * 10000 +
                                      MONTH(TODAY) * 100 + 
                                      DAY(TODAY)) + "_" +
              FILL("0",5 - length(string(i))) + STRING(i) + ".DAT".

ASSIGN
   lcRespFile = lcSpoolDir + lcFileName.

RUN pMarkStarted.
IF RETURN-VALUE EQ "NOK" THEN RETURN.

/* go through all active PMDUB contracts */
FOR FIRST ServiceLimit NO-LOCK WHERE
          ServiceLimit.GroupCode = "PMDUB",
    EACH MServiceLimit NO-LOCK WHERE
         MServiceLimit.SlSeq = ServiceLimit.SlSeq AND
         MServiceLimit.DialType = ServiceLimit.DialType AND
         MServiceLimit.EndTS >= ldeNow AND
         MServiceLimit.FromTS < ldeFirstDayStart USE-INDEX SLSeq:

   FIND FIRST MobSub WHERE
              MobSub.MsSeq = MServiceLimit.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub OR MobSub.PayType EQ False THEN NEXT.

   FIND FIRST MsRequest WHERE
              MsRequest.MsSeq = MobSub.MsSeq AND
              MsRequest.ReqType = 0 AND
              MsRequest.ActStamp >= ldeFirstDayStart AND
              MsRequest.ActStamp <= ldeFirstDayEnd AND
       LOOKUP(STRING(MsRequest.ReqStatus),"4,9") = 0
   USE-INDEX MsSeq NO-LOCK NO-ERROR.
   IF AVAIL MsRequest AND
      LOOKUP(MsRequest.ReqCParam2,lcAllowedTypes) = 0 THEN DO:
      liOngoingSTC = liOngoingSTC + 1.
      NEXT.
   END.
   
   FIND FIRST MsRequest WHERE
              MsRequest.MsSeq = MobSub.MsSeq AND
              MsRequest.ReqType = 9 AND
              MsRequest.ActStamp = ldeFirstMonthEnd  AND
              MsRequest.ReqCParam3 = ServiceLimit.GroupCode AND
       LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0
   USE-INDEX MsSeq NO-LOCK NO-ERROR.
   IF AVAIL MsRequest THEN DO:
      liOngoingTerm = liOngoingTerm + 1.
      NEXT.
   END.

   ldeCharge = ldeMonthlyFee.

   /* 1st full month? */ 
   IF MServiceLimit.FromTS <= ldeFirstMonthEnd AND
      MServiceLimit.FromTs >= ldeFirstMonthBegin THEN DO:
      
      ldeFat = fCalculatePrepaidFirstMonthFAT(
                     MobSub.CLI, 
                     MobSub.MsSeq, 
                     ldeCharge,  /* original amount (full month) */
                     liFirstMonthPeriod). /* YYYYMM */
      ldeCharge = ldeCharge - ldeFat.
   END.

   ldeCharge = INT(100 * ldeCharge).

   IF ldeCharge > 0 THEN DO:
      CREATE ttCharge.
      ASSIGN
         ttCharge.CLI = MobSub.CLI
         ttCharge.Charge = -1 * ldeCharge.
   END.

END.
   
FOR EACH ttCharge NO-LOCK:
   liTransactions = liTransactions + 1.
   liTotalAmount = liTotalAmount + ttCharge.Charge. 
END.

IF liTransactions > 0 THEN DO:

   OUTPUT STREAM sFile TO VALUE(lcRespFile).

   /* header row */
   PUT STREAM sFile UNFORMATTED
      liTransactions    lcSep /* total_transactions */
      liTotalAmount     lcSep /* total_amount */
      "EUR"             lcSep /* currency */
      200               lcSep /* version */
      1                       /* parameter_set */
      SKIP.

   /* data rows */
   FOR EACH ttCharge NO-LOCK:
      PUT STREAM sFile UNFORMATTED 
         ttCharge.CLI     lcSep /* subscriber_id */
         ttCharge.Charge  lcSep /* adjustment_amount */
         "TransCode"      lcSep
         "TransType"      lcSep
         SKIP.
   END.

   OUTPUT STREAM sFile CLOSE.

   fTransDir(lcRespFile,
            "",
            lcOutDir).

END.

RUN pMarkFinished.

PROCEDURE pMarkStarted:
   
   /* check that there isn't already another run for the same purpose */
   IF CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
                     ActionLog.Brand        = gcBrand     AND    
                     ActionLog.TableName    = "Cron" AND
                     ActionLog.ActionID     = "PMDUB_OUT" AND
                     ActionLog.KeyValue     = lcFileName AND
                     ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE} )
   THEN DO:
      
      DO TRANS:
         CREATE ActionLog.
         
         ASSIGN
            ActionLog.Brand        = gcBrand
            ActionLog.ActionID     = "PMDUB_OUT"
            ActionLog.ActionTS     = ldeNow
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
         ActionLog.ActionID     = "PMDUB_OUT"
         ActionLog.ActionTS     = ldeNow
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
             ActionLog.ActionID     = "PMDUB_OUT" AND
             ActionLog.ActionTS     = ldeNow AND
             ActionLog.TableName    = "Cron" AND
             ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE} AND
             ActionLog.KeyValue     = lcFileName 
   EXCLUSIVE-LOCK:
      ASSIGN 
        ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
        ActionLog.ActionChar   = SUBST("Charges: &1, Ongoing STC: &2, Ongoing term: &3",
                                 liTransactions, liOngoingSTC, liOngoingTerm).
   END.
   
END PROCEDURE.
