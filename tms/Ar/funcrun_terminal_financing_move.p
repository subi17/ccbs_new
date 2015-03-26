/* ----------------------------------------------------------------------
  MODULE .......: terminal_financing_move.p
  TASK .........: Move terminal financing monthly fees which are not yet
                  sent to bank.
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 13.6.2013
  Version ......: yoigo
---------------------------------------------------------------------- */
DISABLE TRIGGERS FOR LOAD OF FixedFee.

{commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
{cparam2.i}
{funcrunprocess_run.i}
{funcrunprocess_update.i}
{date.i}
{tmsconst.i}
{coinv.i}
{ftransdir.i}

DEF VAR ldaToDate AS DATE NO-UNDO. 
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR liTotalSubsCount AS INT  NO-UNDO.
DEF VAR llPickedCust  AS LOG  NO-UNDO.

DEF STREAM sout.
DEF STREAM sout2.

DEF VAR ldeTSTo AS DEC NO-UNDO. 
DEF VAR ldaBillPeriod AS DATE NO-UNDO. 
DEF VAR liBillPeriod AS INT NO-UNDO. 
DEF VAR ldaNewBillPeriod AS DATE NO-UNDO. 
DEF VAR liFFItemCount AS INT NO-UNDO. 
DEF VAR ldeFFItemAmount AS DEC NO-UNDO. 
DEF VAR ldaDate AS DATE NO-UNDO. 
DEF VAR lcLogDir AS CHAR NO-UNDO. 
DEF VAR lcLogOutDir AS CHAR NO-UNDO. 
DEF VAR lcLogSpoolDir AS CHAR NO-UNDO. 
DEF VAR lcLogIntDir AS CHAR NO-UNDO. 
DEF VAR lcLogFile AS CHAR NO-UNDO. 
DEF VAR lcLogFile2 AS CHAR NO-UNDO. 
DEF VAR liMoved AS INT NO-UNDO. 
DEF VAR liUpdated AS INT NO-UNDO. 
DEF VAR liErrors AS INT NO-UNDO. 
DEF VAR liLoop AS INTEGER NO-UNDO. 
DEF VAR lcTime AS CHAR NO-UNDO. 
DEF VAR ldaNewRFPeriod AS DATE NO-UNDO. 
DEF VAR ldaNewFFEndPeriod AS DATE NO-UNDO. 

def buffer bffitem for ffitem.

RUN pInitializeFuncRunProcess(OUTPUT liFRProcessID,
                              OUTPUT liFRExecID,    
                              OUTPUT lcRunMode,
                              OUTPUT liUpdateInterval).
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pGetFuncRunProcessParameters(liFRProcessID).

ASSIGN 
   ldaBillPeriod = fSetFuncRunDateParameter(1).

IF ldaBillPeriod = ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

ASSIGN
   ldaToDate = fLastDayOfMonth(ldaBillPeriod) 
   liBillPeriod = YEAR(ldaBillPeriod) * 100 + MONTH(ldaBillPeriod)
   ldeTsTo   = fMake2Dt(ldaToDate,86399)
   lcLogDir = fCParam("TermFinance","LogDir")
   lcLogIntDir = lcLogDir + "internal/".
   lcLogSpoolDir = lcLogDir + "spool/".
   lcLogOutDir = lcLogDir + "outgoing/".

IF NOT lcLogDir > "" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Output log file directory not defined").
   QUIT.
END.
  
ASSIGN
   lcTime = STRING(YEAR(TODAY) * 10000 +
            MONTH(TODAY) * 100  +
            DAY(TODAY)) + "_" + 
            REPLACE(STRING(TIME,"HH:MM:SS"),":","")
   lcLogFile = lcLogIntDir + "terminal_finance_move_" + 
               lcTime + ".txt"
   lcLogFile2 = lcLogSpoolDir + "MOVED_MONTHLY_FEE_" + 
                lcTime + ".txt".

OUTPUT STREAM sout to VALUE(lcLogFile).
OUTPUT STREAM sout2 to VALUE(lcLogFile2).

put stream sout unformatted
   "ORDER_ID|SUBSCR.ID|CUSTNUM|FF_ID|FINANCED_RESULT|NOTE:[FFItemNum;BillPeriod;concerns[1];concerns[2];NewBillPeriod]" skip.

put stream sout2 unformatted
  "MSISDN;SUBSCRIPTION_ID;ORDER_ID;PAYTERM_TYPE_MOVED;DATE" SKIP.

FUNCTION fLogLine RETURNS LOGICAL
   (icNote AS CHAR):

   IF icNote BEGINS "ERROR" THEN liErrors = liErrors + 1.
   ELSE IF icNote BEGINS "UPDATE" THEN liUpdated = liUpdated + 1.

   PUT STREAM sout UNFORMATTED
      Order.OrderID "|"
      Order.MsSeq "|"
      Order.Custnum "|"
      (IF AVAIL FixedFee THEN FixedFee.FFNum ELSE 0) "|"
      (IF AVAIL FixedFee THEN FixedFee.FinancedResult ELSE "") "|"
      icNote SKIP.
END.

/* Check all orders delivered during the billing period */

DEF BUFFER bFixedFee FOR FixedFee.

ORDER_LOOP:
FOR EACH FixedFee NO-LOCK WHERE
         FixedFee.FinancedResult = {&TF_STATUS_HOLD_SENDING} OR
         FixedFee.FinancedResult = {&TF_STATUS_WAITING_SENDING} OR
         FixedFee.FinancedResult = {&TF_STATUS_SENT_TO_BANK},
   FIRST Order NO-LOCK WHERE
         Order.Brand = gcBrand AND
         Order.OrderId = FixedFee.OrderID:

   IF FixedFee.BegDate > ldaToDate THEN NEXT.

   IF NOT SESSION:BATCH THEN DO:
      liLoop = liLoop + 1.
      if liLoop MOD 10 = 0 then do:
         disp liLoop liErrors liUpdated liMoved with frame a.
         pause 0.
      end.
   END.

   /* should not happen */
   IF FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN DO:
      fLogLine("UPDATE:" + {&TF_STATUS_YOIGO_NO_BANK_RESPONSE}).

      IF lcRunMode EQ "Production" THEN DO:
         FIND bFixedFee EXCLUSIVE-LOCK WHERE
              ROWID(bFixedFee) = ROWID(FixedFee).
         bFixedFee.FinancedResult = {&TF_STATUS_YOIGO_NO_BANK_RESPONSE}.
         RELEASE bFixedFee.
      END.

      NEXT ORDER_LOOP.
   END.
   
   IF FixedFee.EndPeriod <= (YEAR(TODAY) * 100 + MONTH(TODAY)) THEN DO:
      fLogLine("UPDATE:" + {&TF_STATUS_YOIGO_FF_TERMINATED}).

      IF lcRunMode EQ "Production" THEN DO:
         FIND bFixedFee EXCLUSIVE-LOCK WHERE
            ROWID(bFixedFee) = ROWID(FixedFee) NO-ERROR.
         bFixedFee.FinancedResult = {&TF_STATUS_YOIGO_FF_TERMINATED}.
         RELEASE bFixedFee.
      END.

      NEXT ORDER_LOOP.
   END.

   /* check if fixedfee item amount does not match with original confs. */
   FOR FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = gcBrand AND
             DayCampaign.DCEvent = FixedFee.CalcObj,
       FIRST FeeModel NO-LOCK WHERE
             FeeModel.Brand = gcBrand AND
             FeeModel.FeeModel = DayCampaign.FeeModel,
       FIRST FMItem NO-LOCK WHERE
             FMItem.Brand = gcBrand AND
             FMItem.FeeModel = FeeModel.FeeModel:
   
      ASSIGN
         liFFItemCount = 0
         ldeFFItemAmount = 0.

      FOR EACH FFItem OF FixedFee NO-LOCK:
         
         ASSIGN
            liFFItemCount = liFFItemCount + 1
            ldeFFItemAmount = ldeFFItemAmount + FFItem.Amt.
      END.

      IF liFFItemCount NE FMItem.FFItemQty OR
         INT(ldeFFItemAmount) NE INT(FMItem.FFItemQty * FMItem.Amount) THEN DO:
         
         fLogLine("UPDATE:" + {&TF_STATUS_YOIGO_FF_CHANGED}). 
      
         IF lcRunMode EQ "Production" THEN DO:
            FIND bFixedFee EXCLUSIVE-LOCK WHERE
                 ROWID(bFixedFee) = ROWID(FixedFee).
            bFixedFee.FinancedResult = {&TF_STATUS_YOIGO_FF_CHANGED}.
            RELEASE bFixedFee.
         END.

         NEXT ORDER_LOOP.
      END.
   END.

   /* move monthly fee to the end if not yet sent to bank */
   FIND FIRST FFItem NO-LOCK WHERE
              FFItem.FFNum = fixedfee.ffnum AND
              ffitem.billperiod = liBillPeriod NO-ERROR.

   IF NOT AVAIL FFItem THEN DO:
      fLogLine("ERROR:FFItem not found"). 
      NEXT ORDER_LOOP.
   END.

   /* check that fee is not yet billed */
   IF FFItem.Billed THEN DO:
      fLogLine("ERROR:Already billed"). 
      NEXT ORDER_LOOP.
   END.
   
   FIND FIRST SingleFee EXCLUSIVE-LOCK WHERE
              SingleFee.Brand = gcBrand AND
              SingleFee.Custnum = FixedFee.Custnum AND
              SingleFee.HostTable = FixedFee.HostTable AND
              SingleFee.KeyValue = Fixedfee.KeyValue AND
              SingleFee.SourceKey = FixedFee.SourceKey AND
              SingleFee.SourceTable = FixedFee.SourceTable AND
              SingleFee.CalcObj = "RVTERM" AND
              SingleFee.Billed = FALSE NO-ERROR.

   FIND FIRST DCCLI EXCLUSIVE-LOCK WHERE
              DCCLI.Brand   = gcBrand AND
              DCCLI.MsSeq   = INT(FixedFee.KeyValue) AND
              DCCLI.DCEvent = FixedFee.CalcObj AND
              DCCLI.percontractId = int(FixedFee.SourceKey) NO-ERROR.

   FIND LAST bffitem NO-LOCK WHERE
             bffitem.ffnum = fixedfee.ffnum NO-ERROR.

   
   ASSIGN
      ldaNewBillPeriod = fPer2Date(bffitem.billperiod,1).
      liMoved = liMoved + 1.
   
   put stream sout unformatted 
      Order.OrderID "|"
      FixedFee.FFNUM "|"
      FixedFee.FinancedResult "|MOVE:"
      FFItem.FFItemNum ";"
      FFItem.BillPeriod ";"
      ffitem.concerns[1] ";"
      ffitem.concerns[2] ";"
      FixedFee.EndPeriod ";"
      (IF AVAIL DCCLI THEN DCCLI.ValidTo ELSE ?) ";"
      (IF AVAIL SingleFee THEN SingleFee.billperiod ELSE ?) ";"
      (IF AVAIL SingleFee THEN SingleFee.concerns[1] ELSE ?) skip.

   IF lcRunMode EQ "Production" THEN DO:
      FIND CURRENT FFItem EXCLUSIVE-LOCK.
      FIND bFixedFee EXCLUSIVE-LOCK WHERE
           ROWID(bFixedFee) = ROWID(FixedFee).
      ASSIGN
      ldaNewBillPeriod = fPer2Date(bffitem.billperiod,1)
      ldaNewFFEndPeriod = fPer2Date(bFixedFee.EndPeriod,1)
      bFixedFee.EndPeriod = YEAR(ldaNewFFEndPeriod) * 100 +
                            MONTH(ldaNewFFEndPeriod)
      ffitem.billperiod  = YEAR(ldaNewBillPeriod) * 100 + 
                           MONTH(ldaNewBillPeriod)
      ffitem.concerns[1] = YEAR(ldaNewBillPeriod) * 10000 + 
                           MONTH(ldaNewBillPeriod) * 100 + 
                           DAY(ldaNewBillPeriod)
      ldaNewBillPeriod = fLastDayOfMonth(ldaNewBillPeriod) 
      ffitem.concerns[2] = YEAR(ldaNewBillPeriod) * 10000 + 
                           MONTH(ldaNewBillPeriod) * 100 + 
                           DAY(ldaNewBillPeriod)
      DCCLI.ValidTo = add-interval(DCCLI.ValidTo,1,"months") WHEN AVAIL DCCLI
      ldaNewRFPeriod = fPer2Date(SingleFee.billperiod,1) WHEN AVAIL SingleFee
      SingleFee.billperiod  = YEAR(ldaNewRFPeriod) * 100 + 
                           MONTH(ldaNewRFPeriod) WHEN AVAIL SingleFee
      SingleFee.concerns[1] = YEAR(ldaNewRFPeriod) * 10000 + 
                           MONTH(ldaNewRFPeriod) * 100 + 
                           DAY(ldaNewRFPeriod) WHEN AVAIL SingleFee.
   END.
  
   PUT STREAM sout2 UNFORMATTED
      Order.CLI ";"
      Order.MsSeq ";"
      Order.OrderID ";"
      FixedFee.CalcObj ";"
      STRING(TODAY,"99-99-9999") SKIP.
   
   IF liUpdateInterval > 0 AND liMoved MOD liUpdateInterval = 0 THEN DO:
      IF NOT fUpdateFuncRunProgress(liFRProcessID,liMoved) THEN
         RETURN "ERROR:Stopped".
   END.   
   
   RELEASE ffitem.
   RELEASE SingleFee.
   RELEASE DCCLI.
   RELEASE bFixedFee.
END.
   
PUT STREAM sout UNFORMATTED 
    "moved: " STRING(liMoved) ", "
    "updated: " STRING(liUpdated) ", "
    "errors: " STRING(liErrors) SKIP.
OUTPUT STREAM sout CLOSE.

OUTPUT STREAM sout CLOSE.
OUTPUT STREAM sout2 CLOSE.
fMove2TransDir(lcLogFile2, "", lcLogOutDir). 

RUN pFinalizeFuncRunProcess(liFRProcessID,liMoved).

QUIT.
