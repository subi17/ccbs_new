/* ----------------------------------------------------------------------
  MODULE .......: terminal_financing_move.p
  TASK .........: Move terminal financing monthly fees which are not yet
                  sent to bank.
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 13.6.2013
  Version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commpaa.i}
ASSIGN 
   Syst.Var:gcBrand = "1"
   Syst.Var:katun   = "Cron".
{Func/cparam2.i}
{Syst/funcrunprocess_run.i}
{Syst/funcrunprocess_update.i}
{Syst/tmsconst.i}
{Func/coinv.i}
{Func/ftransdir.i}

DEF VAR ldaToDate AS DATE NO-UNDO. 
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR liTotalSubsCount AS INT  NO-UNDO.
DEF VAR llPickedCust  AS LOG  NO-UNDO.

DEF STREAM sout.
DEF STREAM sout2.

DEF VAR ldaBillPeriod AS DATE NO-UNDO. 
DEF VAR liBillPeriod AS INT NO-UNDO. 
DEF VAR ldaNewBillPeriod AS DATE NO-UNDO. 
DEF VAR liFFItemCount AS INT NO-UNDO. 
DEF VAR ldeFFItemAmount AS DEC NO-UNDO. 
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
DEF VAR ldaFromDate AS DATE NO-UNDO. 

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
   ldaToDate = Func.Common:mLastDayOfMonth(ldaBillPeriod) 
   liBillPeriod = YEAR(ldaBillPeriod) * 100 + MONTH(ldaBillPeriod)
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
   "ORDER_ID|MSSEQ|CUSTNUM|FF_ID|FINANCED_RESULT|LO_STATUS|<MOVE/UPDATE/ERROR>:[DETAILS]" skip.

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
      FixedFee.FFNum "|"
      FixedFee.FinancedResult "|"
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
         Order.Brand = Syst.Var:gcBrand AND
         Order.OrderId = FixedFee.OrderID:

   IF FixedFee.BegDate > ldaToDate THEN NEXT.
   IF FixedFee.SourceTable NE "DCCLI" THEN NEXT.

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
             DayCampaign.Brand = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = FixedFee.CalcObj,
       FIRST FeeModel NO-LOCK WHERE
             FeeModel.Brand = Syst.Var:gcBrand AND
             FeeModel.FeeModel = DayCampaign.FeeModel,
       FIRST FMItem NO-LOCK WHERE
             FMItem.Brand = Syst.Var:gcBrand AND
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
         (FixedFee.BillCode BEGINS "PAYTERM" AND
         INT(ldeFFItemAmount) NE INT(FMItem.FFItemQty * FMItem.Amount)) THEN DO:
         
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
      
   FIND FIRST DCCLI EXCLUSIVE-LOCK WHERE
              DCCLI.Brand   = Syst.Var:gcBrand AND
              DCCLI.MsSeq   = INT(FixedFee.KeyValue) AND
              DCCLI.DCEvent = FixedFee.CalcObj AND
              DCCLI.percontractId = int(FixedFee.SourceKey) NO-ERROR.
   IF NOT AVAIL DCCLI THEN DO:
      fLogLine("ERROR:DCCLI not found"). 
      RELEASE DCCLI.
      NEXT ORDER_LOOP.
   END.
   
   IF FixedFee.BillCode BEGINS "PAYTERM" THEN
      FIND FIRST SingleFee EXCLUSIVE-LOCK WHERE
                 SingleFee.Brand = Syst.Var:gcBrand AND
                 SingleFee.Custnum = FixedFee.Custnum AND
                 SingleFee.HostTable = FixedFee.HostTable AND
                 SingleFee.KeyValue = Fixedfee.KeyValue AND
                 SingleFee.SourceKey = FixedFee.SourceKey AND
                 SingleFee.SourceTable = FixedFee.SourceTable AND
                 SingleFee.CalcObj = "RVTERM" AND
                 SingleFee.Billed = FALSE NO-ERROR.
   ELSE RELEASE SingleFee.

   FIND bFixedFee EXCLUSIVE-LOCK WHERE
        ROWID(bFixedFee) = ROWID(FixedFee).

   FFITEM_LOOP:
   DO WHILE TRUE:
   
      /* move monthly fee to the end if not yet sent to bank */
      FIND FIRST FFItem EXCLUSIVE-LOCK WHERE
                 FFItem.FFNum = fixedfee.ffnum USE-INDEX FFNum NO-ERROR.

      IF NOT AVAIL FFItem THEN DO:
         fLogLine("ERROR:FFItem not found"). 
         RELEASE FFItem.
         NEXT ORDER_LOOP.
      END.

      IF FFItem.billperiod > liBillPeriod THEN LEAVE FFITEM_LOOP.

      /* check that fee is not yet billed */
      IF FFItem.Billed THEN DO:
         fLogLine("ERROR:Already billed"). 
         RELEASE FFItem.
         NEXT ORDER_LOOP.
      END.
      
      FIND LAST bffitem NO-LOCK USE-INDEX FFNum WHERE
                bffitem.ffnum = fixedfee.ffnum NO-ERROR.

      FIND FIRST OrderDelivery NO-LOCK USE-INDEX OrderID WHERE
                 OrderDelivery.Brand = Order.Brand AND
                 OrderDelivery.OrderID = Order.OrderID NO-ERROR.
      
      put stream sout unformatted 
         Order.OrderID "|"
         Order.MsSeq "|"
         Order.Custnum "|"
         FixedFee.FFNUM "|"
         FixedFee.FinancedResult "|" 
         (IF AVAIL OrderDelivery 
          THEN STRING(OrderDelivery.LOStatusID) ELSE "")
         "|MOVE:"
         FFItem.FFItemNum ";"
         DCCLI.PerContractId ";"
         "|BEFORE_VALUES;"
         FFItem.BillPeriod ";"
         ffitem.concerns[1] ";"
         ffitem.concerns[2] ";"
         FixedFee.EndPeriod ";"
         FixedFee.BegDate ";"
         FixedFee.BegPeriod ";"
         DCCLI.ValidFrom ";"
         DCCLI.ValidTo ";"
         DCCLI.ContractDate ";"
         (IF AVAIL SingleFee THEN SingleFee.billperiod ELSE ?) ";"
         (IF AVAIL SingleFee THEN SingleFee.concerns[1] ELSE ?) ";".

      IF lcRunMode EQ "Production" THEN DO:
      
         ASSIGN
         ldaFromDate = fPer2Date(FFItem.BillPeriod,1)
         ldaNewBillPeriod = fPer2Date(bffitem.billperiod,1)
         ldaNewFFEndPeriod = fPer2Date(bFixedFee.EndPeriod,1)
         bFixedFee.EndPeriod = YEAR(ldaNewFFEndPeriod) * 100 +
                               MONTH(ldaNewFFEndPeriod)
         ffitem.billperiod  = YEAR(ldaNewBillPeriod) * 100 + 
                              MONTH(ldaNewBillPeriod)
         ffitem.concerns[1] = YEAR(ldaNewBillPeriod) * 10000 + 
                              MONTH(ldaNewBillPeriod) * 100 + 
                              DAY(ldaNewBillPeriod)
         ldaNewBillPeriod = Func.Common:mLastDayOfMonth(ldaNewBillPeriod) 
         ffitem.concerns[2] = YEAR(ldaNewBillPeriod) * 10000 + 
                              MONTH(ldaNewBillPeriod) * 100 + 
                              DAY(ldaNewBillPeriod)
         bFixedFee.BegDate = ldaFromDate
         bFixedFee.BegPeriod = YEAR(ldaFromDate) * 100 + MONTH(ldaFromDate)
         DCCLI.ValidFrom = ldaFromDate
         DCCLI.ValidTo = ldaNewBillPeriod
         ldaNewRFPeriod = fPer2Date(SingleFee.billperiod,1) WHEN AVAIL SingleFee
         SingleFee.billperiod  = YEAR(ldaNewRFPeriod) * 100 + 
                              MONTH(ldaNewRFPeriod) WHEN AVAIL SingleFee
         SingleFee.concerns[1] = YEAR(ldaNewRFPeriod) * 10000 + 
                              MONTH(ldaNewRFPeriod) * 100 + 
                              DAY(ldaNewRFPeriod) WHEN AVAIL SingleFee.
      END.
         
      put stream sout unformatted "|AFTER_VALUES:"
         FFItem.BillPeriod ";"
         ffitem.concerns[1] ";"
         ffitem.concerns[2] ";"
         FixedFee.EndPeriod ";"
         FixedFee.BegDate ";"
         FixedFee.BegPeriod ";"
         DCCLI.ValidFrom ";"
         DCCLI.ValidTo ";"
         DCCLI.ContractDate ";"
         (IF AVAIL SingleFee THEN SingleFee.billperiod ELSE ?) ";"
         (IF AVAIL SingleFee THEN SingleFee.concerns[1] ELSE ?) skip.
     
      liMoved = liMoved + 1.
      
      IF liUpdateInterval > 0 AND liMoved MOD liUpdateInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(liFRProcessID,liMoved) THEN
            RETURN "ERROR:Stopped".
      END.

      IF lcRunMode NE "Production" THEN LEAVE.
   END.   
      
   PUT STREAM sout2 UNFORMATTED
      Order.CLI ";"
      Order.MsSeq ";"
      Order.OrderID ";"
      FixedFee.CalcObj ";"
      STRING(TODAY,"99-99-9999") SKIP.
      
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
OUTPUT STREAM sout2 CLOSE.
fMove2TransDir(lcLogFile2, "", lcLogOutDir). 

RUN pFinalizeFuncRunProcess(liFRProcessID,liMoved).

QUIT.
