/* ----------------------------------------------------------------------
  MODULE .......: tf_cancel_send.p
  TASK .........: Creates terminal financing early termination or cancellation 
                  file for the bank.
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 11.11.2014
                  11.12.2015 Modified program work as a part of automated process
  Version ......: yoigo
---------------------------------------------------------------------- */
{commali.i}
{cparam2.i}
{date.i}
{tmsconst.i}
{eventlog.i}
{ftransdir.i}
{msreqfunc.i}

DEF INPUT PARAMETER iiMSrequest AS INT  NO-UNDO.

FIND MSRequest WHERE 
     MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR 
   MsRequest.ReqType NE {&REQTYPE_TERMINAL_FINANCE_CAN_TER_BANK_FILE} THEN
   RETURN "ERROR".

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

DEF STREAM sout.
DEF STREAM slog.

DEF VAR lcLogFile AS CHAR NO-UNDO. 
DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR lcLogDir AS CHAR NO-UNDO. 
DEF VAR lcProcessedFile AS CHAR NO-UNDO. 
DEF VAR liErrors AS INT NO-UNDO. 
DEF VAR liOk AS INT NO-UNDO. 
DEF VAR lcCodFpago AS CHAR NO-UNDO.

ASSIGN
   lcRootDir = fCParam("TermFinance","CanOutRoot")
   lcLogDir = fCParam("TermFinance","LogDir").

IF lcRootDir EQ ? OR
   NOT lcRootDir > "" THEN DO:
   fReqStatus(3,"ERROR:Root directory not defined").
   RETURN.
END.

IF lcLogDir EQ ? OR
   NOT lcLogDir > "" THEN DO:
   fReqStatus(3,"ERROR:Log root directory not defined").
   RETURN.
END.
   
FUNCTION fLogLine RETURNS LOGICAL
   (iiOrderId AS INT,
    icNote AS CHAR):

   PUT STREAM slog UNFORMATTED 
      FixedFeeTF.FFNum "|"
      iiOrderId "|" icNote SKIP.

END.
IF MsRequest.ReqCParam1 = {&TF_BANK_UNOE} THEN DO:
   RUN pCreateFile({&TF_BANK_UNOE},"CANCEL","ANULACIONESYOIGO").
   RUN pCreateFile({&TF_BANK_UNOE},"TERMINATION","CANCELACIONESYOIGO").
END.
ELSE IF MsRequest.ReqCParam1 = {&TF_BANK_SABADELL} THEN DO:
   RUN pCreateFile({&TF_BANK_SABADELL},"CANCEL","ANULACIONESYOIGOSABADELL").
   RUN pCreateFile({&TF_BANK_SABADELL},"TERMINATION","CANCELACIONESYOIGOSABADELL").
END.
ELSE DO:
   fReqStatus(3,"ERROR: Unsupported bank code").
   RETURN.
END.   

PROCEDURE pCreateFile:

   DEF INPUT PARAM icBank AS CHAR NO-UNDO. 
   DEF INPUT PARAM icFileType AS CHAR NO-UNDO.
   DEF INPUT PARAM icFileName AS CHAR NO-UNDO.

   DEF VAR lcFile AS CHAR NO-UNDO. 
   DEF VAR lcSummary AS CHAR NO-UNDO. 
   DEF VAR ldaBankDate AS DATE NO-UNDO. 
   DEF VAR liErrors AS INT NO-UNDO. 
   DEF VAR liOk AS INT NO-UNDO. 
   
   ASSIGN
      lcFile = lcRootDir + "spool/" + icFileName + "_" +
               STRING(YEAR(TODAY),"9999") +
               STRING(MONTH(TODAY),"99") + ".txt".
      lcLogFile = lcLogDir + "internal/" + icFileName + "_" +
               STRING(DAY(TODAY),"99") +
               STRING(MONTH(TODAY),"99") +
               STRING(YEAR(TODAY)) + ".LOG".

   IF SESSION:BATCH THEN fBatchLog("START", lcFile).
   OUTPUT STREAM sout to VALUE(lcFile) APPEND.
   OUTPUT STREAM slog to VALUE(lcLogFile) APPEND.
   PUT STREAM slog UNFORMATTED
   "FIXED_FEE_ID|ORDER_ID|MEMO" skip.

   FOR EACH FixedFeeTF EXCLUSIVE-LOCK WHERE
            FixedFeeTF.CancelStatus = "NEW" AND
            FixedFeeTF.TFBank = icBank:
      
      FIND FIRST FixedFee NO-LOCK WHERE
                 FixedFee.FFNum  = FixedFeeTF.FFNum NO-ERROR.

      IF NOT AVAIL FixedFee THEN DO:
         ASSIGN
            FixedFeeTF.CancelStatus = "ERROR_REQU"
            FixedFeeTF.CancelMemo = "ERROR:FixedFee not found"
            liErrors = liErrors + 1.
         fLogLine(0,FixedfeeTF.CancelMemo).
         NEXT.
      END.
      
      IF FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN DO:
         fLogLine(FIxedFee.OrderId, "SKIPPED:The installment is still waiting the financial response from the bank").
         NEXT.
      END.

      IF LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) = 0 THEN DO:
         ASSIGN
            FixedFeeTF.CancelStatus = "ERROR_REQU"
            FixedFeeTF.CancelMemo = SUBST("ERROR:Incorrect fixedfee status &1",
                            FixedFee.FinancedResult)
            liErrors = liErrors + 1.
         fLogLine(FixedFee.OrderId,FixedFeeTF.CancelMemo).
         NEXT.
      END.

      IF FixedFeeTF.BankDate NE ? THEN 
         ldaBankDate = FixedFeeTF.BankDate.
      ELSE DO:

         FIND SingleFee NO-LOCK WHERE
              SingleFee.Brand = "1" AND
              SingleFee.Custnum = FixedFee.Custnum AND
              SingleFee.HostTable = FixedFee.HostTable AND
              SingleFee.KeyValue = Fixedfee.KeyValue AND
              SingleFee.Sourcetable = "FixedFee" AND
              SingleFee.Sourcekey = string(FixedFee.ffnum) AND
              SingleFee.CalcObj = fixedfee.calcobj and
              SingleFee.billcode begins "PAYTERMCG" NO-ERROR.

         IF AVAIL SingleFee AND
                  SingleFee.Memo[1] BEGINS "Created " then do:
            ldaBankDate = date(entry(2, SingleFee.Memo[1], " ")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN ldaBankDate = ?.
         end.
         
         IF ldaBankDate EQ ? THEN DO:

            FIND FIRST Order NO-LOCK WHERE
                       Order.Brand = gcBrand AND
                       Order.OrderId = FixedFee.OrderId NO-ERROR.

            IF AVAIL Order AND FixedFee.OrderId > 0 THEN DO:
               fTS2Date(Order.CrStamp, OUTPUT ldaBankDate).
               IF INDEX(Order.OrderChannel, "POS") = 0 THEN
                        ldaBankDate = ldaBankDate + 16.
               IF DAY(ldaBankDate) >= 25 THEN 
                  ldaBankDate = ADD-INTERVAL(ldaBankDate, 1, "months").
            END.  
            ELSE DO:
               ASSIGN
                  FixedFeeTF.CancelStatus = "ERROR_REQU"
                  FixedFeeTF.CancelMemo = "ERROR:Bank date check failed"
                  liErrors = liErrors + 1.
               fLogLine(FixedFee.OrderId, FixedFeeTF.CancelMemo).
               NEXT.
            END.
         END.

      END.
      
      IF icFileType EQ "CANCEL" THEN DO:
         IF TODAY - ldaBankDate > 30 THEN NEXT.
      END.
      ELSE IF icFileType EQ "TERMINATION" THEN DO:
         IF TODAY - ldaBankDate <= 30 THEN NEXT.
      END.
      ELSE NEXT.

     liOk = liOk + 1.
      IF NOT SESSION:BATCH AND
         liOk MOD 10 = 0 THEN DO:
            DISP liOk WITH FRAME a.
            PAUSE 0.
      END.

      RUN pPrintLine(FixedFeeTF.Amount, 
                     ldaBankDate,
                     icBank).
      IF FixedFeeTF.ResidualAmount > 0 THEN
         RUN pPrintLine(FixedFeeTF.ResidualAmount,
                        ldaBankDate,
                        icBank).
      ASSIGN
         FixedFeeTF.CancelStatus = "SENT"
         FixedFeeTF.CancelDate = TODAY
         FixedFeeTF.CancelFile = icFileName
         FixedFeeTF.BankDate = ldaBankDate WHEN FixedFeeTF.BankDate EQ ?
         FixedFeeTF.OrderId = Fixedfee.OrderId WHEN FixedFeeTF.OrderId EQ ?.
   END.
   
   lcSummary = SUBST("total: &1, sent &2, errors: &3",
               (liOK + liErrors),
                liOK,
                liErrors).
   PUT STREAM sLog UNFORMATTED lcSummary SKIP.
   OUTPUT STREAM sLog CLOSE.
      
   OUTPUT STREAM sout close.

   fMove2TransDir(lcLogFile, "", lcLogDir + "outgoing/"). 
   lcProcessedFile = fMove2TransDir(lcFile, "", lcRootDir + "outgoing/"). 
   IF SESSION:BATCH AND 
      lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).

   fReqStatus(2,"").
   
   DO TRANS:
      
      CREATE ActionLog.
      
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.ActionID     = "TF_" + icFileType
         ActionLog.ActionTS     = fMakeTS()
         ActionLog.TableName    = "Cron"
         ActionLog.KeyValue     = icBank
         ActionLog.UserCode     = katun
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
         ActionLog.ActionChar   = lcSummary.

      RELEASE ActionLog.   
   END.

END PROCEDURE. 

PROCEDURE pPrintLine:
   
   DEF INPUT PARAM ideTotalAmount AS DEC NO-UNDO. 
   DEF INPUT PARAM idaBankDate AS DATE NO-UNDO.
   DEF INPUT PARAM icBank AS CHAR NO-UNDO. 

   DEF VAR lcTotalAmount AS CHAR NO-UNDO. 
   DEF VAR ldeRVPerc AS DEC NO-UNDO.
   DEF VAR ldeRVAmt AS DEC NO-UNDO.
   DEF VAR ldaOrderDate AS DATE NO-UNDO.
   DEF VAR liOrderId AS IN NO-UNDO.

   lcTotalAmount = REPLACE(REPLACE(TRIM(STRING(ideTotalAmount,"->>>>>>>9.99")),",",""),".","").
   IF FixedFeeTF.OrderId EQ ? THEN
      liOrderId = FixedFee.OrderId.
   ELSE
      liOrderId = FixedFeeTF.OrderId.
   IF FixedFee.BillCode EQ "RVTERM" THEN
      lcCodFpago = "0212".
   ELSE DO:
      FIND FIRST Order WHERE Order.brand = gcBrand AND
                             Order.OrderId = liOrderId NO-ERROR.
      IF AVAIL Order THEN DO:
         fTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).
         IF ldaOrderDate >= 5/1/2015 THEN
             lcCodFpago = "0034".
         ELSE
            lcCodFpago = "0024".
         IF FixedFee.BillCode BEGINS "PAYTERM" AND
            FixedFeeTF.ResidualAmount > 0 THEN DO:
            ASSIGN
               ldeRVPerc = TRUNC(FixedFeeTF.residualAmount /
                           (FixedFeeTF.amount + 
                            FixedFeeTF.residualAmount) * 100 + 0.05,1)
               ldeRVAmt = FixedFeeTF.residualAmount.         
         END.
         FIND FIRST TFConf NO-LOCK WHERE
                    TFConf.RVPercentage = ldeRVPerc AND
                    TFConf.ValidTo >= ldaOrderDate AND
                    TFConf.ValidFrom <= ldaOrderDate NO-ERROR.

         IF AVAIL TFConf THEN DO:

         ASSIGN
            lcCodFpago = TFConf.PaytermCode WHEN TFConf.RVPercentage NE 0.
         END.
      END.
   END.

   PUT STREAM sout 
   /*COD-CDNITR*/    UPPER(FixedFeeTF.OrgId) FORMAT "X(9)"
   /*COD-PRODUCTO*/  "6674" FORMAT "X(4)"
   /*IMP-OPERAC*/    FILL("0",11 - LENGTH(lcTotalAmount)) +
                     lcTotalAmount FORMAT "X(11)"
   /*MES-OPERAC*/    STRING(MONTH(idaBankDate),"99") FORMAT "X(2)"
   /*ANO-OPERAC*/    STRING(YEAR(idaBankDate),"9999") FORMAT "X(4)"
   /*NUM-PEDIDO*/    STRING(FixedFee.OrderId) FORMAT "X(8)"
   /*COD-FPAGO*/     lcCodFpago FORMAT "X(4)".
   
   PUT STREAM sout CONTROL CHR(13) CHR(10).
END.
