/* ----------------------------------------------------------------------
  MODULE .......: terminal_financing_read.p
  TASK .........: Read terminal financing file from the bank.
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 12.6.2013
  Version ......: yoigo
---------------------------------------------------------------------- */
DISABLE TRIGGERS FOR LOAD OF FixedFee.
{commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{cparam2.i}
{timestamp.i}
{tmsconst.i}
{tsformat.i}
{ftransdir.i}
{eventlog.i}
{coinv.i}
{host.i}
      
&GLOBAL-DEFINE TF_PAYTERM_CODES "0018,0024,0034,0125,0127,0129,0131,0225,0227,0229,0231"
&GLOBAL-DEFINE TF_PAYTERM_WITH_RESIDUAL "0125,0127,0129,0131,0225,0227,0229,0231"
&GLOBAL-DEFINE TF_RESIDUAL_CODES "0126,0128,0130,0132,0226,0228,0230,0232,N/A"

DEF VAR lcProcessedFile AS CHAR NO-UNDO.
DEF VAR lcIncDir AS CHAR NO-UNDO. 
DEF VAR lcIncProcDir AS CHAR NO-UNDO. 
DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcInputFile AS CHAR NO-UNDO. 
DEF VAR liNumOk AS INT NO-UNDO. 
DEF VAR liNumErr AS INT NO-UNDO. 
DEF VAR liNumWarning AS INT NO-UNDO. 
DEF VAR lcLogFileName AS CHAR NO-UNDO. 
DEF VAR lcCommissionLog AS CHAR NO-UNDO. 
DEF VAR lcErrorLog AS CHAR NO-UNDO. 
DEF VAR lcLogDir AS CHAR NO-UNDO. 
DEF VAR lcLogIntDir AS CHAR NO-UNDO. 
DEF VAR lcLogSpoolDir AS CHAR NO-UNDO. 
DEF VAR lcLogOutDir AS CHAR NO-UNDO. 
DEF VAR ldaFromDate AS DATE NO-UNDO. 
DEF VAR ldaToDate AS DATE NO-UNDO. 
DEF VAR liDumped AS INT NO-UNDO. 
DEF VAR lcSummary AS CHAR NO-UNDO. 
DEF VAR lcTFBank AS CHAR NO-UNDO. 
DEF VAR lcBankName AS CHARACTER NO-UNDO. 

DEF BUFFER bActionLog FOR ActionLog.
      
DEF BUFFER bResidualFee FOR SingleFee.
DEF BUFFER bFixedFee    FOR FixedFee.

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.
DEF STREAM sCommission.

lcRootDir = fCParam("TermFinance","InRootDir").
IF NOT lcRootDir > "" THEN RETURN.
   
lcLogDir = fCParam("TermFinance","LogDir").
IF NOT lcLogDir > "" THEN RETURN.

FOR EACH TFConf NO-LOCK:
   IF LOOKUP(TFConf.PaytermCode,{&TF_PAYTERM_CODES}) = 0 THEN RETURN.
   IF LOOKUP(TFConf.ResidualCode,{&TF_RESIDUAL_CODES}) = 0 THEN RETURN.
END.

DEFINE TEMP-TABLE ttTFPayment
   FIELD lineNum AS INT
   FIELD content AS char
   FIELD LineId AS INT 
   FIELD OrderId AS INT 
   FIELD PaytermAmt AS DEC
   FIELD PaymentCode AS CHAR
   FIELD FinancedResult AS CHAR
   FIELD ResidualAmt AS DEC
   FIELD ResidualCode AS CHAR
   FIELD ResidualResult AS CHAR
INDEX linenum IS PRIMARY linenum 
INDEX lineid lineid. 

ASSIGN
   lcLogIntDir = lcRootDir + "logs/"
   lcLogSpoolDir = lcLogDir + "spool/"
   lcLogOutDir = lcLogDir + "outgoing/"
   lcIncDir = lcRootDir + "incoming/"
   lcIncProcDir = lcRootDir + "processed/".

FUNCTION fWriteLog RETURNS LOGIC
   (icLine AS CHAR,
    icMessage AS CHAR):
   
   IF icMessage BEGINS "ERROR" THEN liNumErr = liNumErr + 1.
   ELSE IF icMessage BEGINS "WARNING" THEN liNumWarning = liNumWarning + 1.

   PUT STREAM sLog UNFORMATTED
      icLine ";"
      icMessage SKIP.
END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN 
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.

   ASSIGN
      liNumErr = 0
      liNumOK = 0
      liNumWarning = 0
      ldaFromDate = ?
      ldaToDate = ?
      lcTFBank = "".

   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile). 

   lcBankName = (IF INDEX(lcFileName,"SABADELL") > 0
                 THEN "SABADELL" ELSE "UNOE").

   ASSIGN
      lcLogFileName = lcFileName + "_" + 
         ftsformat("yyyymmdd_HHMMss", fMakeTS()) + ".log"

   lcErrorLog = lcLogSpoolDir + "BANK_RESPONSE_LOG_" +
         lcBankName + "_" +
         STRING(YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)) + 
         "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

   lcCommissionLog = lcLogSpoolDir + "COMMISSION_CREATION_LOG_" + 
         lcBankName + "_" +
         STRING(YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)) + 
         "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".
      
   OUTPUT STREAM sLog TO VALUE(lcErrorLog) append.
   OUTPUT STREAM sCommission TO VALUE(lcCommissionLog).

   PUT STREAM sCommission UNFORMATTED
      "MSISDN;SUBSCRIPTION_ID;ORDER_ID;PAYTERM_TYPE;COMMISSION_FEE_AMOUNT;BANK_RESPONSE;CREATION_DATE" SKIP.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.

   EMPTY TEMP-TABLE ttTFPayment.
   
   RUN pReadFileData.
   INPUT STREAM sin CLOSE.
   RUN pProcessData.
   
   lcSummary = SUBST("input: &1, updated: &2, warnings: &3, errors: &4",
                (liNumOK + liNumErr),
                liNumOK,
                liNumWarning,
                liNumErr).
   
   PUT STREAM sLog UNFORMATTED lcSummary SKIP.
   OUTPUT STREAM sLog CLOSE.
   OUTPUT STREAM sCommission CLOSE.

   fMove2TransDir(lcCommissionLog, "", lcLogOutDir). 
   fMove2TransDir(lcErrorLog, "", lcLogOutDir). 
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcIncProcDir). 
   IF SESSION:BATCH AND lcProcessedFile NE "" THEN
      fBatchLog("FINISH", lcProcessedFile).

   IF ldaFromDate NE ? THEN DO:

      /* Fill a possible gap */
      FOR EACH ActionLog NO-LOCK WHERE
               ActionLog.Brand = gcBrand AND
               ActionLog.ActionId = ("TF_READ_" + lcTFBank) AND
               ActionLog.ACtionStatus NE {&ACTIONLOG_STATUS_CANCELLED} AND
               ActionLog.ToDate < ldaFromDate 
            BY ActionLog.ToDate DESC:
         IF ActionLog.Todate < ldaFromDate - 1 THEN
            ldaFromDate = ActionLog.Todate + 1.
         LEAVE.
      END.
      
      DO TRANS:
         
         CREATE ActionLog.
         
         ASSIGN
            ActionLog.Brand        = gcBrand
            ActionLog.ActionID     = "TF_READ_" + lcTFBank
            ActionLog.ActionTS     = fMakeTS()
            ActionLog.TableName    = "Cron"
            ActionLog.KeyValue     = lcLogFileName
            ActionLog.UserCode     = katun
            ActionLog.FromDate     = ldaFromDate 
            ActionLog.Todate       = ldaToDate
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
            ActionLog.ActionChar   = lcSummary.

         RELEASE ActionLog.   
      END.

      FIND DumpFile NO-LOCK WHERE
           DumpFile.Brand = gcBrand AND
           DumpFile.DumpName = "IFSInstallmentAction" NO-ERROR.
   
      IF AVAIL DumpFile THEN
         RUN dumpfile_run.p(dumpfile.dumpid,
                          "modified",
                          lcTFBank,
                          FALSE,
                          OUTPUT liDumped).
   END.
END.

INPUT STREAM sFile CLOSE.

PROCEDURE pReadFileData:

   DEF VAR lcLine AS CHAR NO-UNDO. 
   DEF VAR liOrderId AS INTEGER NO-UNDO.
   DEF VAR ldeTotalAmount AS DEC NO-UNDO. 
   DEF VAR lcPaymentCode AS CHAR NO-UNDO. 
   DEF VAR lcFinancedResult AS CHAR NO-UNDO.
   DEF VAR liLineNum AS INT NO-UNDO. 
   DEF VAR liLineid AS INT NO-UNDO. 
   
   FILE_LINE:
   REPEAT TRANS:
                  
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.
      liLineNum = liLineNum + 1.

      IF NOT SESSION:BATCH AND liLineNum MOD 10 = 0 THEN DO:
         disp "Reading data.. " lcFilename liLineNum with frame a.
         pause 0.
      END.

      IF LENGTH(lcLine) NE 650 THEN DO:
         fWriteLog(lcLine,"ERROR:Incorrect line length").
         NEXT FILE_LINE.
      END.

      ASSIGN 
         liLineID = int(substring(lcLine,9,6))
         liOrderId = int(substring(lcLine,335,10))
         ldeTotalAmount = int(substring(lcLine,508,13))
         lcPaymentCode = substring(lcLine,523,4)
         lcFinancedResult = substring(lcLine,649,2)
         ldeTotalAmount = ldeTotalAmount / 100.0
         NO-ERROR.
 
      IF ERROR-STATUS:ERROR THEN DO:
         fWriteLog(lcLine,"ERROR:Incorrect line syntax").
         NEXT FILE_LINE.
      END.
      
      IF LENGTH(TRIM(lcFinancedResult)) NE 2 THEN DO:
         fWriteLog(lcLine,"ERROR:Incorrect line syntax").
         NEXT FILE_LINE.
      END.
      
      /* normal row */
      IF LOOKUP(lcPaymentCode,{&TF_PAYTERM_CODES}) > 0 THEN DO:

         CREATE ttTFPayment.
         ASSIGN
            ttTFPayment.LineNum = liLineNum
            ttTFPayment.Content = lcLine
            ttTFPayment.LineId = liLineId
            ttTFPayment.OrderId = liOrderId
            ttTFPayment.PaytermAmt = ldeTotalAmount
            ttTFPayment.PaymentCode = lcPaymentCode
            ttTFPayment.FinancedResult = lcFinancedResult.
      END.
      /* residual fee row */
      ELSE IF LOOKUP(lcPaymentCode,{&TF_RESIDUAL_CODES}) > 0 THEN DO:

         FIND ttTFPayment WHERE
              ttTFPayment.LineId = liLineId AND
              ttTFPayment.OrderId = liOrderId NO-ERROR.

         IF NOT AVAIL ttTFPayment THEN DO:
            fWriteLog(lcLine,"ERROR:Residual fee row without PAYTERM fee row").
            NEXT FILE_LINE.
         END.

         ASSIGN
            ttTFPayment.ResidualAmt    = ldeTotalAmount
            ttTFPayment.ResidualCode   = lcPaymentCode
            ttTFPayment.ResidualResult = lcFinancedResult.
      END. 
      ELSE DO:
         fWriteLog(lcLine,SUBST("ERROR:Unsupported payment method &1", 
                                lcPaymentCode)).
         NEXT FILE_LINE.
      END.
   END.
  
END PROCEDURE. 
   
PROCEDURE pProcessData:

   DEF VAR ldeRVPerc AS DEC NO-UNDO. 
   DEF VAR ldaOrderdate  AS DATE NO-UNDO. 
   DEF VAR ldaDate AS DATE NO-UNDO. 
   DEF VAR ldaFeePeriod AS DATE NO-UNDO. 
   DEF VAR lcResult AS CHAR NO-UNDO. 
   DEF VAR ldeCommission AS DEC NO-UNDO. 
   DEF VAR liFeePeriod AS INT NO-UNDO. 
   DEF VAR lcCommissionBillCode AS CHAR NO-UNDO. 

   LINE_LOOP:
   FOR EACH ttTFPayment:
      
      IF NOT SESSION:BATCH AND ttTFPayment.LineNum MOD 10 = 0 THEN DO:
         disp "Processing data.. " lcFilename ttTFPayment.LineNum with frame a.
         pause 0.
      END.

      FIND Order NO-LOCK WHERE
           Order.Brand = gcBrand AND
           Order.OrderId = ttTFPayment.OrderId NO-ERROR.
      IF NOT AVAIL Order THEN DO:
         fWriteLog(ttTFPayment.content,"ERROR:Order not found").
         NEXT LINE_LOOP.
      END.

      FIND FixedFee NO-LOCK WHERE
           FixedFee.Brand = gcBrand AND
           FixedFee.Custnum = Order.Custnum AND
           FixedFee.HostTable = "MobSub" AND
           FixedFee.KeyValue = STRING(Order.MsSeq) AND
           FixedFee.BillCode = "PAYTERM" AND
           FixedFee.OrderId = Order.OrderID AND 
           FixedFee.FinancedResult = {&TF_STATUS_SENT_TO_BANK} NO-ERROR.

      IF NOT AVAIL FixedFee THEN
      FIND FixedFee NO-LOCK WHERE
           FixedFee.Brand = gcBrand AND
           FixedFee.Custnum = Order.Custnum AND
           FixedFee.HostTable = "MobSub" AND
           FixedFee.KeyValue = STRING(Order.MsSeq) AND
           FixedFee.BillCode = "PAYTERM" AND
           FixedFee.OrderId = Order.OrderID NO-ERROR.

      
      IF NOT AVAIL FixedFee THEN DO:
         fWriteLog(ttTFPayment.content,"ERROR:PAYTERM FixedFee not found").
         NEXT LINE_LOOP.
      END.
      
      FIND FIRST FixedFeeTF EXCLUSIVE-LOCK WHERE
                 FixedFeeTF.FFNum = FixedFee.FFNum NO-ERROR.
      IF AVAIL FixedFeeTF THEN ASSIGN
         FixedFeeTF.BankResult   = ttTFPayment.FinancedResult
         FixedFeeTF.BankRespDate = TODAY.

      IF FixedFee.FinancedResult NE {&TF_STATUS_SENT_TO_BANK} AND
         FixedFee.FinancedResult NE "" THEN DO:
         fWriteLog(ttTFPayment.content,
                   SUBST("ERROR:Incorrect financial status &1", 
                         FixedFee.FinancedResult)).
         NEXT LINE_LOOP.
      END.

      /* Check residual fee */
      IF LOOKUP(ttTFPayment.PaymentCode,{&TF_PAYTERM_WITH_RESIDUAL}) > 0
      THEN DO:

         IF ttTFPayment.ResidualCode EQ "" THEN DO:
            fWriteLog(ttTFPayment.content,
                      "ERROR:PAYTERM fee row without residual fee row").
            NEXT LINE_LOOP.
         END.
               
         FIND bResidualFee EXCLUSIVE-LOCK WHERE
              bResidualFee.Brand = gcBrand AND
              bResidualFee.Custnum = FixedFee.Custnum AND
              bResidualFee.HostTable = FixedFee.HostTable AND
              bResidualFee.KeyValue = Fixedfee.KeyValue AND
              bResidualFee.SourceKey = FixedFee.SourceKey AND
              bResidualFee.SourceTable = FixedFee.SourceTable AND
              bResidualFee.CalcObj = "RVTERM" NO-ERROR.
         IF NOT AVAIL bResidualFee THEN DO:
            FIND FIRST bFixedFee WHERE
                       ROWID(bFixedFee) = ROWID(FixedFee)
                 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL bFixedFee THEN DO:
               bFixedFee.FinancedResult = {&TF_STATUS_YOIGO_RVTERM_FEE_DELETION}.
               RELEASE bFixedFee.
            END.
            fWriteLog(ttTFPayment.content,
                      "ERROR:Residual SingleFee not found").
            NEXT LINE_LOOP.
         END.
      
         IF (ttTFPayment.FinancedResult EQ {&TF_STATUS_BANK} AND 
             ttTFPayment.ResidualResult NE {&TF_STATUS_BANK}) OR
            (ttTFPayment.FinancedResult NE {&TF_STATUS_BANK} AND
             ttTFPayment.ResidualResult EQ {&TF_STATUS_BANK}) THEN DO:

            fWriteLog(ttTFPayment.content,
                  SUBST("WARNING:PAYTERM bank response code &1 does not match" +
                  " with residual fee bank response code &2",
                  ttTFPayment.FinancedResult,
                  ttTFPayment.ResidualResult)).

            IF ttTFPayment.FinancedResult NE {&TF_STATUS_BANK} THEN ASSIGN
               ttTFPayment.FinancedResult = {&TF_STATUS_BANK}
               FixedFeeTF.BankResult = {&TF_STATUS_BANK}
                  WHEN AVAIL FixedFeeTF.
            ELSE IF ttTFPayment.ResidualResult NE {&TF_STATUS_BANK} THEN
               ttTFPayment.ResidualResult = {&TF_STATUS_BANK}.
         END.
        
      END.
      ELSE RELEASE bResidualFee.
      
      IF ttTFPayment.FinancedResult EQ {&TF_STATUS_BANK} THEN DO:
      
         IF LOOKUP(FixedFee.TFBank,{&TF_BANK_CODES}) = 0 THEN DO:
            fWriteLog(ttTFPayment.content,
                      SUBST("ERROR:Incorrect Fixed fee bank code &1", 
                            FixedFee.FinancedResult)).
            NEXT LINE_LOOP.
         END.
         
         ldeRVPerc = TRUNC(ttTFPayment.ResidualAmt / 
                          (ttTFPayment.PaytermAmt +
                           ttTFPayment.ResidualAmt) * 100 + 0.05,1).

         fTS2Date(Order.CrStamp, OUTPUT ldaOrderdate).

         FIND FIRST TFConf NO-LOCK WHERE
                    TFConf.RVPercentage = ldeRVPerc AND
                    TFConf.ValidTo >= ldaOrderDate AND
                    TFConf.ValidFrom <= ldaOrderDate NO-ERROR.

         IF NOT AVAIL TFConf THEN DO:
            fWriteLog(ttTFPayment.content,
                      "ERROR:Terminal financing configuration not found").
            NEXT LINE_LOOP. 
         END.

         ldeCommission = ROUND((TFConf.CommFeePerc / 100) * 
                        (ttTFPayment.PaytermAmt +  ttTFPayment.ResidualAmt),2).

         /* fee might have been moved to bext month, so check the correct
            period from the first FFItem */
         FIND FIRST FFItem NO-LOCK USE-INDEX FFNum WHERE
                    FFItem.FFNum = FixedFee.FFNum NO-ERROR.

         IF AVAIL FFItem THEN ASSIGN
            liFeePeriod = FFItem.BillPeriod
            ldaFeePeriod = fInt2Date(FFItem.Concerns[1],1).
         ELSE liFeePeriod = 0.

         lcCommissionBillCode = 
              (IF FixedFee.TFBank EQ {&TF_BANK_UNOE} THEN "PAYTERMCG1E"
               ELSE "PAYTERMCGBS").
            
         /* should not be possible but better to check */
         FIND FIRST SingleFee NO-LOCK WHERE
                    SingleFee.Brand = gcBrand AND
                    SingleFee.Custnum = FixedFee.Custnum AND
                    SingleFee.HostTable = "MobSub" AND
                    SingleFee.KeyValue = FixedFee.KeyValue AND
                    SingleFee.BillCode =  lcCommissionBillCode AND
                    SingleFee.SourceTable = "FixedFee" AND
                    SingleFee.SourceKey = STRING(FixedFee.FFNum) NO-ERROR.

         IF AVAIL SingleFee THEN 
            fWriteLog(ttTFPayment.content,
                      "WARNING:Commission fee already exists").
   
         IF liFeePeriod > 0 AND NOT AVAIL SingleFee THEN DO:
         
            RUN creasfee.p (
              fixedfee.CustNum,
              Order.MsSeq,
              ldaFeePeriod,
              "FeeModel",
              lcCommissionBillCode,
              9,
              ldeCommission,
              "Created " + STRING(TODAY,"99.99.9999") + 
                 "¤" + fixedfee.CalcObj,  /* memo */
              FALSE,   /* no messages to screen */
              "Cron",
              "BankResponseFile",
              0,
              "FixedFee",
              STRING(FixedFee.FFNum),
              OUTPUT lcResult).

            IF lcResult BEGINS "ERROR" OR 
               lcResult BEGINS "0 fees created" THEN DO:
               fWriteLog(ttTFPayment.content,
                  SUBST("ERROR:Commission fee creation failed: &1", lcResult)).
               NEXT LINE_LOOP.
            END.
            ELSE DO:
               PUT STREAM sCommission UNFORMATTED
                  Order.CLI ";"
                  Order.MsSeq ";"
                  Order.OrderID ";" 
                  FixedFee.CalcObj ";"
                  ldeCommission ";"
                  ttTFPayment.FinancedResult ";"
                  STRING(TODAY,"99-99-9999") SKIP.

               IF AVAIL bResidualFee THEN DO:
                  IF FixedFee.TFBank EQ {&TF_BANK_UNOE} THEN ASSIGN
                     bResidualFee.BillCode = "RVTERM1EF".
                  ELSE IF FixedFee.TFBank EQ {&TF_BANK_SABADELL} THEN
                     bResidualFee.BillCode = "RVTERMBSF".
                  RELEASE bResidualFee.
               END.
            END.

         END. 
      END.
         
      FIND CURRENT FixedFee EXCLUSIVE-LOCK.

      ASSIGN
         liNumOK = liNumOK + 1
         FixedFee.FinancedResult = ttTFPayment.FinancedResult
         lcTFBank = FixedFee.TFBank WHEN lcTFBank EQ ""
         /* do not send termination to bank if the fee is rejected
            by the bank */
         FixedFeeTF.CancelStatus = "" WHEN AVAIL FixedFeeTF AND 
                                      FixedFeeTF.CancelStatus EQ "NEW" AND
                                      ttTFPayment.FinancedResult NE "00".

      RELEASE FixedFee.
      
      IF INDEX(Order.OrderChannel,"pos") > 0 THEN
      FOR FIRST OrderTimeStamp NO-LOCK WHERE
                OrderTimeStamp.Brand = gcBrand AND
                OrderTimeStamp.OrderId = Order.OrderId AND
                OrderTimeStamp.RowType = {&ORDERTIMESTAMP_DELIVERY}:
         
         fTs2Date(OrderTimeStamp.TimeStamp, OUTPUT ldaDate).

         IF ldaFromDate EQ ? THEN ASSIGN
            ldaFromDate = ldaDate
            ldaToDate  = ldaDate.
         ELSE ASSIGN
            ldaFromDate = ldaDate WHEN ldaDate < ldaFromDate
            ldaToDate = ldaDate WHEN ldaDate > ldaToDate.
      END.
      
   END.

END PROCEDURE. 
   
