/* ----------------------------------------------------------------------
  MODULE .......: ifs_installment_operation.p
  TASK .........: Create a dump file for IFS from monthly installment
                  creations, cancellations and acc. YDR-328
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 26.08.11
  Version ......: yoigo
---------------------------------------------------------------------- */
{commali.i}
{dumpfile_run.i}
{tmsconst.i}
{date.i}
{cparam2.i}
{coinv.i}

DEF INPUT PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR liBatches   AS INT  NO-UNDO.
DEF VAR lcDelimiter AS CHAR NO-UNDO INIT ";".
DEF VAR ldaDataFrom AS DATE NO-UNDO. 
DEF VAR ldaDataTo   AS DATE NO-UNDO. 
DEF VAR lcError     AS CHAR NO-UNDO. 
DEF VAR lcTFBank    AS CHAR NO-UNDO. 
DEF VAR lcLogDir    AS CHAR NO-UNDO. 
DEF VAR lcLogFile   AS CHAR NO-UNDO. 
DEF VAR ldaDueDate    AS DATE NO-UNDO.

DEFINE STREAM sLog.
DEF STREAM sFixedFee.

DEF BUFFER bActionLog FOR ActionLog.

lcLogDir = fCParam("IFS","IFSHireLog").
IF lcLogDir > "" THEN DO:
   FILE-INFO:FILE-NAME = lcLogDir.
   IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.
   lcLogFile = lcLogDir + "/ifs_hire_" + 
      STRING(YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)) + "_" + 
      REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".
   OUTPUT STREAM sFixedFee TO VALUE(lcLogFile).
END.


FUNCTION fDispDecimal RETURNS CHAR
   (idAmount AS DEC):

   RETURN TRIM(REPLACE(STRING(idAmount,"->>>>>>>>>9.99"),",",".")).

END FUNCTION.

FUNCTION fDate2String RETURNS CHAR
   (idaDate AS DATE):

   IF idaDate = ? THEN RETURN "".

   RETURN STRING(YEAR(idaDate),"9999") +
          STRING(MONTH(idaDate),"99")  +
          STRING(DAY(idaDate),"99").

END FUNCTION.

FUNCTION fGetChannel RETURNS CHAR
   (BUFFER ibFixedFee FOR FixedFee,
    OUTPUT ocOrderType AS CHAR):

   DEF BUFFER Order FOR Order.
   DEF BUFFER bOrder FOR Order.
      
   IF ibFixedFee.OrderId > 0 THEN DO:
      FIND FIRST Order NO-LOCK WHERE
                 Order.Brand = gcBrand AND
                 Order.OrderId = ibFixedFee.OrderId NO-ERROR.
   END.
   ELSE DO:
      RELEASE Order.
      FOR EACH bOrder NO-LOCK WHERE
               bOrder.Msseq = INT(ibFixedFee.KeyValue) AND
               bOrder.OrderType <= 2 AND 
               bOrder.StatusCode = {&ORDER_STATUS_DELIVERED} 
               BY bOrder.CrStamp DESC:
         FIND Order NO-LOCK WHERE ROWID(Order) = ROWID(bOrder) NO-ERROR.
         LEAVE.
      END.
   END.

   IF AVAIL Order THEN DO:
      IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN
         ocOrderType = "R".
      RETURN
      (IF INDEX(Order.OrderChannel,"pos") > 0 THEN "I" ELSE "D").
   END.
   ELSE RETURN "".

END FUNCTION.

FUNCTION fGetFixedFeeOrderId RETURNS CHARACTER
    (BUFFER ibFixedFee FOR FixedFee):
    
    DEF VAR lcOrderIdValue AS CHARACTER NO-UNDO.

    IF ibFixedFee.OrderId EQ -1 THEN lcOrderIdValue = "MANUAL".
    ELSE IF ibFixedFee.OrderId EQ 0 THEN lcOrderIdValue = "".
    ELSE lcOrderIdValue = STRING(ibFixedFee.OrderID).

    RETURN lcOrderIdValue.
    
END FUNCTION.

/******* MAIN start *********/

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAIL DumpFile THEN lcDelimiter = DumpFile.DumpDelimiter.
            
FIND FIRST ActionLog NO-LOCK WHERE
           ActionLog.Brand    = gcBrand AND
           ActionLog.ActionID BEGINS "TF_READ_" AND
           ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE} NO-ERROR.
         
IF NOT AVAIL ActionLog THEN DO:
   olInterrupted = TRUE.
   LEAVE.
END.
ELSE DO TRANS:

   FIND FIRST bActionLog NO-LOCK WHERE
              bActionLog.Brand    = gcBrand AND
              bActionLog.ActionId = ActionLog.ActionID AND
              bActionLog.ActionStatus NE {&ACTIONLOG_STATUS_CANCELLED} AND
              bActionLog.FromDate <= ActionLog.Todate AND
              bActionLog.ToDate   >= ActionLog.FromDate AND 
        ROWID(bActionLog) NE ROWID(ActionLog) NO-ERROR.
   
   FIND CURRENT ActionLog EXCLUSIVE-LOCK.

   IF AVAIL bActionLog THEN DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ERROR}
         ActionLog.ActionChar = ActionLog.ActionChar + CHR(10) +
                                "HIRE:ERROR:Overlapping ActionLog"
         oiEvents = {&DUMPLOG_ERROR_NOTIFICATION}. 
         olInterrupted = TRUE.
      LEAVE.
   END.
   ELSE ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}.

   ASSIGN
      lcTFBank = ENTRY(3,ActionLog.ActionID,"_")
      ldaDataFrom  = ActionLog.FromDate
      ldaDataTo   = ActionLog.ToDate.

   IF LOOKUP(lcTFBank,{&TF_BANK_CODES}) = 0 THEN DO:

      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ERROR}
         ActionLog.ActionChar = ActionLog.ActionChar + CHR(10) +
                                SUBST("HIRE:ERROR:Unsupported bank code &1", lcTFBank)
         oiEvents = {&DUMPLOG_ERROR_NOTIFICATION} 
         olInterrupted = TRUE.
      LEAVE.
   END.
   
   IF INDEX(icFile, lcTFBank) = 0 THEN DO:

      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ERROR}
         ActionLog.ActionChar = ActionLog.ActionChar + CHR(10) +
                                "HIRE:ERROR:Inconsistent bank code"
         oiEvents = {&DUMPLOG_ERROR_NOTIFICATION} 
         olInterrupted = TRUE.
      LEAVE.
   END.

   FIND CURRENT ActionLog NO-LOCK.
END.

DEFINE TEMP-TABLE ttInstallment NO-UNDO
   FIELD OperCode AS CHAR
   FIELD Custnum AS INT
   FIELD MsSeq   AS INT
   FIELD Amount  AS DEC
   FIELD Items   AS INT
   FIELD OperDate AS DATE
   FIELD Renewal AS CHAR
   FIELD BankCode AS CHAR
   FIELD ResidualAmount AS DEC
   FIELD Channel AS CHAR
   FIELD FFNum AS INT
   FIELD OrderId AS CHAR
   FIELD RowSource AS CHAR
INDEX OperDate IS PRIMARY OperDate.

RUN pCollectActivations.

IF olInterrupted THEN DO TRANS:

   IF RETURN-VALUE BEGINS "ERROR" THEN ASSIGN
      lcError = RETURN-VALUE.
   ELSE lcError = "ERROR:Interrupted".
      
   oiEvents = {&DUMPLOG_ERROR_NOTIFICATION}. 

   FIND CURRENT ActionLog EXCLUSIVE-LOCK.
   
   ASSIGN ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ERROR}.
          ActionLog.ActionChar = ActionLog.ActionChar + 
                                 CHR(10) + "HIRE:" + lcError.
   
   RELEASE ActionLog.
   LEAVE.
END.

RUN pCollectInstallmentContractChanges.
IF olInterrupted THEN DO:
   oiEvents = {&DUMPLOG_ERROR_NOTIFICATION}. 
   LEAVE.
END.

RUN pCollectReactivations.
IF olInterrupted THEN DO:
   oiEvents = {&DUMPLOG_ERROR_NOTIFICATION}. 
   LEAVE.
END.

RUN pCollectACC.
IF olInterrupted THEN DO:
   oiEvents = {&DUMPLOG_ERROR_NOTIFICATION}. 
   LEAVE.
END.

RUN pCollectInstallmentCancellations.
IF olInterrupted THEN DO:
   oiEvents = {&DUMPLOG_ERROR_NOTIFICATION}. 
   LEAVE.
END.

OUTPUT STREAM sLog TO VALUE(icFile).
            
IF DAY(TODAY) < 6 THEN ldaDueDate = DATE(MONTH(TODAY), 6, YEAR(TODAY)).
ELSE IF MONTH(TODAY) = 12 THEN ldaDueDate = DATE(1,6,YEAR(TODAY) + 1).
ELSE ldaDueDate = DATE(MONTH(TODAY) + 1, 6, YEAR(TODAY)).

FOR EACH ttInstallment:

   PUT STREAM sLog UNFORMATTED
      "H"                             lcDelimiter       /*  1: line_type */
      ttInstallment.OperCode          lcDelimiter       /*  2: operation_code */
      ttInstallment.CustNum           lcDelimiter       /*  3: identity  */
      ttInstallment.MsSeq             lcDelimiter       /*  4: subscription_id */
      ttInstallment.Amount            lcDelimiter       /*  5: amount */
      (IF ttInstallment.OperCode EQ "B" THEN ""
       ELSE STRING(ttInstallment.Items)) lcDelimiter    /*  6: number_items */
      fDate2String(ttInstallment.OperDate) lcDelimiter  /*  7: operation_date */
      fDate2String(ldaDueDate) lcDelimiter              /*  8: due_date */
      ttInstallment.Renewal lcDelimiter                 /*  9 payterm_type */ /* YDR-426 */
      ttInstallment.BankCode lcDelimiter                /*  10 bank_code for financed by bank */
      ttInstallment.ResidualAmount lcDelimiter          /*  11 residual_amount */
      ttInstallment.Channel lcDelimiter                 /*  12 channel */
      ttInstallment.OrderId lcDelimiter                 /*  13 OrderId */
      SKIP.
   
   /* mark as transferred only when picking modified ones, full dump 
      ttInstallment.FFNum > 0 THEN DO:
      (control) must not have any influence on daily dumps */
   IF icDumpMode = "modified" THEN DO:
     
      IF ttInstallment.FFNum NE 0 AND
         ttInstallment.RowSource EQ "ACTIVATION" THEN DO:

         FIND FixedFee EXCLUSIVE-LOCK WHERE 
              FixedFee.FFNum = ttInstallment.FFNum NO-ERROR.
         IF AVAIL FixedFee THEN 
            FixedFee.IFSStatus = {&IFS_STATUS_SENT}.
      END.

      IF lcLogDir > "" THEN
         PUT STREAM sFixedFee UNFORMATTED
         "H"                             lcDelimiter       /*  1: line_type */
         ttInstallment.OperCode          lcDelimiter       /*  2: operation_code */
         ttInstallment.CustNum           lcDelimiter       /*  3: identity  */
         ttInstallment.MsSeq             lcDelimiter       /*  4: subscription_id */
         ttInstallment.Amount            lcDelimiter       /*  5: amount */
         (IF ttInstallment.OperCode EQ "B" THEN ""
          ELSE STRING(ttInstallment.Items)) lcDelimiter    /*  6: number_items */
         fDate2String(ttInstallment.OperDate) lcDelimiter  /*  7: operation_date */
         fDate2String(ldaDueDate) lcDelimiter              /*  8: due_date */
         ttInstallment.Renewal lcDelimiter                 /*  9 payterm_type */ /* YDR-426 */
         ttInstallment.BankCode lcDelimiter                /*  10 bank_code for financed by bank */
         ttInstallment.ResidualAmount lcDelimiter          /*  11 residual_amount */
         ttInstallment.Channel lcDelimiter                 /*  12 channel */
         ttInstallment.OrderId lcDelimiter                 /*  13 OrderId */
         ttInstallment.FFNum lcDelimiter
         ttInstallment.RowSource lcDelimiter SKIP.
      
      IF AVAIL FixedFee THEN RELEASE FixedFee.
   END.

END.
   
DO TRANS:
   FIND CURRENT ActionLog EXCLUSIVE-LOCK.
   ASSIGN ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
          ActionLog.ActionChar = ActionLog.ActionChar + CHR(10) + "HIRE:OK".
   FIND CURRENT ActionLog NO-LOCK.
END.

IF NOT SESSION:BATCH THEN
   HIDE FRAME fQty NO-PAUSE.

IF lcLogDir > "" THEN
   OUTPUT STREAM sFixedFee CLOSE.
OUTPUT STREAM sLog CLOSE.

FINALLY:
   EMPTY TEMP-TABLE ttInstallment.
END.

PROCEDURE pCollectActivations:

   DEF VAR llFinancedByBank AS LOG NO-UNDO. 
   DEF VAR lcOrderType AS CHAR NO-UNDO. 
   DEF VAR lcChannel AS CHAR NO-UNDO. 
   DEF VAR lgMsRequest AS LOG NO-UNDO.
   DEF VAR ldResidual  AS DEC NO-UNDO. 

   FF_LOOP:
   FOR EACH FixedFee NO-LOCK WHERE
            FixedFee.IFSStatus = {&IFS_STATUS_WAITING_SENDING}:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE FF_LOOP.
      END.
           
      IF NOT FixedFee.BillCode BEGINS "PAYTERM" AND
         NOT FixedFee.BillCode BEGINS "RVTERM" THEN NEXT.

      IF FixedFee.BegDate >= TODAY THEN NEXT.

      IF FixedFee.FinancedResult EQ {&TF_STATUS_HOLD_SENDING} OR
         FixedFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING} OR
         FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN NEXT.
         
      IF FixedFee.TFBank > "" AND FixedFee.TFBank NE lcTFBank THEN NEXT.
      IF FixedFee.TFBank EQ "" AND lcTFBank NE {&TF_BANK_UNOE} THEN NEXT.

      ldResidual = 0.
      IF NOT FixedFee.BillCode BEGINS "RVTERM" THEN DO:
         FIND FIRST SingleFee NO-LOCK WHERE
                    SingleFee.Brand = gcBrand AND
                    SingleFee.Custnum = FixedFee.Custnum AND
                    SingleFee.HostTable = FixedFee.HostTable AND
                    SingleFee.KeyValue = Fixedfee.KeyValue AND
                    SingleFee.SourceKey = FixedFee.SourceKey AND
                    SingleFee.SourceTable = FixedFee.SourceTable AND
                    SingleFee.CalcObj = "RVTERM" NO-ERROR.
         IF AVAILABLE SingleFee THEN ldResidual = SingleFee.Amt.
      END.
      
      liBatches = 0.
      FOR FIRST FeeModel NO-LOCK WHERE 
                FeeModel.Brand = gcBrand AND
                FeeModel.FeeModel EQ FixedFee.FeeModel,
         FIRST FMItem OF FeeModel NO-LOCK:
            liBatches = FMItem.FFItemQty.
      END.

      llFinancedByBank = (LOOKUP(FixedFee.FinancedResult,
                          {&TF_STATUSES_BANK}) > 0).


      lcChannel = fGetChannel(BUFFER FixedFee, OUTPUT lcOrderType).

      CREATE ttInstallment.
      ASSIGN
         ttInstallment.OperCode = IF FixedFee.BillCode BEGINS "RVTERM" 
                                  THEN "E"
                                  ELSE IF llFinancedByBank 
                                       THEN "C" 
                                       ELSE "A" 
         ttInstallment.Custnum = FixedFee.Custnum
         ttInstallment.MsSeq   = INT(FixedFee.KeyValue)
         ttInstallment.Amount  = FixedFee.Amt
         ttInstallment.Items   = liBatches
         ttInstallment.OperDate = FixedFee.Begdate
         ttInstallment.Renewal = lcOrderType
         ttInstallment.BankCode = FixedFee.TFBank WHEN llFinancedByBank
         ttInstallment.ResidualAmount = ldResidual
         ttInstallment.Channel = lcChannel
         ttInstallment.FFNum = FixedFee.FFNum
         ttInstallment.OrderId = fGetFixedFeeOrderId(BUFFER FixedFee)
         ttInstallment.RowSource = "ACTIVATION"
         oiEvents = oiEvents + 1.

      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents LABEL "Requests"
         WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
            TITLE " Collecting activations " FRAME fQty.
      END.
   
   END.

   RETURN "".

END.

PROCEDURE pCollectACC:
   
   DEF VAR ldCheck       AS DEC  NO-UNDO.
   DEF VAR ldFrom        AS DEC  NO-UNDO.
   DEF VAR ldTo          AS DEC  NO-UNDO.
   DEF VAR lcOrderType   AS CHAR NO-UNDO. 
   DEF VAR lcOrderIdVal  AS CHAR NO-UNDO. 

   DEFINE VARIABLE ldaACCDate AS DATE NO-UNDO. 
   DEFINE VARIABLE liTime AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldeendtime AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE liBatches AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldeAmount AS DECIMAL NO-UNDO. 
   DEF BUFFER bmsowner FOR msowner.
   DEF VAR ldFeeEndDate     AS DATE NO-UNDO.
   DEF VAR llFinancedByBank AS LOG  NO-UNDO. 
   DEF VAR liFFItemQty      AS INT  NO-UNDO.
   DEF VAR ldaFFLastMonth   AS DATE NO-UNDO. 
   DEF VAR liFFItemTotalQty AS INT  NO-UNDO. 
   DEF VAR ldResidual       AS DEC  NO-UNDO. 
   DEF VAR lcOperCode       AS CHAR NO-UNDO.

   /* check from last 20 days if there are ones that have been completed
      yesterday */
   IF icDumpMode = "modified" THEN ASSIGN
      ldFrom  = fMake2Dt(ldaDataFrom,0)
      ldTo    = fMake2Dt(ldaDataTo,86399)
      ldCheck = fMake2Dt(ldaDataFrom - 20,0).
   /* take all */
   ELSE ASSIGN
      ldFrom  = fMake2Dt(2/1/10,0)
      ldTo    = fMake2Dt(TODAY - 1,86399)
      ldCheck = ldFrom.

   REQUEST_LOOP:
   FOR EACH msrequest NO-LOCK where
            msrequest.brand = gcBrand and
            msrequest.reqtype = 10 and
            msrequest.reqstatus = 2 and
            MsRequest.ActStamp >= ldCheck AND
            MsRequest.DoneStamp >= ldFrom AND
            MsRequest.DoneStamp <= ldTo 
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.
      
      ldeendtime = fSecOffset(msrequest.actstamp,-1).

      fSplitTs(msrequest.actstamp, output ldaACCDate, output liTime).
      
      find msowner where
           msowner.msseq = msrequest.msseq and
           msowner.tsend = ldeendtime NO-LOCK NO-ERROR.
      IF NOT AVAIL msowner THEN DO:
         PUT STREAM sFixedFee UNFORMATTED
            "0;" MsRequest.MsSeq ";"
            "ERROR:ACC old msowner not found" SKIP.
         NEXT.
      END.
      
      find bmsowner where
           bmsowner.msseq = msrequest.msseq and
           bmsowner.tsbegin = msrequest.actstamp NO-LOCK NO-ERROR.
      IF NOT AVAIL bmsowner THEN DO:
         PUT STREAM sFixedFee UNFORMATTED
            "0;" MsRequest.MsSeq ";"
            "ERROR:ACC new msowner not found" SKIP.
         NEXT.
      END.

      /* YBU-1191 */
      IF msowner.custnum = bmsowner.custnum THEN DO:
         PUT STREAM sFixedFee UNFORMATTED
            "0;" MsRequest.MsSeq ";"
            "ERROR:ACC new and old msowner are the same" SKIP.
         NEXT.
      END.
      
      FOR EACH DCCLI NO-LOCK WHERE
               DCCLI.MsSeq         = MsRequest.MsSeq      AND
               (DCCLI.DCEvent BEGINS "PAYTERM" OR
                DCCLI.DCEvent BEGINS "RVTERM")            AND
               DCCLI.ValidFrom     < ldaACCDate           AND
               DCCLI.ValidTo      >= ldaACCDate,
         FIRST DayCampaign NO-LOCK WHERE
               DayCampaign.Brand = "1" AND
               DayCampaign.DCEvent = DCCLI.DCevent:
         
         lcOrderIdVal = "".

         RELEASE ttInstallment.

         /* old owner */
         FF_LOOP:
         FOR EACH FixedFee NO-LOCK USE-INDEX Custnum WHERE
                  FixedFee.Brand     = "1" AND
                  FIxedFee.Custnum   = msowner.custnum AND 
                  FixedFee.HostTable = "MobSub" AND
                  FixedFee.KeyValue  = STRING(MsRequest.MsSeq) AND
                  FixedFee.FeeModel  = DayCampaign.FeeModel AND
                  FixedFee.SourceTable = "DCCLI" AND
                  FixedFee.SourceKey = STRING(DCCLI.PerContractID):
            
            IF FixedFee.TFBank > "" AND FixedFee.TFBank NE lcTFBank THEN NEXT REQUEST_LOOP.
            IF FixedFee.TFBank EQ "" AND lcTFBank NE {&TF_BANK_UNOE} THEN NEXT REQUEST_LOOP.
         
            IF FixedFee.FinancedResult EQ {&TF_STATUS_HOLD_SENDING} OR
               FixedFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING} OR
               FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN DO:
               PUT STREAM sFixedFee UNFORMATTED
                  FixedFee.FFNum ";" MsRequest.MsSeq ";"
                  "ERROR:ACC old customer fee bank response not received"
               SKIP.
               NEXT REQUEST_LOOP.
            END.

            /* residual fee should have been moved to new customer */
            ldResidual = 0.
            IF NOT FixedFee.BillCode BEGINS "RVTERM" THEN DO:
               FIND FIRST SingleFee NO-LOCK WHERE
                          SingleFee.Brand = gcBrand AND
                          SingleFee.Custnum = bmsowner.custnum AND
                          SingleFee.HostTable = FixedFee.HostTable AND
                          SingleFee.KeyValue = Fixedfee.KeyValue AND
                          SingleFee.SourceKey = FixedFee.SourceKey AND
                          SingleFee.SourceTable = FixedFee.SourceTable AND
                          SingleFee.CalcObj = "RVTERM" NO-ERROR.
               IF AVAILABLE SingleFee THEN ldResidual = SingleFee.Amt.
            END.

            ASSIGN
               liBatches = 0
               ldeAmount = 0
               liFFItemQty = 0.
            
            FOR FIRST FeeModel NO-LOCK WHERE 
                      FeeModel.Brand = gcBrand AND
                      FeeModel.FeeModel EQ FixedFee.FeeModel,
               FIRST FMItem OF FeeModel NO-LOCK:
            
               FOR EACH FFItem OF FixedFee NO-LOCK:
                  liBatches = liBatches + 1.
               END.
            
               /* The total fee quantity might not be full 24 months
                  due to ACCs */
               ldaFFLastMonth = fPer2Date(YEAR(DCCLI.ValidFrom) * 100 + 
                                          MONTH(DCCLI.ValidFrom),
                                          DayCampaign.DurMonths) - 1.
            
               liFFItemTotalQty = MIN(INTERVAL(ldaFFLastMonth,
                                               FixedFee.BegDate,
                                               "months") + 1,
                                      FMItem.FFItemQty).

               ASSIGN liFFItemQty = liFFItemTotalQty - liBatches
                      ldeAmount   = liFFItemQty * FixedFee.Amt.
            END.

           ASSIGN
              ldFeeEndDate = DATE(FixedFee.EndPeriod MOD 100,
                             1,
                             INT(FixedFee.EndPeriod / 100))
              ldFeeEndDate = fLastDayOfMonth(ldFeeEndDate)
              llFinancedByBank = (LOOKUP(FixedFee.FinancedResult,
                                  {&TF_STATUSES_BANK}) > 0).

           IF FixedFee.BillCode BEGINS "RVTERM" THEN DO:
              lcOperCode = "F".
           END.
           ELSE DO:
              lcOperCode = IF llFinancedByBank THEN "D" ELSE "B".
           END.

           CREATE ttInstallment.
           ASSIGN
              ttInstallment.OperCode = lcOperCode
              ttInstallment.Custnum = msowner.Custnum
              ttInstallment.MsSeq   = msowner.MsSeq
              ttInstallment.Amount  = (IF llFinancedByBank THEN FixedFee.Amt ELSE ldeAmount)
              ttInstallment.Items   = liFFItemQty
              ttInstallment.OperDate = ldFeeEndDate 
              ttInstallment.BankCode = FixedFee.TFBank WHEN llFinancedByBank
              ttInstallment.ResidualAmount = ldResidual
              ttInstallment.Channel = ""
              ttInstallment.OrderId = (IF FixedFee.BegDate < 11/19/2014 THEN ""
                                       ELSE fGetFixedFeeOrderId(BUFFER FixedFee))
              lcOrderIdVal          = ttInstallment.OrderId
              ttInstallment.RowSource = "ACC_OLD"
              ttInstallment.FFNum = FixedFee.FFNum
              oiEvents = oiEvents + 1.

           IF NOT SESSION:BATCH AND oiEvents MOD 10 = 0 THEN DO:
              PAUSE 0.
              DISP oiEvents LABEL "Requests"
              WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                 TITLE " Collecting " FRAME fQty.
           END.

           LEAVE FF_LOOP.

         END.
             
         /* new owner */
         FF_LOOP:
         FOR EACH FixedFee NO-LOCK USE-INDEX Custnum WHERE
                  FixedFee.Brand     = gcBrand AND
                  FIxedFee.Custnum   = bmsowner.custnum AND 
                  FixedFee.HostTable = "MobSub" AND
                  FixedFee.KeyValue  = STRING(bmsowner.MsSeq) AND
                  FixedFee.FeeModel  = DayCampaign.FeeModel AND
                  FixedFee.BegDate  >= ldaACCDate AND
                  FixedFee.SourceTable = "DCCLI" AND
                  FixedFee.SourceKey = STRING(DCCLI.PerContractID):
            
            IF FixedFee.FinancedResult NE {&TF_STATUS_YOIGO} THEN DO:
               /* do not send partial data (old customer) */
               IF AVAIL ttInstallment THEN
                  DELETE ttInstallment.
               PUT STREAM sFixedFee UNFORMATTED
                  FixedFee.FFNum ";" MsRequest.MsSeq ";"
                  "ERROR:ACC new customer fee is not financed by Yoigo" SKIP.
            END.

            ldResidual = 0.
            IF NOT FixedFee.BillCode BEGINS "RVTERM" THEN DO:
               FIND FIRST SingleFee NO-LOCK WHERE
                          SingleFee.Brand = gcBrand AND
                          SingleFee.Custnum = FixedFee.Custnum AND
                          SingleFee.HostTable = FixedFee.HostTable AND
                          SingleFee.KeyValue = Fixedfee.KeyValue AND
                          SingleFee.SourceKey = FixedFee.SourceKey AND
                          SingleFee.SourceTable = FixedFee.SourceTable AND
                          SingleFee.CalcObj = "RVTERM" NO-ERROR.
               IF AVAILABLE SingleFee THEN ldResidual = SingleFee.Amt.
            END.
            
            /* calculate these directly from items
              (not from FMItem.FFItemQty) to get the actualized quantity */
            liBatches = 0.
            FOR EACH FFItem OF FixedFee NO-LOCK:
               liBatches = liBatches + 1.
            END.
           
            CREATE ttInstallment.
            ASSIGN
               ttInstallment.OperCode = IF FixedFee.BillCode BEGINS "RVTERM" 
                                        THEN "G"
                                        ELSE "A"
               ttInstallment.Custnum = bmsowner.Custnum
               ttInstallment.MsSeq   = MsRequest.MsSeq
               ttInstallment.Amount  = FixedFee.Amt
               ttInstallment.Items   = liBatches
               ttInstallment.OperDate = FixedFee.BegDate
               ttInstallment.BankCode =  ""
               ttInstallment.ResidualAmount = ldResidual
               ttInstallment.Channel = fGetChannel(BUFFER FixedFee, OUTPUT lcOrderType)
               ttInstallment.OrderId = IF lcOrderIdVal NE "" THEN lcOrderIdVal 
                                       ELSE fGetFixedFeeOrderId(BUFFER FixedFee)
               ttInstallment.RowSource = "ACC_NEW"
               ttInstallment.FFNum = FixedFee.FFNum
               oiEvents = oiEvents + 1.
         
            IF NOT SESSION:BATCH AND oiEvents MOD 10 = 0 THEN DO:
               PAUSE 0.
               DISP oiEvents LABEL "Requests"
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                  TITLE " Collecting ACCs " FRAME fQty.
            END. 

            LEAVE FF_LOOP.

         END.
      END.
   END.

END PROCEDURE. 


PROCEDURE pCollectInstallmentContractChanges:

   DEF VAR ldaActDate    AS DATE NO-UNDO.
   DEF VAR ldDebt        AS DEC  NO-UNDO.
   DEF VAR ldCheck       AS DEC  NO-UNDO.
   DEF VAR ldFrom        AS DEC  NO-UNDO.
   DEF VAR ldTo          AS DEC  NO-UNDO.
   DEF VAR llFinancedByBank AS LOG NO-UNDO. 
   DEF VAR ldFeeEndDate  AS DATE NO-UNDO.
   DEF VAR liFFItemQty   AS INT  NO-UNDO.
   DEF VAR lcOrderType AS CHAR NO-UNDO. 
   DEF VAR lcChannel AS CHAR NO-UNDO. 
   DEF VAR liBatches AS INT NO-UNDO. 
   DEF VAR ldeAmount AS DEC NO-UNDO. 
   DEF VAR ldaFFLastMonth AS DATE NO-UNDO. 
   DEF VAR liFFItemTotalQty AS INT NO-UNDO. 
   DEF VAR ldeResidualFee AS DEC NO-UNDO. 

   DEF BUFFER bTermRequest FOR MSRequest.
   DEF BUFFER bActRequest FOR MSRequest.
   DEF BUFFER bActDCCLI FOR DCCLI.
   DEF BUFFER bTermDCCLI FOR DCCLI.

   IF icDumpMode = "modified" THEN ASSIGN
      ldFrom  = fMake2Dt(ldaDataFrom,0)
      ldTo    = fMake2Dt(ldaDataTo,86399)
      ldCheck = fMake2Dt(ldaDataFrom - 20,0).
   /* take all */
   ELSE ASSIGN
      ldFrom  = fMake2Dt(2/1/10,0)
      ldTo    = fMake2Dt(TODAY - 1,86399)
      ldCheck = ldFrom.

   REQUEST_LOOP:
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand     = gcBrand AND
            MsRequest.ReqType   = {&REQTYPE_INSTALLMENT_CONTRACT_CHANGE} AND
            MsRequest.ReqStatus = 2       AND
            MsRequest.ActStamp >= ldCheck AND
            MsRequest.DoneStamp >= ldFrom AND
            MsRequest.DoneStamp <= ldTo
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      FIND bTermRequest NO-LOCK WHERE
           bTermRequest.OrigRequest = MsRequest.MsRequest AND
           bTermRequest.ReqType = 9 AND
           bTermRequest.ReqStatus = 2 NO-ERROR.
      
      FIND bActRequest NO-LOCK WHERE
           bActRequest.OrigRequest = MsRequest.MsRequest AND
           bActRequest.ReqType = 8 AND
           bActRequest.ReqStatus = 2 NO-ERROR.

      IF NOT AVAIL bTermRequest OR
         NOT AVAIL bActRequest THEN DO:
      
         IF lcLogDir > "" THEN
            PUT STREAM sFixedFee UNFORMATTED
               MsRequest.MsSeq ";"
               "ERROR:Installment contract change activation or termination request is missing"
            SKIP.
         NEXT.
      END.
      
      fTS2Date(bTermRequest.ActStamp,
               OUTPUT ldaActDate).

      FIND bTermDCCLI WHERE
           bTermDCCLI.MsSeq         = bTermRequest.MsSeq      AND
           bTermDCCLI.DCEvent       = bTermRequest.ReqCParam3 AND
           bTermDCCLI.PerContractID = bTermRequest.ReqIParam3 AND
           bTermDCCLI.TermDate      = ldaActDate NO-LOCK NO-ERROR.

      fTS2Date(bActRequest.ActStamp,
               OUTPUT ldaActDate).
      
      FIND bActDCCLI WHERE
           bActDCCLI.MsSeq     = bActRequest.MsSeq      AND
           bActDCCLI.DCEvent   = bActRequest.ReqCParam3 AND
           bActDCCLI.ValidFrom = ldaActDate
      NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE bTermDCCLI OR
         NOT AVAILABLE bActDCCLI THEN DO:
         IF lcLogDir > "" THEN
            PUT STREAM sFixedFee UNFORMATTED
               ";" MsRequest.MsSeq ";"
               "ERROR:Installment contract change old or new DCCLI not found"
            SKIP.
         NEXT.
      END.
      
      /* rest of the unbilled fixed fees are converted into a single fee in
         termination */
      ASSIGN
         ldDebt = 0
         llFinancedByBank = FALSE
         liFFItemQty = 0.

      RELEASE ttInstallment.

      /* Old fee */
      FOR FIRST FixedFee NO-LOCK WHERE
                FixedFee.Brand = gcBrand AND
                FixedFee.Custnum = MsRequest.Custnum AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue = STRING(MsRequest.MsSeq) AND
                (FixedFee.BillCode = "PAYTERM" OR
                 FixedFee.BillCode = "RVTERM") AND
                FixedFee.SourceTable = "DCCLI" AND
                FixedFee.SourceKey = STRING(bTermDCCLI.PerContractID),
          FIRST DayCampaign NO-LOCK USE-INDEX DCEvent WHERE
                DayCampaign.Brand = gcBrand AND
                DayCampaign.DCEvent = bTermDCCLI.DCEvent:
            
         IF FixedFee.TFBank > "" AND FixedFee.TFBank NE lcTFBank THEN NEXT REQUEST_LOOP.
         IF FixedFee.TFBank EQ "" AND lcTFBank NE {&TF_BANK_UNOE} THEN NEXT REQUEST_LOOP.
         
         /* wait bank response for the old contract before sending B/D + A row 
            A/C row for the old contract should go in the same or earlier HIRE file */
         IF FixedFee.FinancedResult EQ {&TF_STATUS_HOLD_SENDING} OR
            FixedFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING} OR
            FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN DO:
            PUT STREAM sFixedFee UNFORMATTED
               FixedFee.FFNum ";" MsRequest.MsSeq ";"
               "ERROR:Installment contract change old contract bank response not received"
            SKIP.
            NEXT REQUEST_LOOP.
         END.
            
         ASSIGN
            liBatches = 0
            ldeAmount = 0
            liFFItemQty = 0.

         FOR FIRST FeeModel NO-LOCK WHERE 
                   FeeModel.Brand = gcBrand AND
                   FeeModel.FeeModel EQ FixedFee.FeeModel,
            FIRST FMItem OF FeeModel NO-LOCK:

            FOR EACH FFItem OF FixedFee NO-LOCK:
               liBatches = liBatches + 1.
            END.
         
            /* The total fee quantity might not be full 24 months
               due to ACCs */
            ldaFFLastMonth = fPer2Date(YEAR(bActDCCLI.ValidFrom) * 100 + 
                                       MONTH(bActDCCLI.ValidFrom),
                                       DayCampaign.DurMonths) - 1.
         
            liFFItemTotalQty = MIN(INTERVAL(ldaFFLastMonth,
                                            FixedFee.BegDate,
                                            "months") + 1,
                                   FMItem.FFItemQty).

            ASSIGN liFFItemQty = liFFItemTotalQty - liBatches
                   ldeAmount   = liFFItemQty * FixedFee.Amt.
         END.

         ASSIGN
            ldFeeEndDate = DATE(FixedFee.EndPeriod MOD 100,
                                1,
                                INT(FixedFee.EndPeriod / 100))
            ldFeeEndDate = fLastDayOfMonth(ldFeeEndDate)
            llFinancedByBank = (LOOKUP(FixedFee.FinancedResult,
                                {&TF_STATUSES_BANK}) > 0).

        IF FixedFee.BegDate > ldFeeEndDate THEN 
           fTS2Date(bTermRequest.DoneStamp, OUTPUT ldFeeEndDate).
         
         IF FixedFee.BillCode EQ "PAYTERM" THEN DO:

            FIND FIRST SingleFee NO-LOCK WHERE
                       SingleFee.Brand = gcBrand AND
                       SingleFee.Custnum = FixedFee.Custnum AND
                       SingleFee.HostTable = FixedFee.HostTable AND
                       SingleFee.KeyValue = Fixedfee.KeyValue AND
                       SingleFee.SourceKey = FixedFee.SourceKey AND
                       SingleFee.SourceTable = FixedFee.SourceTable AND
                       SingleFee.CalcObj = "RVTERM" NO-ERROR.

            IF AVAIL SingleFee THEN 
               ldeResidualFee = SingleFee.Amt.
            ELSE IF bTermDCCLI.Amount NE ? THEN
               ldeResidualFee = bTermDCCLI.Amount.
            ELSE ldeResidualFee = 0.
         END.
         ELSE ldeResidualFee = 0.

         CREATE ttInstallment.
         ASSIGN
            ttInstallment.OperCode = (IF FixedFee.BillCode EQ "RVTERM" THEN "F"
                                      ELSE IF llFinancedByBank THEN "D" ELSE "B")
            ttInstallment.Custnum = MsRequest.Custnum
            ttInstallment.MsSeq   = MsRequest.MsSeq
            ttInstallment.Amount  = (IF llFinancedByBank THEN FixedFee.Amt ELSE ldeAmount)
            ttInstallment.Items   = (IF llFinancedByBank THEN liFFItemQty ELSE 0)
            ttInstallment.OperDate = ldFeeEndDate
            ttInstallment.BankCode = FixedFee.TFBank WHEN llFinancedByBank
            ttInstallment.ResidualAmount = ldeResidualFee
            ttInstallment.Channel = ""
            ttInstallment.OrderId = fGetFixedFeeOrderId(BUFFER FixedFee)
            ttInstallment.RowSource = "INSTALLMENT_CHANGE_OLD"
            ttInstallment.FFNum = FixedFee.FFNum
            oiEvents = oiEvents + 1.
      END.

      /* New fee */
      FOR FIRST FixedFee NO-LOCK WHERE
                FixedFee.Brand = gcBrand AND
                FixedFee.Custnum = MsRequest.Custnum AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue = STRING(MsRequest.MsSeq) AND
               (FixedFee.BillCode = "PAYTERM" OR
                FixedFee.BillCode = "RVTERM") AND
                FixedFee.SourceTable = "DCCLI" AND
                FixedFee.SourceKey = STRING(bActDCCLI.PerContractID):

         /* calculate these directly from items (not from FMItem.FFItemQty),
            to get the actualized quantity */
         liBatches = 0.
         FOR EACH FFItem OF FixedFee NO-LOCK:
            liBatches = liBatches + 1.
         END.
         
         /* the contract should be always financed by Yoigo */
         IF NOT (FixedFee.FinancedResult BEGINS "Y" OR
                 FixedFee.FinancedResult EQ "" OR
                 (LENGTH(FixedFee.FinancedResult) EQ 2 AND
                         FixedFee.FinancedResult NE "00"))
            THEN DO:
            PUT STREAM sFixedFee UNFORMATTED
               FixedFee.FFNum ";" MsRequest.MsSeq ";"
               "Installment contract change new Fixedfee financial status is not not Y00"
            SKIP.

            NEXT REQUEST_LOOP.
         END.
         
         IF FixedFee.BillCode EQ "PAYTERM" THEN 
            FIND FIRST SingleFee NO-LOCK WHERE
                       SingleFee.Brand = gcBrand AND
                       SingleFee.Custnum = FixedFee.Custnum AND
                       SingleFee.HostTable = FixedFee.HostTable AND
                       SingleFee.KeyValue = Fixedfee.KeyValue AND
                       SingleFee.SourceKey = FixedFee.SourceKey AND
                       SingleFee.SourceTable = FixedFee.SourceTable AND
                       SingleFee.CalcObj = "RVTERM" NO-ERROR.
         ELSE RELEASE SingleFee.

         lcChannel = fGetChannel(BUFFER FixedFee, OUTPUT lcOrderType).

         CREATE ttInstallment.
         ASSIGN
            ttInstallment.OperCode = (IF FixedFee.BillCode EQ "PAYTERM" THEN "A" 
                                      ELSE "G")
            ttInstallment.Custnum = FixedFee.Custnum
            ttInstallment.MsSeq   = INT(FixedFee.KeyValue)
            ttInstallment.Amount  = FixedFee.Amt
            ttInstallment.Items   = liBatches
            ttInstallment.OperDate = FixedFee.Begdate
            ttInstallment.Renewal = lcOrderType
            ttInstallment.BankCode = ""
            ttInstallment.ResidualAmount = (IF AVAIL SingleFee
                                            THEN SingleFee.Amt ELSE 0) 
            ttInstallment.Channel = lcChannel
            ttInstallment.OrderId = fGetFixedFeeOrderId(BUFFER FixedFee) 
            ttInstallment.RowSource = "INSTALLMENT_CHANGE_NEW"
            ttInstallment.FFNum = FixedFee.FFNum
            oiEvents = oiEvents + 1.
         
         IF NOT SESSION:BATCH AND oiEvents MOD 10 = 0 THEN DO:
            PAUSE 0.
            DISP oiEvents LABEL "Requests"
            WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                 TITLE " Collecting installment contract changes " FRAME fQty.
         END.
      END.
 
   END.

END PROCEDURE.

PROCEDURE pCollectReactivations:

   DEF VAR ldDebt        AS DEC  NO-UNDO.
   DEF VAR ldCheck       AS DEC  NO-UNDO.
   DEF VAR ldFrom        AS DEC  NO-UNDO.
   DEF VAR ldTo          AS DEC  NO-UNDO.
   DEF VAR ldaReacDate   AS DATE NO-UNDO.
   DEF VAR liDoneTime    AS INT  NO-UNDO.
   DEF VAR llFinancedByBank AS LOG NO-UNDO. 
   DEF VAR ldFeeEndDate  AS DATE NO-UNDO.
   DEF VAR liFFItemQty   AS INT  NO-UNDO.
   DEF VAR ldePendingFees AS DEC NO-UNDO. 
   DEF VAR llPendingFeesBilled AS LOG NO-UNDO. 
   DEF VAR llTerminationSent AS LOG NO-UNDO. 
   DEF VAR ldResidual    AS DEC  NO-UNDO.     
   DEF VAR ldResidualNB  AS DEC  NO-UNDO.

   DEF BUFFER bMsRequest     FOR MsRequest.
   DEF BUFFER bMainMsRequest FOR MsRequest.
   DEF BUFFER bSubMsRequest  FOR MsRequest.

   /* use action log from/to dates */
   IF icDumpMode = "modified" THEN ASSIGN
      ldFrom  = fMake2Dt(ldaDataFrom,0)
      ldTo    = fMake2Dt(ldaDataTo,86399)
      ldCheck = fMake2Dt(ldaDataFrom - 20,0).
   /* take all */
   ELSE ASSIGN
      ldFrom  = fMake2Dt(2/1/10,0)
      ldTo    = fMake2Dt(TODAY - 1,86399)
      ldCheck = ldFrom.

   REQUEST_LOOP:
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand     = gcBrand AND
            MsRequest.ReqType   = 8       AND
            MsRequest.ReqStatus = 2       AND
            MsRequest.ActStamp >= ldCheck AND
            (MsRequest.ReqCparam3 BEGINS "PAYTERM" OR
             MsRequest.ReqCparam3 BEGINS "RVTERM") AND
            MsRequest.DoneStamp >= ldFrom AND
            MsRequest.DoneStamp <= ldTo   AND
           (MsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} OR
            MsRequest.ReqSource = {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER})
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      fSplitTS(MsRequest.ActStamp,
               OUTPUT ldaReacDate,
               OUTPUT liDoneTime).

      FIND FIRST DCCLI WHERE
                 DCCLI.MsSeq         = MsRequest.MsSeq      AND
                 DCCLI.DCEvent       = MsRequest.ReqCParam3 AND
                 DCCLI.PerContractID = MsRequest.ReqIParam3 
      NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE DCCLI THEN NEXT.

      /* rest of the unbilled fixed fees are converted into a single fee in
         termination */
      ASSIGN
         ldDebt = 0
         llFinancedByBank = FALSE
         liFFItemQty = 0
         ldePendingFees = 0
         llPendingFeesBilled = FALSE
         llTerminationSent = FALSE.

      FOR FIRST FixedFee NO-LOCK WHERE
                FixedFee.Brand = gcBrand AND
                FixedFee.Custnum = MsRequest.Custnum AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue = STRING(MsRequest.MsSeq) AND
                (FixedFee.BillCode BEGINS "PAYTERM" OR 
                 FixedFee.BillCode BEGINS "RVTERM") AND 
                FixedFee.SourceTable = "DCCLI" AND
                FixedFee.SourceKey = STRING(DCCLI.PerContractID):
            
         IF FixedFee.TFBank > "" AND FixedFee.TFBank NE lcTFBank THEN NEXT.
         IF FixedFee.TFBank EQ "" AND lcTFBank NE {&TF_BANK_UNOE} THEN NEXT.
      
         /* wait bank response for the old contract before sending B/D + A row 
            A/C row for the old contract should go in the same or earlier HIRE file */
         IF FixedFee.FinancedResult EQ {&TF_STATUS_HOLD_SENDING} OR
            FixedFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING} OR
            FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN DO:
            PUT STREAM sFixedFee UNFORMATTED
               FixedFee.FFNum ";" MsRequest.MsSeq ";"
               "ERROR:Reactivated installment bank response not received"
            SKIP.
            NEXT REQUEST_LOOP.
         END.
         
         /* check if fees are already billed */
         FIND FIRST SingleFee WHERE
                    SingleFee.Brand     = gcBrand AND
                    SingleFee.HostTable = "MobSub" AND
                    SingleFee.KeyValue  = STRING(MsRequest.MsSeq) AND
                    SingleFee.CalcObj   = "DC" + STRING(DCCLI.PerContractID)
         NO-LOCK NO-ERROR.
         IF AVAILABLE SingleFee THEN DO:

            IF SingleFee.Billed AND 
               CAN-FIND(FIRST Invoice NO-LOCK wHERE
                              Invoice.InvNum = SingleFee.InvNum AND
                              Invoice.InvType = 1) THEN
               llPendingFeesBilled = TRUE.
         END.
            
         /* it's assumed that residual fee is not recreated
            after reactivation */
         ASSIGN
            ldResidual = 0
            ldResidualNB = 0.
         IF NOT FixedFee.BillCode BEGINS "RVTERM" THEN DO:            
            FIND FIRST SingleFee NO-LOCK WHERE
                       SingleFee.Brand = gcBrand AND
                       SingleFee.Custnum = FixedFee.Custnum AND
                       SingleFee.HostTable = FixedFee.HostTable AND
                       SingleFee.KeyValue = Fixedfee.KeyValue AND
                       SingleFee.SourceKey = FixedFee.SourceKey AND
                       SingleFee.SourceTable = FixedFee.SourceTable AND
                       SingleFee.CalcObj = "RVTERM" NO-ERROR.
            IF AVAIL SingleFee THEN DO:
               ldResidual = SingleFee.Amt.
               IF  SingleFee.Billed AND
                  CAN-FIND(FIRST Invoice NO-LOCK wHERE
                                 Invoice.InvNum = SingleFee.InvNum AND
                                 Invoice.InvType = 1) THEN
                  llPendingFeesBilled = TRUE.
               ELSE ldResidualNB = SingleFee.Amt.   
            END.      
         END.
         
         /* collect pending fees */
         FOR EACH FFItem NO-LOCK WHERE
                  FFItem.FFNum = FixedFee.FFNum:
            IF FFItem.Billed AND
               CAN-FIND(FIRST Invoice WHERE
                              Invoice.InvNum = FFItem.InvNum AND
                              Invoice.InvType = 1) THEN NEXT.
            ASSIGN
               liFFItemQty = liFFItemQty + 1
               ldePendingFees = ldePendingFees + FFItem.Amt.
         END.

         /* reactivated fee can be financed by bank only if the
            reactivation happpened during the termination month
            and the termination was not yet sent to bank */
         ASSIGN
            ldFeeEndDate = DATE(FixedFee.EndPeriod MOD 100,
                                1,
                                INT(FixedFee.EndPeriod / 100))
            ldFeeEndDate = fLastDayOfMonth(ldFeeEndDate)
            llFinancedByBank = (LOOKUP(FixedFee.FinancedResult,
                                {&TF_STATUSES_BANK}) > 0).
            
         IF llFinancedByBank THEN DO:
         
            FIND FIRST FixedFeeTF NO-LOCK WHERE
                       FixedFeeTF.FFNum = FixedFee.FFNum NO-ERROR.
      
            IF AVAIL FixedfeeTF AND
                     FixedFeeTF.CancelStatus NE ""
               THEN llTerminationSent = TRUE.
               ELSE IF NOT llPendingFeesBilled THEN NEXT REQUEST_LOOP.
         END.
         ELSE IF NOT llPendingFeesBilled THEN NEXT REQUEST_LOOP.
         
         IF NOT llPendingFeesBilled AND
            ((llTerminationSent AND llFinancedByBank) OR 
             FixedFee.FinancedResult EQ {&TF_STATUS_YOIGO_REACTIVATION_FBB})
            THEN DO:
         
            IF liFFItemQty EQ 0 THEN DO:
                PUT STREAM sFixedFee UNFORMATTED
                   FixedFee.FFNum ";" MsRequest.MsSeq ";"
                "SKIPPED:MF quantity is 0, termination row not sent (REACTIVATION)"
                SKIP.
                NEXT.
            END.
            
            CREATE ttInstallment.
            ASSIGN
               ttInstallment.OperCode = IF FixedFee.BillCode BEGINS "RVTERM" 
                                        THEN "G"
                                        ELSE "D"
               ttInstallment.Custnum = MsRequest.Custnum
               ttInstallment.MsSeq   = MsRequest.MsSeq
               ttInstallment.Amount  = ldePendingFees / liFFItemQty 
               ttInstallment.Items   = liFFItemQty
               ttInstallment.OperDate = 
                  (IF FixedFee.BegDate < DATE(MONTH(ldaReacDate),
                                              1,
                                              YEAR(ldaReacDate))
                   THEN fLastDayOfMonth(ADD-INTERVAL(ldaReacDate,-1,"months"))
                   ELSE ldaReacDate)
               ttInstallment.BankCode = FixedFee.TFBank WHEN llFinancedByBank
               ttInstallment.ResidualAmount = ldResidual
               ttInstallment.Channel = ""
               ttInstallment.OrderId = fGetFixedFeeOrderId(BUFFER FixedFee)
               ttInstallment.RowSource = "REACTIVATION"
               ttInstallment.FFNum = FixedFee.FFNum
               oiEvents = oiEvents + 1.
         END.

         IF llFinancedByBank AND 
            llTerminationSent THEN llFinancedByBank = FALSE.

         IF liFFItemQty EQ 0 THEN DO:
            PUT STREAM sFixedFee UNFORMATTED
               FixedFee.FFNum ";" MsRequest.MsSeq ";"
               "SKIPPED:MF quantity is 0, activation row not sent (REACTIVATION)"
            SKIP.
            NEXT.
         END.

         CREATE ttInstallment.
         ASSIGN
            ttInstallment.OperCode = IF FixedFee.BillCode BEGINS "RVTERM"
                                     THEN "G"
                                     ELSE IF llFinancedByBank 
                                          THEN "C" 
                                          ELSE "A"
            ttInstallment.Custnum = MsRequest.Custnum
            ttInstallment.MsSeq   = MsRequest.MsSeq
            ttInstallment.Amount  = ldePendingFees / liFFItemQty 
            ttInstallment.Items   = liFFItemQty
            ttInstallment.OperDate = ldaReacDate
            ttInstallment.BankCode = FixedFee.TFBank WHEN llFinancedByBank
            ttInstallment.ResidualAmount = ldResidualNB
            ttInstallment.Channel = ""
            ttInstallment.OrderId = (IF FixedFee.BegDate < 11/19/2014 THEN "" 
                                     ELSE fGetFixedFeeOrderId(BUFFER FixedFee))
            ttInstallment.RowSource = "REACTIVATION"
            ttInstallment.FFNum = FixedFee.FFNum
            oiEvents = oiEvents + 1.

         IF NOT SESSION:BATCH AND oiEvents MOD 10 = 0 THEN DO:
            PAUSE 0.
            DISP oiEvents LABEL "Requests"
            WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                 TITLE " Collecting reactivations " FRAME fQty.
         END.
      END.
   END.

END PROCEDURE.

PROCEDURE pCollectInstallmentCancellations:

   DEF VAR ldaActDate    AS DATE NO-UNDO.
   DEF VAR ldCheck       AS DEC  NO-UNDO.
   DEF VAR ldFrom        AS DEC  NO-UNDO.
   DEF VAR ldTo          AS DEC  NO-UNDO.
   DEF VAR llFinancedByBank AS LOG NO-UNDO. 
   DEF VAR ldFeeEndDate  AS DATE NO-UNDO.
   DEF VAR liFFItemQty   AS INT  NO-UNDO.
   DEF VAR liBatches AS INT NO-UNDO. 
   DEF VAR ldeAmount AS DEC NO-UNDO. 
   DEF VAR ldeResidualAmt AS DEC NO-UNDO. 
   DEF VAR lcCancelType AS CHAR NO-UNDO. 

   DEF BUFFER bTermDCCLI FOR DCCLI.
   DEF BUFFER bMainRequest FOR MSRequest.

   IF icDumpMode = "modified" THEN ASSIGN
      ldFrom  = fMake2Dt(ldaDataFrom,0)
      ldTo    = fMake2Dt(ldaDataTo,86399)
      ldCheck = fMake2Dt(ldaDataFrom - 20,0).
   /* take all */
   ELSE ASSIGN
      ldFrom  = fMake2Dt(2/1/10,0)
      ldTo    = fMake2Dt(TODAY - 1,86399)
      ldCheck = ldFrom.

   REQUEST_LOOP:
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand     = gcBrand AND
            MsRequest.ReqType   = 9 AND
            MsRequest.ReqStatus = 2       AND
            MsRequest.ActStamp >= ldCheck AND
            MsRequest.DoneStamp >= ldFrom AND
            MsRequest.DoneStamp <= ldTo AND
            (MsRequest.ReqCparam3 BEGINS "PAYTERM" OR
             MsRequest.ReqCParam3 BEGINS "RVTERM") AND
           (MsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} OR
            MsRequest.ReqSource = {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} OR
            MsRequest.ReqCParam2 = "canc")
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:
      
      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      /* for Q25 subscription termination is not reported here */
      IF MsRequest.ReqCParam3 BEGINS "RVTERM" AND
         MsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION}
      THEN NEXT. 

      IF MsRequest.ReqCparam2 EQ "canc" THEN
         lcCancelType = "INSTALLMENT_CANCELLATION".
      ELSE lcCancelType = "ORDER_CANCELLATION".

      IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} AND
         NOT CAN-FIND(
            FIRST bMainRequest NO-LOCK WHERE
                  bMainRequest.MsRequest = MsRequest.OrigRequest AND
                  bMainRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
           LOOKUP(bMainRequest.ReqCParam3,
                  SUBST("&1,&2,&3",
                    {&SUBSCRIPTION_TERM_REASON_ORDER_CANCELLATION},
                    {&SUBSCRIPTION_TERM_REASON_POS_ORDER_CANCELATION},
                    {&SUBSCRIPTION_TERM_REASON_DIRECT_ORDER_CANCELATION})) > 0)
         THEN NEXT.
      
      fTS2Date(MsRequest.ActStamp,
               OUTPUT ldaActDate).

      FIND bTermDCCLI WHERE
           bTermDCCLI.MsSeq         = MsRequest.MsSeq      AND
           bTermDCCLI.DCEvent       = MsRequest.ReqCParam3 AND
           bTermDCCLI.PerContractID = MsRequest.ReqIParam3 AND
           bTermDCCLI.TermDate      = ldaActDate NO-LOCK NO-ERROR.

      IF NOT AVAILABLE bTermDCCLI THEN DO:
         IF lcLogDir > "" THEN
            PUT STREAM sFixedFee UNFORMATTED
               ";" MsRequest.MsSeq ";"
               "ERROR:Installment cancellation DCCLI not found"
            SKIP.
         NEXT.
      END.
      
      FOR FIRST FixedFee NO-LOCK WHERE
                FixedFee.Brand = gcBrand AND
                FixedFee.Custnum = MsRequest.Custnum AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue = STRING(MsRequest.MsSeq) AND
                (FixedFee.BillCode BEGINS "PAYTERM" OR
                 FixedFee.BillCode BEGINS "RVTERM") AND
                FixedFee.SourceTable = "DCCLI" AND
                FixedFee.SourceKey = STRING(bTermDCCLI.PerContractID),
          FIRST DayCampaign NO-LOCK USE-INDEX DCEvent WHERE
                DayCampaign.Brand = gcBrand AND
                DayCampaign.DCEvent = bTermDCCLI.DCEvent:
      
         IF FixedFee.IFSStatus NE {&IFS_STATUS_SENT} THEN NEXT REQUEST_LOOP. 
         IF FixedFee.TFBank > "" AND FixedFee.TFBank NE lcTFBank THEN NEXT REQUEST_LOOP.
         IF FixedFee.TFBank EQ "" AND lcTFBank NE {&TF_BANK_UNOE} THEN NEXT REQUEST_LOOP.
         
         /* wait bank response for the old contract before sending B/D + A row 
            A/C row for the old contract should go in the same or earlier HIRE file */
         IF FixedFee.FinancedResult EQ {&TF_STATUS_HOLD_SENDING} OR
            FixedFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING} OR
            FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN DO:
            PUT STREAM sFixedFee UNFORMATTED
               FixedFee.FFNum ";" MsRequest.MsSeq ";"
               "ERROR:Installment cancellation contract bank response not received"
            SKIP.
            NEXT REQUEST_LOOP.
         END.
            
         ASSIGN
            liBatches = 0
            ldeAmount = 0
            liFFItemQty = 0.

         FOR FIRST FeeModel NO-LOCK WHERE 
                   FeeModel.Brand = gcBrand AND
                   FeeModel.FeeModel EQ FixedFee.FeeModel,
            FIRST FMItem OF FeeModel NO-LOCK:

            FOR EACH FFItem OF FixedFee NO-LOCK:
               liBatches = liBatches + 1.
            END.
         
            ASSIGN liFFItemQty = FMItem.FFItemQty - liBatches
                   ldeAmount   = liFFItemQty * FixedFee.Amt.
         END.

         ldeResidualAmt = 0.
         IF NOT FixedFee.BillCode BEGINS "RVTERM" THEN DO: 
            FIND FIRST SingleFee NO-LOCK WHERE
                       SingleFee.Brand = gcBrand AND
                       SingleFee.Custnum = FixedFee.Custnum AND
                       SingleFee.HostTable = FixedFee.HostTable AND
                       SingleFee.KeyValue = Fixedfee.KeyValue AND
                       SingleFee.SourceKey = FixedFee.SourceKey AND
                       SingleFee.SourceTable = FixedFee.SourceTable AND
                       SingleFee.CalcObj = "RVTERM" NO-ERROR.
            IF AVAIL SingleFee THEN ldeResidualAmt = SingleFee.Amt.
            ELSE IF bTermDCCLI.Amount > 0 THEN 
               ldeResidualAmt = bTermDCCLI.Amount.
         END.

         ASSIGN
            ldFeeEndDate = DATE(FixedFee.EndPeriod MOD 100,
                                1,
                                INT(FixedFee.EndPeriod / 100))
            ldFeeEndDate = fLastDayOfMonth(ldFeeEndDate)
            llFinancedByBank = (LOOKUP(FixedFee.FinancedResult,
                                {&TF_STATUSES_BANK}) > 0).

         CREATE ttInstallment.
         ASSIGN
            ttInstallment.OperCode = IF FixedFee.BillCode BEGINS "RVTERM" 
                                     THEN "F"
                                     ELSE IF llFinancedByBank 
                                          THEN "D" 
                                          ELSE "B"
            ttInstallment.Custnum = MsRequest.Custnum
            ttInstallment.MsSeq   = MsRequest.MsSeq
            ttInstallment.Amount  = (IF llFinancedByBank THEN FixedFee.Amt ELSE ldeAmount)
            ttInstallment.Items   = (IF llFinancedByBank THEN liFFItemQty ELSE 0)
            ttInstallment.OperDate = ldFeeEndDate
            ttInstallment.BankCode = FixedFee.TFBank WHEN llFinancedByBank
            ttInstallment.ResidualAmount = ldeResidualAmt
            ttInstallment.Channel = ""
            ttInstallment.OrderId = fGetFixedFeeOrderId(BUFFER FixedFee)
            ttInstallment.RowSource = lcCancelType 
            ttInstallment.FFNum = FixedFee.FFNum
            oiEvents = oiEvents + 1.
      END.
         
      IF NOT SESSION:BATCH AND oiEvents MOD 10 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents LABEL "Requests"
         WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
              TITLE " Collecting order/installment cancellations " FRAME fQty.
      END.
   END.

END.


/******* MAIN end *********/
