/* ----------------------------------------------------------------------
  MODULE .......: ifs_installment_operation.p
  TASK .........: Create a dump file for IFS from monthly installment
                  creations, cancellations and acc. YDR-328
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 26.08.11
  Version ......: yoigo
---------------------------------------------------------------------- */
/*
{commali.i}
{dumpfile_run.i}
{tmsconst.i}
{date.i}

&SCOPED-DEFINE BANK_CODE "0049"

DEF INPUT PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.
*/

{commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{dumpfile_run.i}
{tmsconst.i}
{date.i}

&SCOPED-DEFINE BANK_CODE "0049"

DEFINE VARIABLE ldaFrom AS DATE NO-UNDO INIT 02/17/2014.
DEFINE VARIABLE ldaTo AS DATE NO-UNDO INIT 02/23/2014.

DEF VAR iiDumpID      AS INT  NO-UNDO.
DEF VAR icFile        AS CHAR NO-UNDO.
icFile = "/apps/yoigo/tms_support/billing/log/IFS_HIRE_OPERATION_" +
   string(year(ldafrom) * 10000 + month(ldafrom) * 100 + day(ldafrom)) + "_" +
   string(year(ldaTo) * 10000 + month(ldaTo) * 100 + day(ldaTo)) + ".DAT".
   
DEF VAR icDumpMode    AS CHAR NO-UNDO.
icDumpMode = "modified".
DEF VAR idLastDump    AS DEC  NO-UNDO.
DEF VAR icEventSource AS CHAR NO-UNDO.
DEF VAR icEventFields AS CHAR NO-UNDO.
DEF VAR oiEvents      AS INT  NO-UNDO.
DEF VAR olInterrupted AS LOG  NO-UNDO.


DEF VAR liBatches     AS INT  NO-UNDO.
DEF VAR lcDelimiter   AS CHAR NO-UNDO INIT ";".


DEFINE STREAM sLog.

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

/******* MAIN start *********/
/*
FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
*/
DEFINE TEMP-TABLE ttInstallment NO-UNDO
   FIELD OperCode AS CHAR
   FIELD Custnum AS INT
   FIELD MsSeq   AS INT
   FIELD Amount  AS DEC
   FIELD Items   AS INT
   FIELD OperDate AS DATE
   FIELD DueDate AS DATE
   FIELD Renewal AS CHAR
   FIELD BankCode AS CHAR
INDEX OperDate IS PRIMARY OperDate.

RUN pCollectActivations.
IF olInterrupted THEN LEAVE.

RUN pCollectTerminations.
IF olInterrupted THEN LEAVE.

RUN pCollectACC.
IF olInterrupted THEN LEAVE.

OUTPUT STREAM sLog TO VALUE(icFile).

FOR EACH ttInstallment:

   PUT STREAM sLog UNFORMATTED
      "H"                             lcDelimiter  /*  1: line_type */
      ttInstallment.OperCode          lcDelimiter  /*  2: operation_code */
      ttInstallment.CustNum           lcDelimiter  /*  3: identity  */
      ttInstallment.MsSeq             lcDelimiter  /*  4: subscription_id */
      ttInstallment.Amount            lcDelimiter  /*  5: amount */
      (IF ttInstallment.OperCode EQ "B" THEN ""
       ELSE STRING(ttInstallment.Items)) lcDelimiter  /*  6: number_items */
      fDate2String(ttInstallment.OperDate) lcDelimiter  /* 7: operation_date */
      fDate2String(ttInstallment.Duedate) lcDelimiter  /*  8: due_date */
      ttInstallment.Renewal lcDelimiter /* 9 payterm_type */ /* YDR-426 */
      ttInstallment.BankCode lcDelimiter
      SKIP.
END.
  
EMPTY TEMP-TABLE ttInstallment.

IF NOT SESSION:BATCH THEN
   HIDE FRAME fQty NO-PAUSE.

OUTPUT STREAM sLog CLOSE.

PROCEDURE pCollectActivations:

   DEF VAR ldaDueDate    AS DATE NO-UNDO.
   DEF VAR ldCheck       AS DEC  NO-UNDO.
   DEF VAR ldFrom        AS DEC  NO-UNDO.
   DEF VAR ldTo          AS DEC  NO-UNDO.
   DEFINE VARIABLE ldaActDate AS DATE NO-UNDO. 
   DEFINE VARIABLE liTime AS INTEGER NO-UNDO. 
   DEF VAR lcRenewal AS CHAR NO-UNDO. 
   DEF VAR llFinancedByBank AS LOG NO-UNDO. 

   DEF BUFFER bMsRequest FOR MsRequest.

   /* check from last 20 days if there are ones that have been completed
      yesterday */
   IF icDumpMode = "modified" THEN ASSIGN
      ldFrom  = fMake2Dt(ldaFrom,0)
      ldTo    = fMake2Dt(ldaTo,86399)
      ldCheck = fMake2Dt(ldaFrom - 20,0).
   /* take all */
   ELSE ASSIGN
      ldFrom  = fMake2Dt(2/1/10,0)
      ldTo    = fMake2Dt(TODAY - 1,86399)
      ldCheck = ldFrom.

   RequestLoop:
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand     = gcBrand AND
            MsRequest.ReqType   = 8       AND
            MsRequest.ReqStatus = 2       AND
            MsRequest.ActStamp >= ldCheck AND
            MsRequest.DoneStamp >= ldFrom AND
            MsRequest.DoneStamp <= ldTo,
      FIRST DayCampaign NO-LOCK USE-INDEX DCEvent WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = MsRequest.ReqCParam3 AND
            DayCampaign.DCType = "5":

      if MsRequest.reqsource EQ "4" THEN NEXT. 

      lcRenewal = "".
      
      /* Include only indirect channels */
      IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} THEN DO:
         FIND FIRST Order WHERE
                    Order.MsSeq = MsRequest.MsSeq AND
                    Order.OrderType < 2 NO-LOCK NO-ERROR.
         IF AVAIL Order THEN DO:
            IF INDEX(Order.OrderChannel,"pos") = 0 THEN NEXT RequestLoop.

            /* YDR-543 - Only send when the MNP status is APOR */
            IF Order.OrderType = {&ORDER_TYPE_MNP} AND
               Order.MNPStatus <> ({&MNP_ST_APOR} + 1) THEN NEXT RequestLoop.
         END. /* IF AVAIL Order THEN DO: */
      END.
      ELSE IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_RENEWAL} THEN DO:

         RELEASE Order.

         IF MsRequest.OrigRequest > 0 THEN DO:

            find first bmsrequest NO-LOCK where
                       bmsrequest.msrequest = msrequest.origrequest and
                       bmsrequest.reqtype = 46 no-error.

            IF AVAIL bmsrequest then do:
               FIND FIRST order NO-LOCK where
                          order.brand = gcBrand and
                          order.orderid = bmsrequest.reqiparam1 NO-ERROR.
               IF AVAIL order AND
                  INDEX(Order.OrderChannel,"pos") = 0 THEN NEXT RequestLoop.
            END.
         END.
         /* This should not happen */
         IF NOT AVAIL Order THEN DO:
            FOR EACH Order NO-LOCK WHERE 
                     Order.MsSeq = MsRequest.MsSeq AND 
                     Order.OrderType EQ 2 AND 
                     LOOKUP(STRING(Order.StatusCode),
                            {&ORDER_CLOSE_STATUSES}) = 0 AND
                     Order.Crstamp <= MsRequest.Actstamp
               USE-INDEX MsSeq BY Order.CrStamp DESC:
               IF INDEX(Order.OrderChannel,"pos") = 0 THEN NEXT RequestLoop.
               LEAVE.
            END.
         END.
         lcRenewal = "R".
      END.
      ELSE IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER}
      THEN NEXT RequestLoop.
      
      fSplitTs(MsRequest.actstamp, output ldaActDate, output litime).

      FF_LOOP:
      FOR FIRST DCCLI NO-LOCK WHERE
                DCCLI.Brand   = gcBrand AND
                DCCLI.MsSeq   = MsRequest.MsSeq AND
                DCCLI.DCEvent = MsRequest.ReqCParam3 AND
                DCCLI.ValidFrom <= ldaActDate AND
                DCCLI.ValidTo >= ldaActDate,
          EACH FixedFee NO-LOCK USE-INDEX Custnum WHERE
               FixedFee.Brand    = gcBrand AND
               FixedFee.Custnum  = MsRequest.Custnum AND 
               FixedFee.HostTable = "MobSub" AND
               FixedFee.KeyValue  = STRING(MsRequest.MsSeq) AND
               FixedFee.FeeModel  = DayCampaign.FeeModel AND
               FixedFee.BegDate = DCCLI.ValidFrom AND
              (IF FixedFee.SourceTable EQ "DCCLI" THEN
                  FixedFee.SourceKey EQ STRING(DCCLI.PerContractID) ELSE TRUE)
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE RequestLoop.
      END.

      /* calculate these directly from items (not from FMItem.FFItemQty),
         to get the actualized quantity */
      liBatches = 0.
      FOR EACH FFItem OF FixedFee NO-LOCK:
         liBatches = liBatches + 1.
      END.

      /* due date is the 6. of the second month */
      IF MONTH(FixedFee.BegDate) = 12 THEN
         ldaDueDate = DATE(1,6,YEAR(FixedFee.BegDate) + 1).
      ELSE ldaDueDate = DATE(MONTH(FixedFee.BegDate) + 1,6,YEAR(FixedFee.BegDate)).

      llFinancedByBank = (LOOKUP(FixedFee.FinancedResult,
                          {&TF_STATUSES_BANK}) > 0).

      IF LOOKUp(FixedFee.FinancedResult,"B01,B02") > 0 then
         MESSAGE MsRequest.cli VIEW-AS ALERT-BOX.

      CREATE ttInstallment.
      ASSIGN
         ttInstallment.OperCode = (IF llFinancedByBank THEN "C" ELSE "A") 
         ttInstallment.Custnum = MsRequest.Custnum
         ttInstallment.MsSeq   = MsRequest.MsSeq
         ttInstallment.Amount  = FixedFee.Amt
         ttInstallment.Items   = liBatches
         ttInstallment.OperDate = DCCLI.ValidFrom
         ttInstallment.DueDate = ldaDueDate
         ttInstallment.Renewal = lcRenewal
         ttInstallment.BankCode = {&BANK_CODE} WHEN llFinancedByBank.

      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents LABEL "Requests"
         WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
            TITLE " Collecting " FRAME fQty.
      END.

      LEAVE FF_LOOP.

   END. 
   END.

END.

PROCEDURE pCollectACC:
   
   DEF VAR ldaDueDate    AS DATE NO-UNDO.
   DEF VAR ldCheck       AS DEC  NO-UNDO.
   DEF VAR ldFrom        AS DEC  NO-UNDO.
   DEF VAR ldTo          AS DEC  NO-UNDO.

   DEFINE VARIABLE ldaACCDate AS DATE NO-UNDO. 
   DEFINE VARIABLE liTime AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldeendtime AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE liBatches AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldeAmount AS DECIMAL NO-UNDO. 
   DEF BUFFER bmsowner FOR msowner.
   DEF VAR ldFeeEndDate AS DATE NO-UNDO.
   DEF VAR llFinancedByBank AS LOG NO-UNDO. 
   DEF VAR liFFItemQty AS INT NO-UNDO.

   /* check from last 20 days if there are ones that have been completed
      yesterday */
   IF icDumpMode = "modified" THEN ASSIGN
      ldFrom  = fMake2Dt(ldaFrom,0)
      ldTo    = fMake2Dt(ldaTo,86399)
      ldCheck = fMake2Dt(ldaFrom - 20,0).
   /* take all */
   ELSE ASSIGN
      ldFrom  = fMake2Dt(2/1/10,0)
      ldTo    = fMake2Dt(TODAY - 1,86399)
      ldCheck = ldFrom.

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
      IF NOT AVAIL msowner THEN NEXT.
      
      find bmsowner where
           bmsowner.msseq = msrequest.msseq and
           bmsowner.tsbegin = msrequest.actstamp NO-LOCK NO-ERROR.
      IF NOT AVAIL bmsowner THEN NEXT.

      /* YBU-1191 */
      if msowner.custnum = bmsowner.custnum then next.
      
      FOR EACH DCCLI NO-LOCK WHERE
               DCCLI.MsSeq = MsRequest.MsSeq AND
               DCCLI.DCEvent BEGINS "PAYTERM" AND
               DCCLI.ValidFrom < ldaACCDate AND
               DCCLI.ValidTo >= ldaACCDate,
          FIRST DayCampaign NO-LOCK WHERE
                DayCampaign.Brand = "1" AND
                DayCampaign.DCEvent = DCCLI.DCevent:
         
         /* old owner */
         FF_LOOP:
         FOR EACH FixedFee NO-LOCK USE-INDEX Custnum WHERE
                  FixedFee.Brand     = "1" AND
                  FIxedFee.Custnum   = msowner.custnum AND 
                  FixedFee.HostTable = "MobSub" AND
                  FixedFee.KeyValue  = STRING(MsRequest.MsSeq) AND
                  FixedFee.FeeModel  = DayCampaign.FeeModel AND
                  (IF FixedFee.SourceTable EQ "DCCLI" THEN 
                      FixedFee.SourceKey EQ STRING(DCCLI.PerContractID)
                   ELSE  TRUE):
            
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

           ASSIGN
              ldFeeEndDate = DATE(FixedFee.EndPeriod MOD 100,
                             1,
                             INT(FixedFee.EndPeriod / 100))
              ldFeeEndDate = fLastDayOfMonth(ldFeeEndDate)
              llFinancedByBank = (LOOKUP(FixedFee.FinancedResult,
                                  {&TF_STATUSES_BANK}) > 0).

           /* due date is the 6. of the second month */
            IF MONTH(ldFeeEndDate) = 12 THEN
               ldaDueDate = DATE(1,6,YEAR(ldFeeEndDate) + 1).
            ELSE ldaDueDate = DATE(MONTH(ldFeeEndDate) + 1,6,YEAR(ldFeeEndDate)).

           CREATE ttInstallment.
           ASSIGN
              ttInstallment.OperCode = (IF llFinancedByBank THEN "D" ELSE "B")
              ttInstallment.Custnum = msowner.Custnum
              ttInstallment.MsSeq   = msowner.MsSeq
              ttInstallment.Amount  = (IF llFinancedByBank THEN FixedFee.Amt ELSE ldeAmount)
              ttInstallment.Items   = liFFItemQty
              ttInstallment.OperDate = ldFeeEndDate 
              ttInstallment.DueDate = (IF llFinancedByBank THEN ldaDueDate ELSE ?)
              ttInstallment.BankCode = {&BANK_CODE} WHEN llFinancedByBank.
           
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
                  FIxedFee.Custnum = bmsowner.custnum AND 
                  FixedFee.HostTable = "MobSub" AND
                  FixedFee.KeyValue  = STRING(bmsowner.MsSeq) AND
                  FixedFee.FeeModel  = DayCampaign.FeeModel AND
                  FixedFee.BegDate = ldaACCDate AND
                  (IF FixedFee.SourceTable EQ "DCCLI" THEN 
                      FixedFee.SourceKey EQ STRING(DCCLI.PerContractID)
                   ELSE  TRUE):

            /* calculate these directly from items
              (not from FMItem.FFItemQty) to get the actualized quantity */
            liBatches = 0.
            FOR EACH FFItem OF FixedFee NO-LOCK:
               liBatches = liBatches + 1.
            END.
            
            /* due date is the 6. of the second month */
            IF MONTH(FixedFee.BegDate) = 12 THEN
               ldaDueDate = DATE(1,6,YEAR(FixedFee.BegDate) + 1).
            ELSE ldaDueDate = DATE(MONTH(FixedFee.BegDate) + 1,
                                   6,
                                   YEAR(FixedFee.BegDate)).
           
            llFinancedByBank = (LOOKUP(FixedFee.FinancedResult,
                               {&TF_STATUSES_BANK}) > 0).

            CREATE ttInstallment.
            ASSIGN
               ttInstallment.OperCode = (IF llFinancedByBank THEN "C" ELSE "A")
               ttInstallment.Custnum = bmsowner.Custnum
               ttInstallment.MsSeq   = MsRequest.MsSeq
               ttInstallment.Amount  = FixedFee.Amt
               ttInstallment.Items   = liBatches
               ttInstallment.OperDate = FixedFee.BegDate
               ttInstallment.DueDate = ldaDueDate
               ttInstallment.BankCode = {&BANK_CODE} WHEN llFinancedByBank.
         
            oiEvents = oiEvents + 1.
            IF NOT SESSION:BATCH AND oiEvents MOD 10 = 0 THEN DO:
               PAUSE 0.
               DISP oiEvents LABEL "Requests"
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                  TITLE " Collecting " FRAME fQty.
            END. 

            LEAVE FF_LOOP.
         END.
      END.
   END.

END PROCEDURE. 


PROCEDURE pCollectTerminations:

   DEF VAR ldaTo         AS DATE NO-UNDO.
   DEF VAR ldaFrom       AS DATE NO-UNDO.
   DEF VAR ldaActDate    AS DATE NO-UNDO.
   DEF VAR liTime        AS INT  NO-UNDO.
   DEF VAR ldDebt        AS DEC  NO-UNDO.
   DEF VAR ldCheck       AS DEC  NO-UNDO.
   DEF VAR ldFrom        AS DEC  NO-UNDO.
   DEF VAR ldTo          AS DEC  NO-UNDO.
   DEF VAR ldaDoneDate   AS DATE NO-UNDO.
   DEF VAR liDoneTime    AS INT  NO-UNDO.
   DEF VAR ldaRenewalDoneDate AS DATE NO-UNDO.
   DEF VAR liRenewalTime AS INT  NO-UNDO.
   DEF VAR llFinancedByBank AS LOG NO-UNDO. 
   DEF VAR ldFeeEndDate  AS DATE NO-UNDO.
   DEF VAR ldaDueDate    AS DATE NO-UNDO.
   DEF VAR liFFItemQty   AS INT  NO-UNDO.

   DEF BUFFER bOrigRequest   FOR MsRequest.
   DEF BUFFER bMsRequest     FOR MsRequest.
   DEF BUFFER bMainMsRequest FOR MsRequest.
   DEF BUFFER bSubMsRequest  FOR MsRequest.

   /* check from last 2 months if there are ones that have been completed
      during last month */
   IF icDumpMode = "modified" THEN ASSIGN
      ldaTo   = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
      ldaFrom = DATE(MONTH(ldaTo),1,YEAR(ldaTo))
      ldFrom  = fMake2Dt(ldaFrom,0)
      ldTo    = fMake2Dt(ldaTo,86399)
      ldCheck = fMake2Dt(ldaFrom - 20,0).
   /* take all */
   ELSE ASSIGN
      ldFrom  = fMake2Dt(2/1/10,0)
      ldTo    = fMake2Dt(TODAY - 1,86399)
      ldCheck = ldFrom.

   RequestLoop:
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand     = gcBrand AND
            MsRequest.ReqType   = 9       AND
            MsRequest.ReqStatus = 2       AND
            MsRequest.ActStamp >= ldCheck AND
            MsRequest.DoneStamp >= ldFrom AND
            MsRequest.DoneStamp <= ldTo,
      FIRST DayCampaign NO-LOCK USE-INDEX DCEvent WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = MsRequest.ReqCParam3 AND
            DayCampaign.DCType = {&DCTYPE_INSTALLMENT}
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      IF msrequest.ReqSource EQ 
         {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} THEN NEXT.

      if MsRequest.reqsource EQ "4" THEN NEXT.
      
      /* Exclude terminations originating from renewal orders */
      IF MsRequest.OrigRequest > 0 THEN DO:
         FIND FIRST bOrigRequest WHERE
                    bOrigRequest.MsRequest = MsRequest.OrigRequest
         NO-LOCK NO-ERROR.
         IF AVAIL bOrigRequest AND 
                  bOrigRequest.ReqType = {&REQTYPE_AFTER_SALES_ORDER}
         THEN NEXT.
      END.

      fSplitTS(MsRequest.DoneStamp,
               OUTPUT ldaDoneDate,
               OUTPUT liDoneTime).

      IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} THEN
         /* Find main Revert renewal order */
         FOR FIRST bMainMsRequest WHERE
                   bMainMsRequest.MsRequest = MsRequest.OrigRequest AND
                   bMainMsRequest.ReqType   = {&REQTYPE_REVERT_RENEWAL_ORDER} AND
                   bMainMsRequest.ReqStatus = {&REQUEST_STATUS_DONE} NO-LOCK,
             /* Find corresponding renewal order */
             FIRST bMsRequest WHERE
                   bMsRequest.MsSeq      = bMainMsRequest.MsSeq AND
                   bMsRequest.ReqType    = {&REQTYPE_AFTER_SALES_ORDER} AND
                   bMsRequest.ReqStatus  = {&REQUEST_STATUS_DONE} AND
                   bMsRequest.ReqIParam1 = bMainMsRequest.ReqIParam1 NO-LOCK,
             /* Find PAYTERM CONTRACT activation renewal order */
             FIRST bSubMsRequest WHERE
                   bSubMsRequest.OrigRequest = bMsRequest.MsRequest AND
                   bSubMsRequest.MsSeq       = bMsRequest.MsSeq AND
                   bSubMsRequest.ReqType     = 8 AND
                   bSubMsRequest.ReqStatus   = 2 AND
                   bSubMsRequest.ReqCparam3  = MsRequest.ReqCparam3 NO-LOCK:

            fSplitTS(bSubMsRequest.DoneStamp,
                     OUTPUT ldaRenewalDoneDate,
                     OUTPUT liRenewalTime).
            IF ldaRenewalDoneDate = ldaDoneDate THEN NEXT RequestLoop.
         END. /* FOR FIRST bMainMsRequest NO-LOCK WHERE */

      fSplitTS(MsRequest.ActStamp,
               OUTPUT ldaActDate,
               OUTPUT liTime).

      FIND FIRST DCCLI WHERE
                 DCCLI.MsSeq    = MsRequest.MsSeq AND
                 DCCLI.DCEvent  = MsRequest.ReqCParam3 AND
                 DCCLI.TermDate = ldaActDate NO-LOCK NO-ERROR.
      IF NOT AVAILABLE DCCLI THEN NEXT.

      /* rest of the unbilled fixed fees are converted into a single fee in
         termination */
      ASSIGN
         ldDebt = 0
         llFinancedByBank = FALSE
         liFFItemQty = 0.

      FIND FIRST SingleFee WHERE
                 SingleFee.Brand     = gcBrand AND
                 SingleFee.HostTable = "MobSub" AND
                 SingleFee.KeyValue  = STRING(MsRequest.MsSeq) AND
                 SingleFee.CalcObj   = "DC" + STRING(DCCLI.PerContractID)
      NO-LOCK NO-ERROR.
      IF AVAILABLE SingleFee THEN
         ldDebt = SingleFee.Amt.
         
      FOR FIRST FixedFee NO-LOCK WHERE
                FixedFee.Brand = gcBrand AND
                FixedFee.Custnum = MsRequest.Custnum AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue = STRING(MsRequest.MsSeq) AND
                FixedFee.BillCode = "PAYTERM" AND
                FixedFee.SourceTable = "DCCLI" AND
                FixedFee.SourceKey = STRING(DCCLI.PerContractID):

         ASSIGN
            liFFItemQty = ldDebt / FixedFee.Amt
            ldFeeEndDate = DATE(FixedFee.EndPeriod MOD 100,
                                1,
                                INT(FixedFee.EndPeriod / 100))
            ldFeeEndDate = fLastDayOfMonth(ldFeeEndDate)
            llFinancedByBank = (LOOKUP(FixedFee.FinancedResult,
                                {&TF_STATUSES_BANK}) > 0).

         /* due date is the 6. of the second month */
         IF MONTH(ldFeeEndDate) = 12 THEN
            ldaDueDate = DATE(1,6,YEAR(ldFeeEndDate) + 1).
         ELSE ldaDueDate = DATE(MONTH(ldFeeEndDate) + 1,6,YEAR(ldFeeEndDate)).

         CREATE ttInstallment.
         ASSIGN
            ttInstallment.OperCode = (IF llFinancedByBank THEN "D" ELSE "B")
            ttInstallment.Custnum = MsRequest.Custnum
            ttInstallment.MsSeq   = MsRequest.MsSeq
            ttInstallment.Amount  = (IF llFinancedByBank THEN FixedFee.Amt ELSE ldDebt)
            ttInstallment.Items   = liFFItemQty
            ttInstallment.OperDate = ldFeeEndDate
            ttInstallment.DueDate = (IF llFinancedByBank THEN ldaDueDate ELSE ?)
            ttInstallment.BankCode = {&BANK_CODE} WHEN llFinancedByBank.

         oiEvents = oiEvents + 1.
         IF NOT SESSION:BATCH AND oiEvents MOD 10 = 0 THEN DO:
            PAUSE 0.
            DISP oiEvents LABEL "Requests"
            WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                 TITLE " Collecting " FRAME fQty.
         END.
      END.
   END.

END PROCEDURE.


/******* MAIN end *********/
