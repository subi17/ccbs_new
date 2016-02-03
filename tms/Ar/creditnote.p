/* ----------------------------------------------------------------------------
  MODULE .......: creditnote.p
  FUNCTION .....: Create credit invoices from requests
  APPLICATION ..: TMS
  AUTHOR .......: aam (from nncimu)
  CREATED ......: 29.08.07
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Func/msreqfunc.i}

{Func/finvbal.i}
{Func/faccper.i}
{Func/fcustbal.i}
{Func/fcustcnt.i}
{Func/finvamt.i}
{Syst/eventval.i} 
{Func/fbankday.i}
{Ar/nnpcst.i}
{Func/finvnum.i}
{Func/fhdrtext.i}
{Func/frefundreq.i}
{Func/finvoiceacc.i}
{Func/fcreditvalid.i}
{Func/fparse.i}

DEFINE TEMP-TABLE ttSubInvoice
       FIELD SubInvoice        AS INT
       FIELD InvRow            AS INT
       FIELD InvRowAmtExlVAT   AS DEC
       FIELD InvRowVATAmt      AS DEC
       FIELD VATPerc           AS DEC
       INDEX SubInvRow IS PRIMARY UNIQUE SubInvoice InvRow.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}
END.

DEF BUFFER bSubRequest FOR MsRequest.


DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 22 THEN RETURN "ERROR".

/* Note: Idea is to use same procedure for both modes 
   otherwise lot of code will be duplicated */
CASE MsRequest.ReqCParam3:
WHEN "partial" THEN RUN pFullCreditNote.
OTHERWISE RUN pFullCreditNote.
END CASE.

/* eventlog temp-tables */
fCleanEventObjects().

RETURN RETURN-VALUE.


/* get next external invoice id */
FUNCTION fLocalNextExtID RETURNS CHARACTER
   (icSeqPrefix AS CHAR,
    icExtInvID  AS CHAR):

   DEF VAR liSeqInvNum AS INT NO-UNDO.
   
   /* remove prefix (don't use replace) */
   IF icSeqPrefix > "" AND icExtInvID BEGINS icSeqPrefix THEN DO:
      IF LENGTH(icExtInvID) > LENGTH(icSeqPrefix)
      THEN icExtInvID = SUBSTRING(icExtInvID,LENGTH(icSeqPrefix) + 1).
      ELSE icExtInvID = "".
   END.
         
   liSeqInvNum = INTEGER(icExtInvID) NO-ERROR.
         
   /* invalid integer value */
   IF ERROR-STATUS:ERROR THEN RETURN "".

   RETURN icSeqPrefix + STRING(liSeqInvNum + 1,"99999999").
   
END FUNCTION.


PROCEDURE pFullCreditNote:

   DEF VAR ldtCreditDate AS DATE NO-UNDO.
   DEF VAR ldCreditLimit AS DEC  NO-UNDO.
   DEF VAR liCalled      AS INT  NO-UNDO. 
   DEF VAR llUpdate      AS LOG  NO-UNDO. 
   DEF VAR lcCustName    AS CHAR NO-UNDO.
   DEF VAR lcExtInvID    AS CHAR NO-UNDO. 
   DEF VAR lcPrefix      AS CHAR NO-UNDO.
   DEF VAR liInvType     AS INT  NO-UNDO. 
   DEF VAR lcInvGroup    AS CHAR NO-UNDO. 
   DEF VAR ldActStamp    AS DEC  NO-UNDO. 
   DEF VAR lcError       AS CHAR NO-UNDO. 
   DEF VAR ldBalance     AS DEC  NO-UNDO.
   DEF VAR liAccNum      AS INT  NO-UNDO.
   DEF VAR ldTotRefund   AS DEC  NO-UNDO.
   DEF VAR ldRefundAmt   AS DEC  NO-UNDO. 
   DEF VAR liDDCancel    AS INT  NO-UNDO.
   DEF VAR ldRefundReq   AS DEC  NO-UNDO EXTENT 3. 
   DEF VAR liCreated     AS INT  NO-UNDO. 
   DEF VAR liInvRecid    AS INT  NO-UNDO.
   DEF VAR lcInvRowNumList        AS CHAR NO-UNDO.
   DEF VAR lcInvRowDetails        AS CHAR NO-UNDO.
   DEF VAR lcInvRowDetail         AS CHAR NO-UNDO.
   DEF VAR lcMode                 AS CHAR NO-UNDO.
   DEF VAR lcUserId               AS CHAR NO-UNDO.
   DEF VAR liCounter              AS INT  NO-UNDO. 
   DEF VAR liNumEntries           AS INT  NO-UNDO.
   DEF VAR liInvRowNum            AS INT  NO-UNDO.
   DEF VAR ldeSubInvAmtExclVAT    AS DEC  NO-UNDO.
   DEF VAR ldeSubInvVATAmt        AS DEC  NO-UNDO.
   DEF VAR ldeSubInvAmt           AS DEC  NO-UNDO.
   DEF VAR ldeInvRowAmt           AS DEC  NO-UNDO.
   DEF VAR ldeInvRowGrossAmt      AS DEC  NO-UNDO.
   DEF VAR llSubInvFullCredit     AS LOG  NO-UNDO.
   DEF VAR llOtherInvRowSpecified AS LOG  NO-UNDO.
   DEF VAR llCustomSubInv         AS LOG  NO-UNDO.
   
   DEF VAR liSubInvNum AS INT NO-UNDO.
   DEF VAR lcSubInvNums AS CHAR NO-UNDO. 
   DEF VAR ldeInterestAmtSum AS DEC NO-UNDO. 
   DEF VAR ldeAdvPaymSum AS DEC NO-UNDO. 
   DEF VAR ldeOverPaymSum AS DEC NO-UNDO. 
   DEF VAR ldeAmtExclVATSum AS DEC NO-UNDO. 
   DEF VAR ldeInvAmtSum AS DEC NO-UNDO. 
   DEF VAR ldeVATAmtSum AS DEC NO-UNDO. 
   DEF VAR ldeRoundingSum AS DEC NO-UNDO. 
   DEF VAR ldeVATAmountSum LIKE Invoice.VatAmount.
   DEF VAR ldeVATBasisSum LIKE Invoice.VatBasis.
   DEF VAR ldeSubVATBasis LIKE SubInvoice.VatBasis.
   DEF VAR ldeSubVATAmt   LIKE SubInvoice.VatAmount.
   DEF VAR ldeCurRate AS DEC NO-UNDO. 

   DEF BUFFER bCreditInv  FOR Invoice.
   DEF BUFFER bCreditSubInv FOR SubInvoice.
   DEF BUFFER bCreditRow  FOR InvRow.
   DEF BUFFER bCreditASub FOR InvASub.
   DEF BUFFER bCreditCCN  FOR InvCCN.
   DEF BUFFER bSubInvoice FOR SubInvoice.
   DEF BUFFER bInvRow     FOR InvRow.

   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhInvoice    AS HANDLE NO-UNDO.
      DEFINE VARIABLE lhbCreditInv AS HANDLE NO-UNDO.
      DEFINE VARIABLE lhbCreditSubInv AS HANDLE NO-UNDO.
   END.
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   FIND Invoice WHERE Invoice.InvNum = MsRequest.ReqIParam1 
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      
   IF LOCKED(Invoice) THEN DO:
      /* try again */
      fReqStatus(0,"").
      RETURN.
   END.
   
   IF NOT AVAILABLE Invoice THEN DO:
      fReqError("Invoice not found").
      RETURN.
   END.
   
   ASSIGN lcSubInvNums    = MsRequest.ReqCParam4
          lcInvRowDetails = MsRequest.ReqCParam5
          lcUserId        = MsRequest.UserCode.
   
   liNumEntries = NUM-ENTRIES(lcInvRowDetails).

   DO liCounter = 1 TO liNumEntries:
      lcInvRowDetail = ENTRY(liCounter, lcInvRowDetails).

      liInvRowNum = INT(fParseKVP("InvRow",lcInvRowDetail,"|")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         fReqError("Incorrect InvRow data format").
         RETURN.
      END.

      ldeInvRowAmt = DEC(fParseKVP("InvRowAmt",lcInvRowDetail,"|")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         fReqError("Incorrect InvRowAmt data format").
         RETURN.
      END.

      FIND FIRST InvRow WHERE
                 InvRow.InvNum    = Invoice.InvNum AND
                 InvRow.InvRowNum = liInvRowNum NO-LOCK NO-ERROR.
      IF NOT AVAIL InvRow OR
         LOOKUP(STRING(InvRow.SubInvNum), lcSubInvNums) = 0 THEN DO:
         fReqError("Invalid invrow number").
         RETURN.
      END.

      IF ldeInvRowAmt = ? THEN ldeInvRowAmt = InvRow.Amt.
      
      IF (InvRow.Amt > 0 AND ldeInvRowAmt < 0) OR
         (InvRow.Amt < 0 AND ldeInvRowAmt > 0) THEN DO:
          fReqError("Entered invrow amount and original " +
                    "invrow amount must be whether positive or " +
                    "negative").
          RETURN.
      END.

      IF InvRow.Amt < 0 THEN DO:
         IF abs(InvRow.Amt) < abs(ldeInvRowAmt) THEN DO:
            fReqError("Entered invrow amount is greater than actual invrow amount").
            RETURN.
         END. 
      END.
      ELSE DO:
         IF InvRow.Amt < ldeInvRowAmt THEN DO:
            fReqError("Entered invrow amount is greater than actual invrow amount").
             RETURN.
         END.
      END.
      /* Create Temp-table with SubInvoice/InvRow details */
      IF NOT CAN-FIND(FIRST ttSubInvoice WHERE
                            ttSubInvoice.SubInvoice = InvRow.SubInvNum AND
                            ttSubInvoice.InvRow     = InvRow.InvRowNum)
      THEN DO:
         CREATE ttSubInvoice.
         ASSIGN ttSubInvoice.SubInvoice      = InvRow.SubInvNum
                ttSubInvoice.InvRow          = InvRow.InvRowNum
                ttSubInvoice.InvRowAmtExlVAT = ldeInvRowAmt
                ttSubInvoice.VATPerc         = InvRow.VATPerc
                ttSubInvoice.InvRowVATAmt    =
                ROUND(((ttSubInvoice.InvRowAmtExlVAT * InvRow.VATPerc) / 100),2).
      END. /* IF NOT CAN-FIND(FIRST ttSubInvoice WHERE */

      lcInvRowNumList = lcInvRowNumList + "," + STRING(InvRow.InvRowNum).
   END. /* DO liCounter = 1 TO liNumEntries: */

   lcInvRowNumList = TRIM(lcInvRowNumList,",").

   lcError = fCheckInvoice(BUFFER Invoice,
                           lcSubInvNums,
                           lcInvRowNumList,
                           OUTPUT lcMode).
   IF lcError NE "" THEN DO:
      fReqError(lcError).
      RETURN.
   END.

   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldtCreditDate,
            OUTPUT liReqCnt).
            
   /* never date invoices to past */
   ldtCreditDate = MAX(ldtCreditDate,TODAY).
 
   /* check period */
   IF fPeriodLocked(ldtCreditDate,FALSE) THEN DO:
      fReqError("Period for " + STRING(ldtCreditDate,"99.99.99") + 
                " is locked.").
      RETURN.
   END.

   /* current payment status */
   ASSIGN 
      ldBalance   = fInvBal(BUFFER Invoice,TODAY)
      ldTotRefund = 0.
 
   IF ldBalance NE Invoice.InvAmt THEN DO:

      IF ldBalance < 0 THEN DO:
         fReqError("Invoice has a negative balance").
         RETURN.
      END.
      
      IF ldBalance > Invoice.InvAmt THEN DO:
         fReqError("Open balance is more than original amount").
         RETURN. 
      END.
      
   END.
   
      
   /* make a refund payment (can also be made for an unpaid invoice) 
      1=adv.payment, 2=refund payment */
   IF (MsRequest.ReqIParam4 = 1 OR MsRequest.ReqIParam4 = 2) THEN DO:
      
      liDDCancel = fCParamI("DDCancelDays").
      IF liDDCancel = ? THEN liDDCancel = 0.
      
      /* don't activate before cancellation time after due date has passed */
      ldActStamp = fMake2DT(MAX(TODAY,Invoice.DueDate + liDDCancel + 1),
                            100).
          
      /* payment for dd-invoices will be done on due date */
      IF Invoice.ChargeType = 2 AND Invoice.DueDate >= TODAY AND
         ldBalance = Invoice.InvAmt 
      THEN DO:
         fReqError("Refund not possible: Invoice has not been paid yet").
         RETURN.
      END.
         
      FOR EACH Payment OF Invoice NO-LOCK:
         
         ldRefundReq = 0.
         DO liReqCnt = 1 TO 10:
            /* money and balance payments */
            CASE Payment.AccType[liReqCnt]:
            /* money */
            WHEN 4  THEN ldRefundReq[1] = ldRefundReq[1] + 
                                          Payment.Posting[liReqCnt].
            /* overpayment */
            WHEN 6  THEN ldRefundReq[2] = ldRefundReq[2] + 
                                          Payment.Posting[liReqCnt].
            /* advance payment */
            WHEN 19 THEN ldRefundReq[3] = ldRefundReq[3] + 
                                          Payment.Posting[liReqCnt].
            END CASE. 
         END.
      END.
            
      DO liReqCnt = 1 TO 3:
            
         ldRefundReq[liReqCnt] = MIN(ldRefundReq[liReqCnt],
                                     Invoice.InvAmt - ldBalance - ldTotRefund).
            
         IF ldRefundReq[liReqCnt] <= 0 THEN NEXT.
            
         liCreated = fRefundRequest(MsRequest.CustNum,
                                    MsRequest.ReqIParam1,    /* invoice */
                                    0,
                                    MsRequest.ReqIParam4,
                                    ldRefundReq[liReqCnt], /* amount */
                                    liReqCnt,              /* type of amount */
                                    Customer.BankAcc,
                                    1,
                                    MsRequest.ReqCParam1,
                                    ldActStamp,
                                    FALSE,
                                    "",
                                    OUTPUT lcReqChar).
                                 
         IF liCreated > 0 THEN DO:

            /* link main request to subrequest */
            FIND bSubRequest WHERE bSubRequest.MsRequest = liCreated
               EXCLUSIVE-LOCK.
            ASSIGN 
               bSubRequest.OrigRequest = MsRequest.MsRequest
               bSubRequest.Mandatory   = 0.
            RELEASE bSubRequest.      
 
            ldTotRefund = ldTotRefund + ldRefundReq[liReqCnt].
                                     
            /* move the amount to be refunded into customer's 
               refund balance */
            liAccNum = fCParamI("RefundBalAcc").

            CASE liReqCnt:
            WHEN 1 THEN lcReqChar = "P".
            WHEN 2 THEN lcReqChar = "Overp".
            WHEN 3 THEN lcReqChar = "Advance p".
            END CASE. 
            
            RUN makepaym (BUFFER Invoice,
                          -1 * ldTotRefund,
                          TODAY,
                          liAccNum,
                          "CRRF",
                          0,
                          FALSE,
                          FALSE,
                          "R" + STRING(MsRequest.MsRequest),
                          lcReqChar + 
                             "ayment moved to refund balance when creating " +
                             "a credit note",
                          OUTPUT liReqCnt).
         END.
         
      END.
         
   END.

   /* credit invoice type according to original invoice */
   CASE Invoice.InvType:
   WHEN 6  THEN liInvType = 8.
   WHEN 7  THEN liInvType = 9.
   WHEN 4  THEN liInvType = 10.
   WHEN 3  THEN liInvType = 11.
   WHEN 12 THEN liInvType = 13.
   OTHERWISE    liInvType = 5.
   END CASE. 

   lcInvGroup = Customer.InvGroup.
   /* get taxzone from original invoice and invgroup through that */
   IF Invoice.TaxZone > "" THEN
   FOR FIRST InvGroup NO-LOCK WHERE
             InvGroup.Brand   = "1" AND
             InvGroup.TaxZone = Invoice.TaxZone:
      lcInvGroup = InvGroup.InvGroup.       
   END.

   liInvRecid = RECID(Invoice).
   
   /* external invoice id */   
   lcExtInvID = fGetInvNum(lcInvGroup,
                           liInvType,
                           ldtCreditDate,
                           OUTPUT lcPrefix).

   IF lcExtInvID = "" THEN DO:
      fReqError("Invoice sequence is invalid").
      RETURN.
   END.
         
   ExtInvNum:
   REPEAT:
      /* make sure that field values are visible to other sessions */
      FIND Invoice WHERE RECID(Invoice) = liInvRecid EXCLUSIVE-LOCK.
 
      /* check IF invoice number is already in use */
      IF NOT can-find(FIRST Invoice where
                            Invoice.Brand    = gcBrand AND 
                            Invoice.ExtInvID = lcExtInvID AND
                            RECID(Invoice) NE liInvRecid) 
      THEN LEAVE ExtInvNum.
         
      /* get next-value */
      lcExtInvID = fLocalNextExtID(lcPrefix,
                                   lcExtInvID).
   END.

   CREATE bCreditInv.
   bCreditInv.ExtInvID = lcExtInvID.
      
   BUFFER-COPY Invoice EXCEPT InvNum ExtInvID TO bCreditInv.
   bCreditInv.ChgStamp = fMakeTS().
 
   InvNum:
   REPEAT:

      bCreditInv.InvNum = NEXT-VALUE(IntInvNum) NO-ERROR.
         
      VALIDATE bCreditInv NO-ERROR.
                  
      /* another process has just used the same number */
      IF ERROR-STATUS:ERROR OR bCreditInv.InvNum = 0 THEN NEXT.

      LEAVE.
   END.

   ASSIGN
      bCreditInv.InvDate      = ldtCreditDate
      bCreditInv.DueDate      = ldtCreditDate
      bCreditInv.ClaimState   = 0
      bCreditInv.ClaimStatus  = ""
      bCreditInv.WInvDisp     = FALSE
      bCreditInv.ExpStamp     = 0
      bCreditInv.PaidAmt      = 0
      bCreditInv.CrInvNum     = Invoice.InvNum
      bCreditInv.DDState      = 0
      bCreditInv.PrintState   = 0
      bCreditInv.InvType      = liInvType
      bCreditInv.InvCfg[1]    = IF Invoice.PrintState = 0 
                                THEN TRUE
                                ELSE Invoice.InvCfg[1]
      bCreditInv.WInvDisp     = IF Invoice.PrintState = 0 
                                THEN FALSE
                                ELSE Invoice.WInvDisp
      bCreditInv.CreditReason = MsRequest.ReqCParam1
      bCreditInv.xxmemo[1]    = MsRequest.ReqCParam2  /* Credit Reason Note */
      bCreditInv.DeliveryState = 1.
                              
   /* update last used invoice number */
   llUpdate = NOT fUpdateInvNum(lcInvGroup,
                                liInvType,
                                ldtCreditDate,
                                lcExtInvID).
   
   FOR EACH SubInvoice OF Invoice NO-LOCK:
      ASSIGN llSubInvFullCredit  = TRUE
             llCustomSubInv      = FALSE
             ldeSubInvAmtExclVAT = 0
             ldeSubInvVATAmt     = 0
             ldeSubInvAmt        = 0
             liReqCnt            = 0
             ldeSubVATBasis      = 0
             ldeSubVATAmt        = 0.

      IF lcSubInvNums > "" THEN DO:
         IF LOOKUP(STRING(SubInvoice.SubInvNum),lcSubInvNums) = 0 THEN NEXT.

         IF lcInvRowDetails > "" THEN DO:
            FOR EACH ttSubInvoice NO-LOCK WHERE
                     ttSubInvoice.SubInvoice = SubInvoice.SubInvNum
                BREAK BY ttSubInvoice.VATPerc:
                IF FIRST-OF(ttSubInvoice.VATPerc) THEN
                  liReqCnt = liReqCnt + 1.

                ASSIGN ldeSubInvAmtExclVAT      = ldeSubInvAmtExclVAT +
                                                  ttSubInvoice.InvRowAmtExlVAT
                       ldeSubVATBasis[liReqCnt] = ldeSubVATBasis[liReqCnt] +
                                                  ttSubInvoice.InvRowAmtExlVAT
                       ldeSubVATAmt[liReqCnt]   = ldeSubVATAmt[liReqCnt] +
                                                  ttSubInvoice.InvRowVATAmt
                       ldeSubInvVATAmt          = ldeSubInvVATAmt +
                                                  ttSubInvoice.InvRowVATAmt
                       llSubInvFullCredit       = FALSE
                       llCustomSubInv           = TRUE.
            END. /* FOR EACH ttSubInvoice NO-LOCK WHERE */
            ldeSubInvAmt = (ldeSubInvAmtExclVAT + ldeSubInvVATAmt).
         END. /* IF lcInvRowDetails > "" THEN DO: */

         /* If SubInvoice is already credited but few InvRow still not */
         /* credited, so make credit note for rest                     */
         IF SubInvoice.CrInvNum > 0 AND NOT llCustomSubInv THEN DO:
            FOR EACH bInvRow WHERE
                     bInvRow.InvNum    = Invoice.InvNum AND
                     bInvRow.SubInvNum = SubInvoice.SubInvNum NO-LOCK
                BREAK BY bInvRow.VATPerc:

               IF FIRST-OF(bInvRow.VATPerc) THEN
                  liReqCnt = liReqCnt + 1.
               IF bInvRow.CreditInvNum > 0 THEN NEXT.

               ASSIGN ldeSubInvAmtExclVAT = ldeSubInvAmtExclVAT + bInvRow.Amt
                      ldeSubInvVATAmt     = ldeSubInvVATAmt +
                      ROUND(((bInvRow.Amt * bInvRow.VATPerc) / 100),2)
                      ldeSubVATBasis[liReqCnt] = ldeSubVATBasis[liReqCnt] + bInvRow.Amt
                      ldeSubVATAmt[liReqCnt]   = ldeSubVATAmt[liReqCnt] +
                      ROUND(((bInvRow.Amt * bInvRow.VATPerc) / 100),2)
                      llSubInvFullCredit  = FALSE.
            END. /* FOR EACH bInvRow WHERE */
            ldeSubInvAmt = (ldeSubInvAmtExclVAT + ldeSubInvVATAmt).
         END. /* IF SubInvoice.CrInvNum > 0 THEN DO: */
      END. /* IF lcSubInvNums > "" THEN DO: */

      IF llSubInvFullCredit THEN
         ASSIGN ldeSubInvAmtExclVAT = SubInvoice.AmtExclVAT
                ldeSubInvVATAmt     = SubInvoice.VATAmt
                ldeSubInvAmt        = SubInvoice.InvAmt.
      
      CREATE bCreditSubInv. 
      bCreditSubInv.InvNum = bCreditInv.InvNum.

      BUFFER-COPY SubInvoice EXCEPT InvNum TO bCreditSubInv.
   
      ASSIGN 
         bCreditSubInv.InterestAmt = 0 - SubInvoice.InterestAmt
         bCreditSubInv.AdvPaym     = 0 - SubInvoice.AdvPaym
         bCreditSubInv.OverPaym    = 0 - SubInvoice.OverPaym
         bCreditSubInv.AmtExclVAT  = 0 - ldeSubInvAmtExclVAT
         bCreditSubInv.InvAmt      = 0 - ldeSubInvAmt
         bCreditSubInv.VATAmt      = 0 - ldeSubInvVATAmt
         bCreditSubInv.Rounding    = 0 - SubInvoice.Rounding
         bCreditSubInv.PaidAmt     = 0
         bCreditSubInv.ClaimState  = 0.

      ASSIGN
         ldeInterestAmtSum = ldeInterestAmtSum + SubInvoice.InterestAmt
         ldeAdvPaymSum = ldeAdvPaymSum + SubInvoice.AdvPaym
         ldeOverPaymSum = ldeOverPaymSum + SubInvoice.OverPaym
         ldeAmtExclVATSum = ldeAmtExclVATSum + ldeSubInvAmtExclVAT
         ldeInvAmtSum = ldeInvAmtSum + ldeSubInvAmt
         ldeVATAmtSum = ldeVATAmtSum + ldeSubInvVATAmt
         ldeRoundingSum = ldeRoundingSum + SubInvoice.Rounding.
      
      DO liReqCnt = 1 TO 10:

         IF NOT llSubInvFullCredit THEN
            ASSIGN bCreditSubInv.VatAmount[liReqCnt] = -1 * ldeSubVATAmt[liReqCnt]
                   bCreditSubInv.VatBasis[liReqCnt]  = -1 * ldeSubVATBasis[liReqCnt].
         ELSE
            ASSIGN bCreditSubInv.VatAmount[liReqCnt] = -1 * SubInvoice.VatAmount[liReqCnt]
                   bCreditSubInv.VatBasis[liReqCnt]  = -1 * SubInvoice.VatBasis[liReqCnt].

         ldeVATAmountSum[liReqCnt] = ldeVATAmountSum[liReqCnt] +
                                     bCreditSubInv.VatAmount[liReqCnt].
         ldeVATBasisSum[liReqCnt] = ldeVATBasisSum[liReqCnt] +
                                    bCreditSubInv.VatBasis[liReqCnt].
      END.
   
      IF llDoEvent THEN DO:
         lhbCreditSubInv = BUFFER bCreditSubInv:HANDLE.
         RUN StarEventInitialize(lhbCreditSubInv).
         RUN StarEventMakeCreateEvent(lhbCreditSubInv).
      END.

   END.

   ldeCurRate = fCurrRate(Customer.Currency,Invoice.InvDate).

   ASSIGN 
      bCreditInv.InterestAmt = 0 - ldeInterestAmtSum
      bCreditInv.AdvPaym     = 0 - ldeAdvPaymSum
      bCreditInv.OverPaym    = 0 - ldeOverPaymSum
      bCreditInv.CurrAmt     = 0 - fToHomeCurr(ldeInvAmtSum,ldeCurRate)
      bCreditInv.AmtExclVAT  = 0 - ldeAmtExclVATSum
      bCreditInv.InvAmt      = 0 - ldeInvAmtSum
      bCreditInv.VATAmt      = 0 - ldeVATAmtSum
      bCreditInv.Rounding    = 0 - ldeRoundingSum.

   DO liReqCnt = 1 TO 10:
      bCreditInv.VatAmount[liReqCnt] = ldeVatAmountSum[liReqCnt].
      bCreditInv.VatBasis[liReqCnt]  = ldeVatBasisSum[liReqCnt].
   END.

   /* advance payments  */
   IF bCreditInv.AdvPaym NE 0 then do: 

      fCustBal(Customer.CustNum,
               "",  
               "AP",
               bCreditInv.AdvPaym). 

      CREATE OPLog.
      ASSIGN
         OPLog.CreStamp  = fMakeTS()
         OPLog.CustNum   = Customer.CustNum
         OPLog.EventDate = TODAY
         OPLog.UserCode  = katun
         OPLog.EventType = 11 /* Credit invoice (deduction also credited) */
         OPLog.InvNum    = Invoice.InvNum
         /* OPLog.Voucher */
         OPLog.Amt       = bCreditInv.AdvPaym. /* Advance payments */
   END.
                   
   /* overpayments  */
   IF bCreditInv.OverPaym NE 0 THEN DO: 

      fCustBal(Customer.CustNum,
               "",
               "OP",
               bCreditInv.OverPaym). 

      /* Make a OPLog record */
      CREATE OPLog.
      ASSIGN
         OPLog.CreStamp  = fMakeTS()
         OPLog.CustNum   = Customer.CustNum
         OPLog.EventDate = TODAY
         OPLog.UserCode  = katun
         OPLog.EventType = 4 /* Credit invoice (deduction also credited) */
         OPLog.InvNum    = Invoice.InvNum
         /* OPLog.Voucher */
         OPLog.Amt       = bCreditInv.OverPaym. /* overpayments */
   END.

   IF llDoEvent THEN DO:
      lhInvoice = BUFFER Invoice:HANDLE.
      RUN StarEventInitialize(lhInvoice).
 
      lhbCreditInv = BUFFER bCreditInv:HANDLE.
      RUN StarEventInitialize(lhbCreditInv).
   END.
 
   IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhbCreditInv).

   FOR EACH InvRow of Invoice WHERE InvRow.CreditInvNum = 0:

      IF lcSubInvNums > "" AND
         LOOKUP(STRING(InvRow.SubInvNum),lcSubInvNums) = 0 THEN NEXT.

      /* Note: 1. Credit InvRow(s) if it's specifically mentioned          */
      /* 2. Credit all InvRow,if not mentioned but SubInvoice is mentioned */
      ASSIGN llOtherInvRowSpecified = FALSE
             ldeInvRowAmt       = InvRow.Amt
             ldeInvRowGrossAmt  = InvRow.GrossAmt.

      IF lcInvRowNumList > "" THEN DO:
         IF LOOKUP(STRING(InvRow.InvRowNum),lcInvRowNumList) = 0 THEN DO:
            FOR EACH  ttSubInvoice WHERE
                      ttSubInvoice.SubInvoice = InvRow.SubInvNum NO-LOCK,
                FIRST bSubInvoice WHERE
                      bSubInvoice.InvNum    = Invoice.InvNum AND
                      bSubInvoice.SubInvNum = ttSubInvoice.SubInvoice NO-LOCK,
                FIRST bInvRow WHERE
                      bInvRow.Invnum = bSubInvoice.Invnum AND
                      bInvRow.SubInvNum = bSubInvoice.SubInvNum AND
                      bInvRow.InvRowNum = ttSubInvoice.InvRow NO-LOCK:
                llOtherInvRowSpecified = TRUE.
                LEAVE.
            END. /* FOR EACH  ttSubInvoice NO-LOCK, */
            IF llOtherInvRowSpecified THEN NEXT.
         END. /* IF LOOKUP(STRING(InvRow.InvRowNum),lcInvRowNumList) */
         ELSE DO:
            FIND FIRST ttSubInvoice NO-LOCK WHERE
                       ttSubInvoice.SubInvoice = InvRow.SubInvNum AND
                       ttSubInvoice.InvRow     = InvRow.InvRowNum NO-ERROR.
            IF AVAILABLE ttSubInvoice THEN
               ASSIGN ldeInvRowAmt      = ttSubInvoice.InvRowAmtExlVAT
                      ldeInvRowGrossAmt = ttSubInvoice.InvRowAmtExlVAT.
         END. /* ELSE DO: */
      END. /* IF lcInvRowNumList > "" AND */

      CREATE bCreditRow.
      BUFFER-COPY InvRow TO bCreditRow.
      
      ASSIGN
         bCreditRow.InvNum         = bCreditInv.InvNum
         bCreditRow.InvRow         = NEXT-VALUE(irid)
         /* minutes and qty are not quite true if partial credit and row
            amount has been changed, but there is no way of knowing what
            the real amounts are then */
         bCreditRow.Minutes        = 0 - bCreditRow.Minutes
         bCreditRow.Qty            = 0 - bCreditRow.Qty
         /* mark the credit nbr TO the credited line, 
            important in partial crediting */
         InvRow.CreditInvNum       = bCreditInv.InvNum 
         bCreditRow.GrossAmt       = -1 * ldeInvRowGrossAmt
         bCreditRow.Amt            = -1 * ldeInvRowAmt.
         
      /* in yoigo newest accounts are always used */
      FOR FIRST BillItem NO-LOCK WHERE
                BillItem.Brand = gcBrand AND
                BillItem.BillCode = bCreditRow.BillCode:
         bCreditRow.SlsAcc = fInvRowAccount(Customer.Category,
                                            bCreditInv.VatUsage).
      END.
   END.

   /* credit invoice memo */
   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = fMakeTS()
      Memo.MemoSeq   = next-value(MemoSeq)
      Memo.Brand     = gcBrand
      Memo.MemoTitle = "Credited"
      Memo.CreUser   = lcUserId
      Memo.HostTable = "Invoice"
      Memo.KeyValue  = STRING(bCreditInv.InvNum)
      Memo.CustNum   = Invoice.CustNum.
      Memo.Memotext  = fGetHdrText(50,Customer.Language) + " " +
                       STRING(Invoice.InvNum) +  
                       ". Handler: " + katun  +
                       ". Reason Note/Jira : " + bCreditInv.xxmemo[1].

   /* customer balances */
   fCustBal(Customer.CustNum,
            "",  
            "ARBAL",
            bCreditInv.InvAmt).

   fCustCount(Customer.CustNum,
              "UB",
               -1 * bCreditInv.AmtExclVAT). 

   /* update original (debit) invoice */
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

   ASSIGN 
      Invoice.CrInvNum     = bCreditInv.InvNum 
      /* totally credited, unprinted invoices will not be shown in web */
      Invoice.WInvDisp     = IF Invoice.PrintState = 0 
                             THEN FALSE
                             ELSE Invoice.WInvDisp
      Invoice.CreditReason = MsRequest.ReqCParam1
      Invoice.xxmemo[1]    = MsRequest.ReqCParam2.  /* Credit Reason Note */
                           

   FOR EACH SubInvoice OF Invoice EXCLUSIVE-LOCK WHERE 
            SubInvoice.CrInvNum = 0:
      
      IF NUM-ENTRIES(lcSubInvNums) > 0 AND
         LOOKUP(STRING(SubInvoice.SubInvNum), lcSubInvNums) = 0 THEN NEXT.

      SubInvoice.CrInvNum = Invoice.CrInvNum.
   END.
   
   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = fMakeTS()
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.Brand     = gcBrand
      Memo.MemoTitle = "Credited"
      Memo.CreUser   = lcUserId
      Memo.HostTable = "Invoice"
      Memo.KeyValue  = STRING(Invoice.InvNum)
      Memo.CustNum   = Invoice.CustNum
      Memo.Memotext  = "Credited by " + katun +
                       ". Reason Note/Jira: " + Invoice.xxmemo[1].

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice). 

   /* release events */
   IF MsRequest.ReqIParam2 = 1 THEN DO:
         
      /* copy counters, so that new billing doesn't double these postings */
      FOR EACH InvASub NO-LOCK WHERE
               InvASub.InvNum = Invoice.InvNum:

         IF NUM-ENTRIES(lcSubInvNums) > 0 AND
            LOOKUP(STRING(InvASub.SubInvNum), lcSubInvNums) = 0 THEN NEXT.
         
         CREATE bCreditASub.
         BUFFER-COPY InvASub EXCEPT InvNum TO bCreditASub.
         ASSIGN bCreditASub.InvNum   = bCreditInv.InvNum
                bCreditASub.Qty      = bCreditASub.Qty * -1
                bCreditASub.Amt      = bCreditASub.Amt * -1
                bCreditASub.Min      = bCreditASub.Min * -1
                bCreditASub.GenPrice = bCreditASub.GenPrice * -1
                bCreditASub.MPMAmt   = bCreditASub.MPMAmt * -1
                bCreditASub.DataAmt  = bCreditASub.DataAmt * -1.
      END.
 
      FOR EACH InvCCN NO-LOCK WHERE
               InvCCN.InvNum = Invoice.InvNum:
         
         IF NUM-ENTRIES(lcSubInvNums) > 0 AND
            LOOKUP(STRING(InvCCN.SubInvNum), lcSubInvNums) = 0 THEN NEXT.
         
         CREATE bCreditCCN.
         BUFFER-COPY InvCCN EXCEPT InvNum TO bCreditCCN.
         ASSIGN bCreditCCN.InvNum   = bCreditInv.InvNum
                bCreditCCN.Qty      = bCreditCCN.Qty * -1
                bCreditCCN.Amt      = bCreditCCN.Amt * -1
                bCreditCCN.Min      = bCreditCCN.Min * -1
                bCreditCCN.GenPrice = bCreditCCN.GenPrice * -1
                bCreditCCN.DataAmt  = bCreditCCN.DataAmt * -1.
      END.
          
      IF NUM-ENTRIES(lcSubInvNums) = 0 THEN 
         RUN nnpcst.p(Invoice.InvNum,
                    0,
                    TRUE,
                    INPUT TABLE wMarked).
      ELSE DO:
         
         FOR EACH SubInvoice OF Invoice NO-LOCK:
            
            IF LOOKUP(STRING(InvCCN.SubInvNum), lcSubInvNums) = 0 THEN NEXT.
            
            RUN nnpcst.p(Invoice.InvNum,
                       SubInvoice.SubInvNum,
                       TRUE,
                       INPUT TABLE wMarked).
         END.
      END.
   END.

   /* if used nbr could not be updated earlier then try it once more */
   IF NOT llUpdate THEN DO:
      fUpdateInvNum(lcInvGroup,
                    liInvType,
                    ldtCreditDate,  
                    lcExtInvId).
   END.

   FIND CURRENT MsRequest EXCLUSIVE-LOCK.
   MsRequest.ReqIParam3 = bCreditInv.InvNum.
   
   RELEASE bCreditInv.
   RELEASE Invoice.   

   /* customer's invoicing Period */
   FIND FIRST Invoice WHERE
              Invoice.Brand    = gcBrand          AND
              Invoice.CustNum  = Customer.CustNum AND
              Invoice.CrInvNum = 0                AND
              Invoice.InvAmt > 0 NO-LOCK NO-ERROR.

   IF AVAIL Invoice THEN
      fLatestInv(Customer.CustNum,
                 "",
                 Invoice.InvDate).

   ELSE fCustBal(Customer.CustNum,               
                 "",
                 "INVP",
                 0.00). 

   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.
