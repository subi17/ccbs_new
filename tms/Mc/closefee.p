/* ---------------------------------------------------------------------------
  MODULE .......: CLOSEFEE
  FUNCTION .....: close fixed fees; create a broken rental item for last 
                  period and/or credit already (beforehand) billed items
  APPLICATION ..: TMS
  CREATED ......: 14.09.05/aam 
  MODIFIED .....: 21.06.07 kl yoigo version (no end date on fixed fee)

  VERSION ......: M15
  -------------------------------------------------------------------------- */

{commali.i}
{coinv.i}
{eventval.i}
{bundle_first_month_fee.i}
{tmsconst.i}

DEF INPUT  PARAMETER iiFFNum     AS INT  NO-UNDO.
DEF INPUT  PARAMETER idtEnd      AS DATE NO-UNDO.  /* end date */
DEF INPUT  PARAMETER ilCredit    AS LOG  NO-UNDO.  /* credit billed items   */
DEF INPUT  PARAMETER ilDiscount  AS LOG  NO-UNDO.  /* close discount items  */
DEF INPUT  PARAMETER iiMsSeq     AS INT  NO-UNDO.
DEF INPUT  PARAMETER icGroupCode AS CHAR NO-UNDO.  /* bundle ID */
DEF INPUT  PARAMETER icUserCode  AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icFeeMemo   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiMsRequest AS INT  NO-UNDO.
DEF INPUT  PARAMETER ilFMFee     AS LOG  NO-UNDO.  /* Full monthly fee   */
DEF OUTPUT PARAMETER odBilled    AS DEC  NO-UNDO.  /* billed after end date */

DEF BUFFER bCloseFee  FOR FixedFee.
DEF BUFFER bCloseItem FOR FFItem.
DEF BUFFER bFFItem    FOR FFItem.
DEF BUFFER bCredFee   FOR FixedFee.
DEF BUFFER bMsrequest FOR MsRequest.

DEF VAR liLongEnd   AS INT  NO-UNDO.
DEF VAR liShortEnd  AS INT  NO-UNDO.
DEF VAR liEnd       AS INT  NO-UNDO.
DEF VAR ldtItemBeg  AS DATE NO-UNDO.
DEF VAR ldtItemEnd  AS DATE NO-UNDO.
DEF VAR ldBRAmt     AS DEC  NO-UNDO.
DEF VAR ldCredAmt   AS DEC  NO-UNDO.
DEF VAR liLastCred  AS INT  NO-UNDO.
DEF VAR liBillTarg  AS INT  NO-UNDO.
DEF VAR liBrokenRental AS INT  NO-UNDO.
DEF VAR ldeOriginalFee AS DEC  NO-UNDO.
DEF VAR liPeriodLength AS INT  NO-UNDO.
DEF VAR liMonth        AS INT  NO-UNDO.
DEF VAR ldaDate        AS DATE NO-UNDO.
DEF VAR ldaOrderdate AS DATE NO-UNDO. 
DEF VAR liTime AS INT NO-UNDO. 
DEF VAR lcReason AS CHAR NO-UNDO. 
DEF VAR llUpdateTFStatus AS LOG NO-UNDO. 
DEF VAR llBankFinance AS LOG NO-UNDO. 
  
FIND bCloseFee WHERE bCloseFee.FFNum = iiFFNum EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE bCloseFee THEN RETURN "Unknown fee".

IF bCloseFee.InUse = FALSE THEN RETURN "Fee is not used".

/* already handled here */
IF bCloseFee.CustPP > 0 THEN RETURN "Already handled".


FIND Customer WHERE Customer.CustNum = bCloseFee.CustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN "Unknown customer".


IF llDoEvent THEN DO:

   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}

   DEFINE VARIABLE lhFixedFee AS HANDLE NO-UNDO.
   lhFixedFee = BUFFER bCloseFee:HANDLE.
   RUN StarEventInitialize(lhFixedFee).

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).
   
END.


/* check from feemodel if broken rental is used or not 
   pricelist is not checked -> assume that all use the same method */
IF bCloseFee.FeeModel > "" THEN 
FOR FIRST FMItem NO-LOCK WHERE
          FMItem.Brand      = bCloseFee.Brand    AND
          FMItem.FeeModel   = bCloseFee.FeeModel AND
          FMItem.BillCode   = bCloseFee.BillCode AND
          FMItem.BillMethod = FALSE:

   ASSIGN ldeOriginalFee = FMItem.Amount
          liBrokenRental = FMItem.BrokenRental.

   IF FMItem.BrokenRental = 1 AND icGroupCode = "" THEN DO:
      /* move to end of month */
      IF MONTH(idtEnd) = 12 
      THEN idtEnd = DATE(12,31,YEAR(idtEnd)).
      ELSE idtEnd = DATE(MONTH(idtEnd) + 1,1,YEAR(idtEnd)) - 1.
   END. 
END.

ASSIGN liLongEnd  = YEAR(idtEnd) * 10000 + 
                    MONTH(idtEnd) * 100  +
                    DAY(idtEnd)
       liShortEnd = YEAR(idtEnd) * 100 +
                    MONTH(idtEnd).

 
FOR EACH bCloseItem OF bCloseFee EXCLUSIVE-LOCK:
      
   IF bCloseItem.Concerns[1] > 999999
   THEN liEnd = liLongEnd.
   ELSE liEnd = liShortEnd.
      
   IF bCloseItem.Concerns[2] < liEnd THEN NEXT.
      
   /* delete or credit future items */
   IF bCloseItem.Concerns[1] > liEnd THEN DO:
      IF bCloseItem.Billed = FALSE THEN DELETE bCloseItem.
      ELSE ASSIGN odBilled   = odBilled + bCloseItem.Amt
                  liLastCred = MAX(liLastCred,bCloseItem.Concerns[2]).
   END. 
      
   /* item for end period covers end date -> split it (broken rental) */
   ELSE IF bCloseItem.Concerns[1] <= liEnd AND
           bCloseItem.Concerns[2] >= liEnd
   THEN DO:
        
      ASSIGN 
         /* length of period, use dates */
         ldtItemBeg = fInt2Date(bCloseItem.Concerns[1],1)
         ldtItemEnd = fInt2Date(bCloseItem.Concerns[2],2).
         
      /* whether original fee for first month was broken rental or not 
         determines the divisor */
      IF bCloseItem.Amt = bCloseFee.Amt AND  
         NOT CAN-FIND(FIRST FFItem OF bCloseFee WHERE 
                            FFItem.BillPeriod < bCloseItem.BillPeriod) 
      THEN DO:
         /* period is calendar month */
         IF MONTH(ldtItemEnd) NE MONTH(ldtItemEnd + 1) AND 
            MONTH(ldtItemBeg) = MONTH(ldtItemEnd) THEN 
            liPeriodLength = DAY(ldtItemEnd).
         /* otherwise use constant of 30 days per month */
         ELSE DO:
            IF DAY(ldtItemBeg) <= DAY(ldtItemEnd) THEN liPeriodLength = 1.
            ELSE liPeriodLength = 0.
            liMonth = MONTH(ldtItemBeg).

            DO ldaDate = ldtItemBeg TO ldtItemEnd:
               IF MONTH(ldaDate) NE liMonth THEN ASSIGN
                  liPeriodLength = liPeriodLength + 1
                  liMonth = MONTH(ldaDate).
            END.
            liPeriodLength = liPeriodLength * 30.
         END.   
      END.
      ELSE 
         liPeriodLength = ldtItemEnd - ldtItemBeg + 1. 
                
      /* amount from period's beginning to given end date */
      ldBRAmt = ROUND(bCloseItem.Amt * (idtEnd - ldtItemBeg + 1) /
                                        liPeriodLength,2).

      /* If Broken Rental is full month and calculated amount is less */
      /* than original amount then reverted back to original amount   */
      IF liBrokenRental = 1 AND ldBRAmt < ldeOriginalFee THEN
         ldBRAmt = ldeOriginalFee.

      /* If Groupcode is specified then charge is usage based month */
      IF icGroupCode > "" THEN
         ldBRAmt = fCalculateLastMonthFee(gcBrand,
                                          iiMsSeq,
                                          icGroupCode,
                                          ldeOriginalFee,
                                          liShortEnd).
      ELSE IF liBrokenRental = 2 AND ldBRAmt < ldeOriginalFee THEN
         ldBRAmt = ldeOriginalFee.
      
      /* force to full month fee */
      IF ilFMFee THEN ldBRAmt = ldeOriginalFee.

      /* unbilled item can be changed, billed must be credited */
      IF bCloseItem.Billed = FALSE THEN ASSIGN 
         bCloseItem.Amt         = ldBRAmt
         bCloseItem.Concerns[2] = liEnd.

      ELSE DO:
         /* Billed item can be changed if it is billed by test invoice */
         IF bCloseItem.Billed = TRUE AND
            CAN-FIND (FIRST Invoice USE-INDEX InvNum WHERE
                            Invoice.Brand   = gcBrand AND
                            Invoice.InvNum  = bCloseItem.InvNum AND
                            Invoice.InvType = 99 NO-LOCK) THEN DO:
            ASSIGN
               bCloseItem.Amt         = ldBRAmt
               bCloseItem.Concerns[2] = liEnd.

            /* Update ServiceLimitGroup identifier if first=last month */
            IF NOT CAN-FIND(FIRST bFFItem OF bCloseFee WHERE
                                  bFFItem.BillPeriod < bCloseItem.BillPeriod)
            THEN DO:
               IF liBrokenRental = 2 OR icGroupCode > "" THEN
                  bCloseFee.ServiceLimitGroup = "PMF:" + icGroupCode + ":" +
                                                STRING(bCloseItem.Amt).
               ELSE IF liBrokenRental = 1 THEN
                  bCloseFee.ServiceLimitGroup = "".
            END. /* IF NOT CAN-FIND(FIRST bFFItem OF bCloseFee WHERE */
         END. /* IF bCloseItem.Billed = TRUE AND */
         ELSE
            ASSIGN odBilled   = odBilled + (bCloseItem.Amt - ldBRAmt)
                   liLastCred = MAX(liLastCred,bCloseItem.Concerns[2]).
      END.
   END. 

END.

/* are there discount fees, i.e. with negative amount */
IF bCloseFee.Amt > 0 AND ilDiscount THEN 
FOR EACH bCredFee NO-LOCK WHERE
         bCredFee.Brand     = bCloseFee.Brand     AND
         bCredFee.HostTable = bCloseFee.HostTable AND
         bCredFee.KeyValue  = bCloseFee.KeyValue  AND
         bCredFee.CustNum   = bCloseFee.CustNum   AND
         bCredFee.BillCode  = bCloseFee.BillCode  AND
         bCredFee.Amt       < 0                   AND
         bCredFee.CustPP    = 0                   AND 
         bCredFee.InUse     = TRUE:

   RUN closefee (bCredFee.FFNum,
                 idtEnd,
                 FALSE,
                 FALSE,
                 icUserCode,
                 icFeeMemo,
                 0,
                 FALSE,
                 OUTPUT ldCredAmt).
                   
   /* reduce amount to be credited for billed items */
   odBilled = odBilled + ldCredAmt.
   
END. 


/* make a credit single fee */
IF odBilled > 0 AND ilCredit THEN DO:
    
   liBillTarg = 0.
   IF bCloseFee.HostTable = "mobsub" THEN DO:
      FIND MobSub WHERE MobSub.MsSeq = INTEGER(bCloseFee.KeyValue) 
      NO-LOCK NO-ERROR.
      IF AVAILABLE MobSub THEN liBillTarg = MobSub.BillTarget.
      ELSE DO:
         FIND FIRST MsOwner WHERE MsOwner.MsSeq = INTEGER(bCloseFee.KeyValue)
         NO-LOCK NO-ERROR.
         IF AVAILABLE MsOwner THEN liBillTarg = MsOwner.BillTarget.
      END.
   END.
   ELSE DO:
      FIND FIRST BillTarget OF Customer NO-LOCK NO-ERROR.
      IF AVAILABLE BillTarget THEN liBillTarg = BillTarget.BillTarget.
   END.
   
   CREATE SingleFee.
   ASSIGN SingleFee.Brand       = bCloseFee.Brand
          SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
          SingleFee.CustNum     = bCloseFee.CustNum
          SingleFee.BillTarget  = liBillTarg
          SingleFee.BillCode    = bCloseFee.BillCode
          SingleFee.BillPer     = liShortEnd
          SingleFee.Concerns[1] = YEAR(idtEnd + 1) * 10000 +
                                  MONTH(idtEnd + 1) * 100  +
                                  DAY(idtEnd + 1)
          SingleFee.Concerns[2] = liLastCred
          SingleFee.HostTable   = bCloseFee.HostTable
          SingleFee.KeyValue    = bCloseFee.KeyValue
          SingleFee.CalcObj     = "CLOSECRED"
          SingleFee.VATIncl     = bCloseFee.VatIncl
          SingleFee.BillType    = "KM"
          SingleFee.Contract    = bCloseFee.Contract
          SingleFee.Active      = TRUE
          SingleFee.Amt         = -1 * odBilled.
          SingleFee.Memo[1]     = DYNAMIC-FUNCTION("fHdrText" IN ghFunc1,
                                                   198,
                                                   Customer.Language).
   
   IF llDoEvent THEN RUN StarEventMakeCreateEventWithMemo(
                           lhSingleFee,
                           icUserCode,
                           icFeeMemo).

   RELEASE SingleFee.
          
END. 


IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFixedFee).
  
/* end period for fee */
FIND LAST bCloseItem OF bCloseFee NO-LOCK NO-ERROR.
IF AVAILABLE bCloseItem THEN
   ASSIGN bCloseFee.EndPer  = bCloseItem.BillPer
          bCloseFee.CalcAmt = bCloseItem.Amt.
ELSE DO:
   /* IF BegPer is JAN, EndPer must be DEC */
   IF bCloseFee.BegPer MOD 100 = 1 THEN
      bCloseFee.EndPer = (INT(bCloseFee.BegPer / 100) - 1) * 100 + 12.
   ELSE
      bCloseFee.EndPer  = bCloseFee.BegPer - 1.
   bCloseFee.CalcAmt = bCloseFee.Amt.
END.

/* mark as closed */
bCloseFee.CustPP = liLongEnd.

IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFixedFee).
   
llBankFinance = LOOKUP(bCloseFee.FinancedResult,
      SUBST("&1,&2,&3,&4",
      {&TF_STATUSES_BANK},
      {&TF_STATUS_HOLD_SENDING},
      {&TF_STATUS_SENT_TO_BANK},
      {&TF_STATUS_WAITING_SENDING})) > 0.

IF iiMsRequest > 0 AND
   (llBankFinance OR 
    bCloseFee.IFSStatus EQ {&IFS_STATUS_WAITING_SENDING}) THEN
   FOR FIRST MsRequest NO-LOCK WHERE
             Msrequest.MsRequest = iiMSRequest:

      llUpdateTFStatus =
         (bCloseFee.FinancedResult EQ {&TF_STATUS_HOLD_SENDING} OR
          bCloseFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING}).

      CASE MsRequest.ReqSource :
         
         WHEN {&REQUEST_SOURCE_RENEWAL} THEN DO:
            IF llUpdateTFStatus THEN
               bCloseFee.FinancedResult = {&TF_STATUS_YOIGO_RENEWAL}.
            ELSE lcReason = {&TF_CANCEL_RENEWAL}. 
         END.
         
         WHEN {&REQUEST_SOURCE_STC} THEN DO:
            IF llUpdateTFStatus THEN
               bCloseFee.FinancedResult = {&TF_STATUS_YOIGO_STC}.
            ELSE lcReason = {&TF_CANCEL_STC}. 
         END.
                                        
         WHEN {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} THEN DO:
            
            IF llUpdateTFStatus THEN
               bCloseFee.FinancedResult = {&TF_STATUS_YOIGO_REVERT_RENEWAL}.
            ELSE lcReason = {&TF_CANCEL_RENEWAL_CANCEL}. 

            IF bCloseFee.IFSStatus EQ {&IFS_STATUS_WAITING_SENDING} AND
               (llUpdateTFStatus OR NOT llBankFinance) THEN
               bCloseFee.IFSStatus = {&IFS_STATUS_SENDING_CANCELLED}.
         END.

         WHEN {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE} THEN DO:
         
            IF llUpdateTFStatus THEN
               bCloseFee.FinancedResult = {&TF_STATUS_YOIGO_INSTALLMENT_CHANGE}.
            ELSE lcReason = {&TF_CANCEL_INSTALLMENT}.
         END.
         
         WHEN {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} THEN DO:
               
            FIND FIRST bMsrequest NO-LOCK WHERE
                       bMsrequest.MsRequest = MsRequest.OrigReq AND
                       bMsrequest.ReqType = 18 AND
                       bMsrequest.Msseq = MsRequest.MsSeq NO-ERROR.

            IF AVAIL bMsrequest AND
               LOOKUP(bMsrequest.ReqCParam3,SUBST("&1,&2,&3",
                  {&SUBSCRIPTION_TERM_REASON_ORDER_CANCELLATION},
                  {&SUBSCRIPTION_TERM_REASON_POS_ORDER_CANCELATION},
                  {&SUBSCRIPTION_TERM_REASON_DIRECT_ORDER_CANCELATION})) > 0
               THEN ASSIGN
                  lcReason = {&TF_CANCEL_ORDER_CANCEL}
                  bCloseFee.IFSStatus = {&IFS_STATUS_SENDING_CANCELLED}
                     WHEN bCloseFee.IFSStatus EQ {&IFS_STATUS_WAITING_SENDING} AND
                          (llUpdateTFStatus OR NOT llBankFinance).
            
            IF llUpdateTFStatus THEN DO:
               bCloseFee.FinancedResult = {&TF_STATUS_YOIGO_SUB_TERMINATED}.
            END.

            IF lcReason EQ "" THEN ASSIGN
               lcReason = {&TF_CANCEL_TERMINATION}.
         END.
         OTHERWISE DO:
         
            IF llUpdateTFStatus THEN
               bCloseFee.FinancedResult = {&TF_STATUS_YOIGO_OTHER}.
            ELSE lcReason = {&TF_CANCEL_OTHER}.
         END.

      END CASE.

      IF LOOKUP(bCloseFee.FinancedResult,
         SUBST("&1,&2",
         {&TF_STATUSES_BANK},
         {&TF_STATUS_SENT_TO_BANK})) > 0 THEN DO:
         FIND FixedFeeTF EXCLUSIVE-LOCK WHERE
              FixedFeeTF.FFNum = bCloseFee.FFNum NO-ERROR.

         IF AVAIL FixedFeeTF THEN DO:
            ASSIGN
               FixedFeeTF.CancelStatus = "NEW"
               FixedFeeTF.CancelReason = lcReason.
            RELEASE FixedFeeTF.
         END.
      END.
END.
               
RELEASE bCloseFee.

/* eventlog creates dynamic temp-table, but never cleans it out */
fCleanEventObjects().

RETURN "".

