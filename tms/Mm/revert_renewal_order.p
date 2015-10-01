/* ----------------------------------------------------------------------
  Module .......: Mm/revert_renewal_order.p
  Task .........: Revert Renewal Order If AfterSales request (46) has been launched
  Application ..: TMS
  Author .......: Vikas
  Created ......: 29.06.12
  Version ......: Yoigo
   09.09.2015 hugo.lujan [Q25] - TMS - Cancel Renewal Order
---------------------------------------------------------------------- */

{commali.i}
{eventval.i}
{timestamp.i}
{fpcmaintreq.i}
{fmakemsreq.i}
{msreqfunc.i}
{tmsconst.i}
{ordercancel.i}
{coinv.i}
{dpmember.i}

DEFINE INPUT PARAMETER iiMsRequest  AS INTEGER  NO-UNDO.

DEF VAR liTermPeriod   AS INTEGER NO-UNDO.
DEF VAR lcCreditFees   AS CHAR    NO-UNDO. 
DEF VAR ldaRenewalDate AS DATE    NO-UNDO.
DEF VAR liRenewalTime  AS INTEGER NO-UNDO.

DEF BUFFER bSubMsRequest FOR MsRequest.
DEF BUFFER bMsRequest    FOR MsRequest.

DEF VAR ldtactdate1 AS DATE NO-UNDO.

FIND FIRST MsRequest WHERE
           MsRequest.MsRequest = iiMsRequest  NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN
   RETURN "ERROR:Unknown request".
ELSE IF MsRequest.ReqType NE {&REQTYPE_REVERT_RENEWAL_ORDER} THEN DO:
   fReqError("Invalid request type").
   RETURN.
END. /* ELSE IF MsRequest.ReqType NE {&REQTYPE_REVERT_RENEWAL_ORDER} */

FIND FIRST MobSub WHERE
           MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN DO:
   fReqError("Subscription not found").
   RETURN.
END. /* IF NOT AVAIL MobSub THEN DO: */

FIND FIRST bMsRequest WHERE
           bMsRequest.MsSeq      = MsRequest.MSSeq  AND
           bMsRequest.ReqType    = {&REQTYPE_AFTER_SALES_ORDER} AND
           bMsRequest.ReqIParam1 = MsRequest.ReqIParam1 NO-LOCK NO-ERROR.
IF NOT AVAIL bMsRequest OR
   LOOKUP(STRING(bMsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0
THEN DO:
   fReqError("Renewal order is still ongoing").
   RETURN.
END. /* IF NOT AVAIL bMsRequest OR */

FIND FIRST bSubMsRequest WHERE
           bSubMsRequest.OrigRequest = bMsRequest.MsRequest AND
          (bSubMsRequest.ReqType     = {&REQTYPE_CONTRACT_ACTIVATION} OR
           bSubMsRequest.ReqType     = {&REQTYPE_CONTRACT_TERMINATION}) AND
           LOOKUP(STRING(bSubMsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0
     NO-LOCK NO-ERROR.
IF AVAIL bSubMsRequest THEN DO:
   fReqError("Renewal order is still ongoing").
   RETURN.
END. /* IF AVAIL bSubMsRequest THEN DO: */

fSplitTS(bMsRequest.ActStamp,OUTPUT ldaRenewalDate,OUTPUT liRenewalTime).

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}
END.

FUNCTION fCollectActivationFees RETURNS LOGICAL (
   icDCEvent AS CHAR,
   iiPercontractID AS INT):

   DEF BUFFER FixedFee FOR FixedFee.
   DEF BUFFER SingleFee FOR SingleFee.
   DEF BUFFER Invoice FOR Invoice.

   IF icDCEvent BEGINS "PAYTERM" THEN DO:
      
      FOR FIRST FixedFee USE-INDEX Custnum WHERE
                FixedFee.Brand     = gcBrand   AND
                FixedFee.Custnum   = bSubMsRequest.Custnum AND
                FixedFee.HostTable = "MobSub"  AND
                FixedFee.KeyValue  = STRING(bSubMsRequest.MsSeq) AND
                FixedFee.CalcObj   = icDCEvent AND
                FixedFee.SourceTable = "DCCLI" AND
                FixedFee.SourceKey = STRING(iiPercontractID):
            
         FOR FIRST SingleFee USE-INDEX Custnum WHERE
                   SingleFee.Brand       = gcBrand AND
                   SingleFee.Custnum     = FixedFee.CustNum AND
                   SingleFee.HostTable   = FixedFee.HostTable AND
                   SingleFee.KeyValue    = FixedFee.KeyValue AND
                   SingleFee.SourceTable = FixedFee.SourceTable AND
                   SingleFee.SourceKey   = FixedFee.SourceKey AND
                   SingleFee.CalcObj     = "RVTERM" AND
                   SingleFee.Billed      = TRUE NO-LOCK,
             FIRST Invoice NO-LOCK WHERE
                   Invoice.InvNum = SingleFee.InvNum AND
                   Invoice.InvType = 1:

            IF LOOKUP(STRING(SingleFee.FMItemId), lcCreditFees) = 0 THEN
                    lcCreditFees = lcCreditFees + STRING(SingleFee.FMItemId) + ",".

         END.
      
         FOR FIRST SingleFee USE-INDEX Custnum NO-LOCK WHERE
                   SingleFee.Brand = gcBrand AND
                   SingleFee.Custnum = bSubMsRequest.CustNum AND
                   SingleFee.HostTable = "Mobsub" AND
                   SingleFee.KeyValue = STRING(bSubMsRequest.MsSeq) AND
                   SingleFee.BillCode BEGINS "PAYTERMCG" AND
                   SingleFee.SourceTable = "FixedFee" AND
                   SingleFee.SourceKey = STRING(FixedFee.FFNum) AND
                   SingleFee.Billed = TRUE,
             FIRST Invoice NO-LOCK WHERE
                   Invoice.InvNum = SingleFee.InvNum AND
                   Invoice.InvType = 1:
            IF LOOKUP(STRING(SingleFee.FMItemId), lcCreditFees) = 0 THEN
                    lcCreditFees = lcCreditFees + STRING(SingleFee.FMItemId) + ",".
         END.
      END.
   END.
   ELSE IF icDCEvent BEGINS "TERM" AND
      bSubMsRequest.ReqCParam2 EQ "recreate" THEN DO:
   
      FOR FIRST SingleFee USE-INDEX Custnum WHERE
                SingleFee.Brand = gcBrand AND
                SingleFee.Custnum = bSubMsRequest.CustNum AND
                SingleFee.HostTable = "Mobsub" AND
                SingleFee.KeyValue = STRING(bSubMsRequest.MsSeq) AND
                SingleFee.BillPeriod = liTermPeriod AND
                SingleFee.BillCode BEGINS "TERMPERIOD" AND 
                SingleFee.Billed = TRUE NO-LOCK,
          FIRST Invoice NO-LOCK WHERE
                Invoice.InvNum = SingleFee.InvNum AND
                Invoice.InvType = 1:
         IF LOOKUP(STRING(SingleFee.FMItemId), lcCreditFees) = 0 THEN
                 lcCreditFees = lcCreditFees + STRING(SingleFee.FMItemId) + ",".
      END.

   END.
   
   RETURN TRUE.
END FUNCTION.

FUNCTION fCollectTerminationFees RETURNS LOGICAL (
   icDCEvent AS CHAR,
   iiPercontractID AS INT):

   DEF BUFFER FixedFee FOR FixedFee.
   DEF BUFFER SingleFee FOR SingleFee.
   DEF BUFFER Invoice FOR Invoice.

   IF icDCEvent BEGINS "PAYTERM" THEN DO:
      
      FOR FIRST FixedFee USE-INDEX Custnum WHERE
                FixedFee.Brand     = gcBrand   AND
                FixedFee.Custnum   = bSubMsRequest.Custnum AND
                FixedFee.HostTable = "MobSub"  AND
                FixedFee.KeyValue  = STRING(bSubMsRequest.MsSeq) AND
                FixedFee.CalcObj   = icDCEvent AND
                FixedFee.SourceTable = "DCCLI" AND
                FixedFee.SourceKey = STRING(iiPercontractID):
            
         FOR FIRST SingleFee USE-INDEX Custnum NO-LOCK WHERE
                   SingleFee.Brand = gcBrand AND
                   SingleFee.Custnum = bSubMsRequest.CustNum AND
                   SingleFee.HostTable = "Mobsub" AND
                   SingleFee.KeyValue = STRING(bSubMsRequest.MsSeq) AND
                   SingleFee.BillCode BEGINS "PAYTERMEND" AND
                   SingleFee.SourceTable = "FixedFee" AND
                   SingleFee.SourceKey = STRING(FixedFee.FFNum) AND
                   SingleFee.Billed = TRUE,
             FIRST Invoice NO-LOCK WHERE
                   Invoice.InvNum = SingleFee.InvNum AND
                   Invoice.InvType = 1:
            IF LOOKUP(STRING(SingleFee.FMItemId), lcCreditFees) = 0 THEN
                    lcCreditFees = lcCreditFees + STRING(SingleFee.FMItemId) + ",".
         END.
         
      END.
   END.
   ELSE IF icDCEvent BEGINS "TERM" THEN DO:
   
      FOR FIRST SingleFee USE-INDEX Custnum WHERE
                SingleFee.Brand = gcBrand AND
                SingleFee.Custnum = bSubMsRequest.CustNum AND
                SingleFee.HostTable = "Mobsub" AND
                SingleFee.KeyValue = STRING(bSubMsRequest.MsSeq) AND
                SingleFee.BillPeriod = liTermPeriod AND
                SingleFee.BillCode BEGINS "TERMPERIOD" AND 
                SingleFee.Billed = TRUE NO-LOCK,
          FIRST Invoice NO-LOCK WHERE
                Invoice.InvNum = SingleFee.InvNum AND
                Invoice.InvType = 1:
         IF LOOKUP(STRING(SingleFee.FMItemId), lcCreditFees) = 0 THEN
                 lcCreditFees = lcCreditFees + STRING(SingleFee.FMItemId) + ",".
      END.

   END.
   
   RETURN TRUE.
END FUNCTION.

RUN pRevertRenewalOrder.

IF llDoEvent THEN fCleanEventObjects().

RETURN RETURN-VALUE.


PROCEDURE pRevertRenewalOrder:

   DEF VAR liTermRequest          AS INTEGER   NO-UNDO.
   DEF VAR lcError                AS CHARACTER NO-UNDO.
   DEF VAR ldaLastMonth           AS DATE      NO-UNDO.
   DEF VAR ldaLastDayOfLastMonth  AS DATE      NO-UNDO.
   DEF VAR ldPeriodTo             AS DECIMAL   NO-UNDO.
   DEF VAR ldaTermDate            AS DATE      NO-UNDO.
   DEF VAR liTermTime             AS INTEGER   NO-UNDO.
   DEF VAR llReCreate             AS LOGICAL   NO-UNDO.
   DEF VAR liCount                AS INTEGER   NO-UNDO.
   DEF VAR ldaRequestDate AS DATE NO-UNDO. 

   DEFINE BUFFER bDCCLI      FOR DCCLI.

   /* Request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   TRANS_BLK:
   DO TRANSACTION:
      
   /* Release IMEI */
   fReleaseIMEI(MsRequest.ReqIParam1).

   /* Get all the sub requests processed with AfterSales request */
   FOR FIRST bMsRequest NO-LOCK WHERE
             bMsRequest.MsSeq      = MsRequest.MsSeq AND
             bMsRequest.ReqType    = {&REQTYPE_AFTER_SALES_ORDER} AND
             bMsRequest.ReqStatus  = {&REQUEST_STATUS_DONE} AND
             bMsRequest.ReqIParam1 = MsRequest.ReqIParam1,
       EACH bSubMsRequest NO-LOCK WHERE
            bSubMsRequest.OrigRequest = bMsRequest.MsRequest AND
            (bSubMsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} OR
             bSubMsRequest.ReqType    = {&REQTYPE_CONTRACT_TERMINATION}),
       FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand   = gcBrand AND
             DayCampaign.DCEvent = bSubMsRequest.ReqCparam3 AND
            (DayCampaign.DCType EQ {&DCTYPE_DISCOUNT} OR
             DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT}):

       IF bSubMsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} THEN DO:
      
          IF DayCampaign.DCEvent EQ {&DCTYPE_INSTALLMENT} THEN
             fTS2Date(bMsRequest.ActStamp, output ldaRequestDate).
          ELSE ldaRequestDate = 1/1/2000.

          FIND FIRST DCCLI NO-LOCK WHERE
                     DCCLI.Brand      = gcBrand AND
                     DCCLI.DCEvent    = DayCampaign.DCEvent AND
                     DCCLI.MsSeq      = bSubMsRequest.MsSeq AND
                     DCCLI.ValidFrom <= TODAY AND
                     DCCLI.ValidTo   >= TODAY AND
                     DCCLI.ValidFrom >= ldaRequestDate AND
                     (IF bSubMsRequest.ReqIParam5 > 0
                      THEN DCCLI.percontractid = bSubMsRequest.ReqIParam5
                      ELSE TRUE) NO-ERROR.
          IF NOT AVAIL DCCLI THEN NEXT.

             ASSIGN llReCreate = FALSE
                    liCount = 0.

             /* Find last closed contract to pick validto date */
             REPEAT:
                liCount = liCount + 1.
                FIND FIRST bDCCLI NO-LOCK WHERE
                           bDCCLI.Brand      = gcBrand AND
                           bDCCLI.DCEvent    = DayCampaign.DCEvent AND
                           bDCCLI.MsSeq      = bSubMsRequest.MsSeq AND
                           bDCCLI.ValidFrom <= TODAY AND
                           bDCCLI.ValidTo    = (ldaRenewalDate - liCount) NO-ERROR.
                IF NOT AVAIL bDCCLI THEN LEAVE.
             END.

             /* Check if any contract was closed due to renewal */
             IF bSubMsRequest.ReqCparam2 = "recreate" AND
                bSubMsRequest.ReqIParam1 > 0 THEN DO:
                FIND FIRST bDCCLI NO-LOCK WHERE
                           bDCCLI.Brand      = gcBrand AND
                           bDCCLI.DCEvent    = DayCampaign.DCEvent AND
                           bDCCLI.MsSeq      = bSubMsRequest.MsSeq AND
                           bDCCLI.ValidFrom <= TODAY AND
                           bDCCLI.PerContractID = bSubMsRequest.ReqIParam1 NO-ERROR.
                IF AVAIL bDCCLI THEN llReCreate = TRUE.
             END. /* IF bSubMsRequest.ReqCparam2 = "recreate" AND */

             IF DayCampaign.DCType = {&DCTYPE_DISCOUNT} THEN DO:
                liTermRequest = fPCMaintenanceRequest(bSubMsRequest.MsSeq,
                                        DayCampaign.DCEvent,
                                        "ValidTo",
                                        STRING(DCCLI.ValidFrom - licount),
                                        ?,
                                        FALSE,
                                        {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER},
                                        "",
                                        MsRequest.MsRequest,
                                        FALSE,
                                        OUTPUT lcError).
                IF liTermRequest = 0 THEN
                   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                    "MobSub",
                                    STRING(bSubMsRequest.MsSeq),
                                    bSubMsRequest.CustNum,
                                    DayCampaign.DCEvent + " Termination",
                                    DayCampaign.DCEvent + " request failed " +
                                    lcError).
             END. /* IF DayCampaign.DCType = {&DCTYPE_DISCOUNT} THEN DO: */
             ELSE IF DayCampaign.DCType  = {&DCTYPE_INSTALLMENT} THEN DO:

                 ASSIGN
                 ldaLastDayOfLastMonth = fLastDayOfMonth(
                                         ADD-INTERVAL(DCCLI.ValidFrom, -1, "months"))
                 ldPeriodTo = fMake2Dt(ldaLastDayOfLastMonth,86399).

                /* terminate payterm contract */
                liTermRequest = fPCActionRequest(bSubMsRequest.MsSeq,
                                        DayCampaign.DCEvent,
                                        "term",
                                        ldPeriodTo,
                                        FALSE,   /* create fee */
                                        {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER},
                                        "",
                                        MsRequest.MsRequest,
                                        FALSE,
                                        "",
                                        0,
                                        DCCLI.PerContractID,
                                        OUTPUT lcError).
                IF liTermRequest = 0 THEN
                   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                    "MobSub",
                                    STRING(bSubMsRequest.MsSeq),
                                    bSubMsRequest.CustNum,
                                    DayCampaign.DCEvent + " Termination",
                                    DayCampaign.DCEvent + " request failed " +
                                    lcError).
                ELSE fCollectActivationFees(DayCampaign.DCEvent, DCCLI.PerContractID). 
             END. /* ELSE IF DayCampaign.DCType  = {&DCTYPE_INSTALLMENT} */

             /* Re-activate the previous contract which
                was terminated due to Renewal Order */
             IF llReCreate THEN DO:
                fSplitTS(bSubMsRequest.ActStamp,OUTPUT ldaTermDate,OUTPUT liTermTime).
                ASSIGN ldaTermDate  = ldaTermDate - 1
                       liTermPeriod = YEAR(ldaTermDate) * 100 + MONTH(ldaTermDate).
            
                liTermRequest = fPCActionRequest(bSubMsRequest.MsSeq,
                                                 DayCampaign.DCEvent,
                                                 "reactivate",
                                                 fSecOffSet(fMakeTS(),120), /* 2 mins gap */
                                                 TRUE,
                                                 {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER},
                                                 "",
                                                 MsRequest.MsRequest,
                                                 FALSE,
                                                 "",
                                                 0,
                                                 (IF bDCCLI.DCEvent BEGINS "PAYTERM" 
                                                  THEN bDCCLI.PerContractID
                                                  ELSE 0),
                                                 OUTPUT lcError).
                IF liTermRequest = 0 THEN
                   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                    "MobSub",
                                    STRING(bSubMsRequest.MsSeq),
                                    bSubMsRequest.CustNum,
                                    DayCampaign.DCEvent + " Reactivation",
                                    DayCampaign.DCEvent + " request failed " +
                                    lcError).
                /* Delete Single Fee */
                ELSE fCollectTerminationFees(DayCampaign.DCEvent, bDCCLI.PerContractID). 

           END. /* IF llReCreate THEN DO: */
        END. /* IF bSubMsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} */

       ELSE IF bSubMsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION}
       THEN DO:
          fSplitTS(bSubMsRequest.ActStamp,OUTPUT ldaTermDate,OUTPUT liTermTime).
          ASSIGN liTermPeriod = YEAR(ldaTermDate) * 100 + MONTH(ldaTermDate).

          liTermRequest = fPCActionRequest(bSubMsRequest.MsSeq,
                                           DayCampaign.DCEvent,
                                           "reactivate",
                                           fMakeTS(),
                                           TRUE,
                                           {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER},
                                           "",
                                           MsRequest.MsRequest, /* Father Request */
                                           FALSE,
                                           "",
                                           0,
                                           bSubMsRequest.ReqIParam3, 
                                           OUTPUT lcError).
          IF liTermRequest = 0 THEN
             DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                              "MobSub",
                              STRING(bSubMsRequest.MsSeq),
                              bSubMsRequest.CustNum,
                              DayCampaign.DCEvent + " Reactivation",
                              DayCampaign.DCEvent + " request failed " +
                              lcError).
          /* Delete Single Fee */
          ELSE fCollectTerminationFees(DayCampaign.DcEvent, bSubMsRequest.ReqIParam3).

       END. /* ELSE IF bSubMsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} */
   END. /* FOR FIRST bMsRequest WHERE */
   
   RUN pCreateRenewalCreditNote(MsRequest.ReqIParam1,TRIM(lcCreditFees,",")).
   /* Q25 */
   RUN pQ25HandleDiscount IN THIS-PROCEDURE.
   /* Request handled succesfully */
   FIND FIRST MsRequest WHERE
              MsRequest.MSRequest = iiMsRequest NO-LOCK NO-ERROR.
   
   fReqStatus(2,"").   
END. /* DO TRANSACTION: */

RETURN "".

END PROCEDURE.

/* Q25 */
/*   
   TASK .........: Close the corresponding RVTERMDT1 discount for Quota 25
   YPR-2520
   If the order is cancelled (has not been delivered correctly to the customer
   or order is cancelled by customer) before month 25 then the following 
   actions to be performed: 
   -Don't cancel Quota 25 extension request. 
   -Remove the discount which was given to customer over Quota 25. 
   -If discount (RVTERMDT1) was already billed then we have to bill 
    the Quota 25 again to customer in next invoice. 
   
   Questions before month 25: 
   Q: How to know? If the order has not been delivered correctly to the customer
   A: Order logistics status is 3 (cancelled) and there's revert renewal 
    request (type 49) REQTYPE_REVERT_RENEWAL_ORDER
    with request source 14 (order cancellation) REQUEST_SOURCE_ORDER_CANCELLATION
   Q: How to know? if order is cancelled by customer
   A: There's a revert renewal request (type 49) REQTYPE_REVERT_RENEWAL_ORDER
   with request source other than 14 (REQUEST_SOURCE_ORDER_CANCELLATION)
   NOTE: From TMS point of view the handling is the same for LO and customer cancellation.
   Q: How to? Remove the discount which was given to customer over Quota 25
   A: Close the corresponding RVTERMDT1 discount
*/
PROCEDURE pQ25HandleDiscount:
 DEF VAR liPercontractId AS INTEGER   NO-UNDO. 
 DEF VAR ldeDiscount     AS DECIMAL   NO-UNDO. 
 DEF VAR lcResult        AS CHARACTER NO-UNDO.
 /* Date when the payment plan was created */   
 DEF VAR ldBillPeriod      AS DATE NO-UNDO.
 /* Temp variable for the date of the extension request */
 DEF VAR ldaProrateRequest AS DATE NO-UNDO.
 /* Month 24 Date */
 DEF VAR ldaMonth24Date    AS DATE NO-UNDO.
 /* Number of months elapsed since the payment plan was created */
 DEF VAR liMonthsElapsed   AS INTEGER NO-UNDO.
 
 FIND FIRST OrderAction NO-LOCK WHERE
            OrderAction.Brand    EQ gcBrand AND
            OrderAction.OrderId  EQ MsRequest.ReqIParam1 AND
            OrderAction.ItemType EQ "Q25Extension" NO-ERROR.
    
 IF NOT AVAILABLE OrderAction THEN
    RETURN "OK".

 liPercontractId = INT(OrderAction.ItemKey) NO-ERROR.
 IF ERROR-STATUS:ERROR OR liPercontractId EQ 0 THEN
    RETURN "ERROR:Q25 discount creation failed (incorrect contract id)".
    
 FIND FIRST DCCLI NO-LOCK WHERE
            DCCLI.Brand   EQ gcBrand AND
            DCCLI.DCEvent EQ "RVTERM12" AND
            DCCLI.MsSeq   EQ Mobsub.MsSeq AND
            DCCLI.ValidTo >= TODAY NO-ERROR.
 
 IF NOT AVAILABLE DCCLI THEN
 DO:
  FIND FIRST MSRequest NO-LOCK WHERE 
             MSRequest.MsSeq      EQ Mobsub.MsSeq AND
             MSRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
             MsRequest.ReqStatus  EQ 0 AND /* ONGOING */
             MSREquest.REqcparam3 EQ "RVTERM12" AND
             MSREquest.ReqIParam3 = liPercontractId NO-ERROR.
  IF AVAILABLE MSRequest THEN
  DO:
   fCloseDiscount("RVTERMDT1DISC",
      MobSub.MsSeq,
      TODAY,
      FALSE). /* clean event logs */
     RETURN "OK". 
  END.
  RETURN "ERROR:Q25 Activation Request not found".
 END.
                                            
 ldeDiscount = DEC(OrderAction.ItemParam) NO-ERROR.
 IF ERROR-STATUS:ERROR OR ldeDiscount EQ 0 THEN 
    RETURN "ERROR:Q25 discount creation failed (incorrect discount amount)". 
    
 FIND SingleFee USE-INDEX Custnum WHERE
        SingleFee.Brand       EQ gcBrand AND
        SingleFee.Custnum     EQ MobSub.Custnum AND
        SingleFee.HostTable   EQ "Mobsub" AND
        SingleFee.KeyValue    EQ STRING(Mobsub.MsSeq) AND
        SingleFee.SourceTable EQ "DCCLI" AND
        SingleFee.SourceKey   EQ STRING(liPerContractID) AND
        SingleFee.CalcObj     EQ "RVTERM" NO-LOCK NO-ERROR.        

 IF NOT AVAILABLE SingleFee THEN
    RETURN "ERROR:Q25 discount creation failed (residual fee not found)".
 IF SingleFee.Billed AND
   CAN-FIND(FIRST Invoice NO-LOCK WHERE
                  Invoice.Invnum  EQ SingleFee.Invnum AND
                  Invoice.InvType EQ 1) THEN /* 1 Service Invoice */
 DO:
    FIND FIRST 
       subInvoice NO-LOCK WHERE
       subInvoice.InvNum EQ SingleFee.InvNum NO-ERROR.
    
    IF NOT AVAILABLE subInvoice THEN
      RETURN "ERROR:Q25 subInvoice record not found)".
    
    FIND FIRST 
       invrow NO-LOCK WHERE
       invrow.InvNum    EQ subInvoice.InvNum AND
       invrow.SubInvNum EQ subInvoice.SubInvNum AND
       /* The discount which was given to customer over Quota 25 */
       invrow.BillCode EQ "RVTERMDT1" 
       NO-ERROR.
    
    IF NOT AVAILABLE invrow THEN
      RETURN "ERROR:Q25 invrow record not found)".  
      
    /* Month 25 and after: Create new single fee 
       (CRVTERMDT) with corresponding amount*/
     RUN creasfee.p(MobSub.CustNum,
        MobSub.MsSeq,
        ldtActDate,
        "FeeModel",
        "CRVTERMDT",
        9,
        ABS(invrow.Amt), /* Discount amount, to charge in next invoice */
        DCCLI.DCEvent + " created " + 
           STRING(ldtActDate,"99.99.9999"),  /* memo */
        FALSE,              /* no messages to screen */
        "",
        "RevertRenewalQ25",
        0, /* order id */
        SingleFee.SourceTable,
        SingleFee.SourceKey,
        OUTPUT lcResult).
   IF lcResult > "" THEN
   DO:
      /* Create a memo in TMS */
   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = fMakeTS() 
       Memo.Brand     = gcBrand
       Memo.HostTable = ""
       Memo.KeyValue  = STRING(MobSub.MsSeq)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = katun
       Memo.MemoTitle = "CRVTERMDT Fee creation failed"
       Memo.MemoText  = lcResult
       Memo.CustNum   = MobSub.CustNum.
   END.   
 END.
 ELSE
 DO:
   fCloseDiscount("RVTERMDT1DISC",
      MobSub.MsSeq,
      TODAY,
      FALSE). /* clean event logs */
     RETURN "OK". 
  END.
END PROCEDURE.
