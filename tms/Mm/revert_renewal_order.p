/* ----------------------------------------------------------------------
  Module .......: Mm/revert_renewal_order.p
  Task .........: Revert Renewal Order If AfterSales request (46) has been launched
  Application ..: TMS
  Author .......: Vikas
  Created ......: 29.06.12
  Version ......: Yoigo
   09.09.2015 hugo.lujan [Q25] - TMS - Cancel Renewal Order
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/fpcmaintreq.i}
{Func/fmakemsreq.i}
{Func/msreqfunc.i}
{Syst/tmsconst.i}
{Func/ordercancel.i}
{Func/coinv.i}
{Mc/dpmember.i}

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
           LOOKUP(STRING(bSubMsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0 AND
           bSubMsRequest.ReqCParam3 NE "RVTERM12"
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

       IF bSubMsRequest.ReqCParam3 EQ "RVTERM12" THEN NEXT.

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
   RUN pCloseQ25Discount IN THIS-PROCEDURE.
   IF RETURN-VALUE BEGINS "ERROR" THEN
       DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                        "MobSub",
                        STRING(MobSub.MsSeq),
                        MobSub.CustNum,
                        "Revert renewal order",
                        RETURN-VALUE).

   /* Request handled succesfully */
   FIND FIRST MsRequest WHERE
              MsRequest.MSRequest = iiMsRequest NO-LOCK NO-ERROR.
   
   fReqStatus(2,"").   
END. /* DO TRANSACTION: */

RETURN "".

END PROCEDURE.

/* Q25 - Close the corresponding RVTERMDT1 discount for Quota 25. YPR-2520 */
PROCEDURE pCloseQ25Discount:

   DEF VAR liPercontractId AS INTEGER   NO-UNDO. 
   DEF VAR ldeDiscount     AS DECIMAL   NO-UNDO. 
   DEF VAR lcResult        AS CHARACTER NO-UNDO.
   DEF VAR liRequest       AS INT NO-UNDO. 

   DEF BUFFER bMsRequest FOR MSRequest.

   FIND FIRST OrderAction NO-LOCK WHERE
              OrderAction.Brand    EQ gcBrand AND
              OrderAction.OrderId  EQ MsRequest.ReqIParam1 AND
              OrderAction.ItemType EQ "Q25Discount" NO-ERROR.
      
   IF NOT AVAILABLE OrderAction THEN RETURN "".

   liPercontractId = INT(OrderAction.ItemParam) NO-ERROR.
   IF ERROR-STATUS:ERROR OR liPercontractId EQ 0 THEN
      RETURN "ERROR:Q25 discount cancellation (contract id)".
   
   ldeDiscount = DEC(OrderAction.ItemKey) NO-ERROR.
   IF ERROR-STATUS:ERROR OR ldeDiscount EQ 0 THEN 
      RETURN "ERROR:Q25 discount cancellation (discount amount)". 

   FIND SingleFee NO-LOCK WHERE
        SingleFee.Brand       = gcBrand AND
        SingleFee.HostTable   = "Mobsub" AND
        SingleFee.KeyValue    = STRING(Mobsub.MsSeq) AND
        SingleFee.SourceTable = "DCCLI" AND
        SingleFee.SourceKey   = STRING(liPerContractID) AND
        SingleFee.CalcObj     = "RVTERM" NO-ERROR.
   
   IF NOT AVAILABLE SingleFee THEN RETURN "".

   FIND FIRST bmsrequest NO-LOCK WHERE
              bmsrequest.msseq = mobsub.msseq AND
              bmsrequest.reqtype = 8 AND
              bmsrequest.reqstatus = 0 AND
              bmsrequest.reqcparam3 = "RVTERM12" AND
              bmsrequest.reqiparam3 = liPerContractID NO-ERROR.
   IF AVAIL bmsrequest THEN DO:

      FIND MsRequest NO-LOCK WHERE
           MsRequest.MSRequest = bmsrequest.MSRequest.

      fReqStatus(4,"Cancelled by renewal cancellation").
      
      FIND FIRST MsRequest WHERE
                 MsRequest.MsRequest = iiMsRequest  NO-LOCK NO-ERROR.
   END.
   ELSE DO:

      FIND DCCLI NO-LOCK WHERE
           DCCLI.Brand   EQ gcBrand AND
           DCCLI.DCEvent EQ "RVTERM12" AND
           DCCLI.MsSeq   EQ MobSub.MsSeq AND
           DCCLI.ValidTo >= TODAY NO-ERROR.

      IF AMBIGUOUS(DCCLI) THEN 
         RETURN "ERROR:More than one active Q25 extension contract".

      IF AVAIL DCCLI THEN DO:

         liRequest = fPCActionRequest(
            MobSub.MsSeq,
            "RVTERM12",
            "term",
            fMakeTS(),
            TRUE, /* create fees */
            {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER},
            "",
            MsRequest.MsRequest,
            FALSE,
            "",
            0, /* payterm residual fee */
            0,
            OUTPUT lcResult).

         IF liRequest EQ 0 THEN
            RETURN SUBST("ERROR:Q25 extension contract termination failed: &1",
                   lcResult).
      END.
   END.
   
   FOR EACH DiscountPlan NO-LOCK WHERE
            DiscountPlan.Brand = gcBrand AND
     LOOKUP(DiscountPlan.DPRuleID,"RVTERMDT1DISC,RVTERMDT4DISC") > 0:

      FIND FIRST dpmember NO-LOCK WHERE
                 dpmember.dpid = DiscountPlan.DpId AND
                 dpmember.hosttable = "mobsub" AND
                 dpmember.keyvalue = string(mobsub.msseq) AND
                 dpmember.validfrom = fPer2Date(SingleFee.BillPeriod,0) AND
                 dpmember.validto >= dpmember.validfrom AND
                 dpmember.discvalue = ldeDiscount NO-ERROR.
      IF AVAIL dpmember THEN LEAVE.
   END.
   
   IF NOT AVAILABLE dpmember THEN RETURN "".
  
   IF SingleFee.Billed AND
      NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                     Invoice.Invnum  EQ SingleFee.Invnum AND
                     Invoice.Custnum EQ SingleFee.Custnum AND
                     Invoice.InvType EQ 99) THEN DO:

      FIND FIRST subInvoice NO-LOCK WHERE
                 subInvoice.InvNum EQ SingleFee.InvNum AND
                 subInvoice.MsSeq EQ Mobsub.MsSeq NO-ERROR.
      
      IF NOT AVAILABLE subInvoice THEN
        RETURN "ERROR:Q25 discount cancellation (subinvoice not found)".
      
      FIND FIRST invrow NO-LOCK WHERE
                 invrow.InvNum    EQ subInvoice.InvNum AND
                 invrow.SubInvNum EQ subInvoice.SubInvNum AND
          LOOKUP(invrow.BillCode,"RVTERMDTRW,RVTERMDTEQ25") > 0  NO-ERROR.
      
      IF NOT AVAILABLE invrow THEN RETURN "".
        
      /* Month 25 and after: Create new single fee 
         (CRVTERMDT) with corresponding amount*/
       RUN Mc/creasfee.p(
          SingleFee.CustNum,
          MobSub.MsSeq,
          TODAY,
          "FeeModel",
          "CRVTERMDT",
          9,
          MIN(ldeDiscount,ABS(invrow.Amt)),
          "Renewal order cancelled " + 
             STRING(TODAY,"99.99.9999"),  /* memo */
          FALSE,              /* no messages to screen */
          "",
          "RevertRenewalOrder",
          SingleFee.OrderID, /* order id */
          SingleFee.SourceTable,
          SingleFee.SourceKey,
          OUTPUT lcResult).

      IF lcResult BEGINS "ERROR:" OR lcResult BEGINS "0" THEN
         RETURN SUBST("ERROR:Q25 discount cancellation (CRVTERMDT):&1",lcResult).
      
      RETURN "".

   END.

   fCloseDiscount(DiscountPlan.DPRuleID,
     MobSub.MsSeq,
     dpmember.ValidFrom - 1,
     FALSE). /* clean event logs */
      
   RETURN "". 

END PROCEDURE.
