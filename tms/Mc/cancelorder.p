/* ----------------------------------------------------------------------
hell MODULE .......: cancelorder.p
  TASK .........: Cancels direct channel order. YDR-74, YOT-924
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 04.03.10
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/fsubstermreq.i}
{Func/ordercancel.i}
{Func/fmakemsreq.i}
{Func/main_add_lines.i}
{Mc/cash_revert_order.i}

DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.
DEF INPUT PARAMETER ilCheckLOStatus AS LOG NO-UNDO.

DEF VAR liTermReason AS INTEGER NO-UNDO.
DEF VAR ldeTS AS DECIMAL NO-UNDO.
DEF VAR liReq AS INTEGER NO-UNDO.
DEF VAR llYoigoCLi AS LOGICAL NO-UNDO.
DEF VAR liMsisdnStat AS INTEGER NO-UNDO.
DEF VAR liSimStat AS INTEGER NO-UNDO.
DEF VAR liQuarTime AS INTEGER NO-UNDO.
DEF VAR llPenaltyFee AS LOGICAL NO-UNDO.
DEF VAR lcResult AS CHARACTER NO-UNDO. 
DEF VAR lcCreditReason AS CHARACTER NO-UNDO. 
DEF VAR ldtLOTS AS DATETIME NO-UNDO.
DEF VAR liError AS INT NO-UNDO. 
DEF VAR lcTermType AS CHARACTER NO-UNDO. 

FIND Order NO-LOCK WHERE
     Order.Brand = gcBrand AND
     Order.OrderID = iiOrder NO-ERROR.
IF NOT AVAIL Order THEN RETURN "".

IF LOOKUP(Order.StatusCode, {&ORDER_INACTIVE_STATUSES}) = 0 THEN RETURN "".

IF ilCheckLOStatus THEN DO:

   FIND FIRST OrderDelivery OF Order NO-LOCK USE-INDEX OrderId NO-ERROR.

   IF NOT AVAILABLE OrderDelivery
   THEN RETURN "".

   ldtLOTS = OrderDelivery.LOTimeStamp.

   FOR EACH OrderDelivery NO-LOCK WHERE
            OrderDelivery.Brand   = gcBrand AND
            OrderDelivery.OrderId = Order.OrderId AND
            OrderDelivery.LOTimeStamp = ldtLOTS:
      IF LOOKUP(STRING(OrderDelivery.LOStatusId),{&DEXTRA_CANCELLED_STATUSES}) = 0
      THEN RETURN "".
   END.

END.

IF LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN DO:

   /* Credit cash invoice */
   IF Order.InvNum > 0 AND 
      /* check if credit note was already created from closeorder.p */
      fCheckCreditNoteRequest(Order.Custnum, Order.InvNum) EQ "" THEN DO:
      
      IF Order.MNPStatus = 0 THEN lcCreditReason = "1010".
      ELSE IF Order.MNPStatus = 5 THEN lcCreditReason = "1012".
      ELSE DO:
         FIND MNPProcess WHERE
              MNPProcess.OrderId = Order.OrderId AND
              MNPProcess.StatusCode = ({&MNP_ST_ACAN}) NO-LOCK NO-ERROR.
         IF AVAIL MNPProcess AND
                  MNPProcess.StatusReason = "CANC_ABONA" THEN
              lcCreditReason = "1010".
         ELSE lcCreditReason = "1011".
      END.

      lcResult = fCashInvoiceCreditnote(Order.Invnum, lcCreditReason).

      IF lcResult > "" THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                           "Order",
                           STRING(Order.OrderId),
                           Order.Custnum,
                           "CREDIT NOTE CREATION FAILED",
                           lcResult). 
   END.
   /*release icc 
     or 
     set it to Temporally if conditions are met*/

   IF Order.OrderType EQ {&ORDER_TYPE_NEW} OR
      Order.OrderType EQ {&ORDER_TYPE_MNP} THEN DO:

      FIND SIM WHERE
           SIM.ICC = Order.ICC AND
           SIM.SimStat = {&SIM_SIMSTAT_SENT_TO_LOGISTICS} AND
           SIM.MsSeq = Order.MsSeq
      NO-LOCK USE-INDEX simseSta_s NO-ERROR.
         
      IF AVAIL SIM THEN DO:
         IF OrderDelivery.LoStatusID = 875 THEN DO:
            FIND CURRENT SIM EXCLUSIVE-LOCK.
            SIM.SimStat = {&SIM_SIMSTAT_LOST}.
            RELEASE SIM.
         END.
         /*YPR-2486*/
         ELSE IF fcParamI("UseTempSimStatus") EQ 1 THEN DO:
            FIND CURRENT SIM EXCLUSIVE-LOCK.
            SIM.SimStat = {&SIM_SIMSTAT_TEMP}.
            RELEASE SIM.
         END. 
         ELSE fReleaseSim(Order.OrderID).
      END.
   END.

   fReleaseImei(Order.OrderId).
END.
/* Subscription is not changed according to Orders */
ELSE IF Order.OrderType EQ {&ORDER_TYPE_MNP} THEN DO:

   FIND MobSub WHERE
        MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.

   IF NOT AVAIL MobSub THEN RETURN "".

   IF MobSub.PayType EQ FALSE THEN
   fCreateLimitHistory(MobSub.InvCust,
                       MobSub.MsSeq,
                       {&LIMIT_TYPE_BILLPERM},
                       {&LIMIT_BILLPERM_PROHIBITED},
                       0,
                       0,
                       FALSE,
                       TODAY,
                       12/31/2049).
END.
ELSE DO:
   lcResult = fCashRevertOrder(Order.OrderId).

   IF lcResult > ""
   THEN RETURN lcResult.

   IF Order.OrderType EQ {&ORDER_TYPE_NEW} THEN DO:
      FIND MobSub WHERE
           MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.

      IF NOT AVAIL MobSub THEN RETURN "". /* subscription already terminated */

      ASSIGN
         liTermReason = {&SUBSCRIPTION_TERM_REASON_DIRECT_ORDER_CANCELATION}
         ldeTS = fSecOffSet(fMakeTS(),5).
      
      liError = fDeleteMsValidation(Order.MsSeq,
                                    liTermReason,
                                    OUTPUT lcResult).
      IF liError EQ 0 THEN DO:
         lcResult = "".
         llYoigoCLI = fIsYoigoCLI(MobSub.CLI).
         llPenaltyFee = fIsPenalty(liTermReason,Order.MsSeq).
         fCheckOrderer(liTermReason, llYoigoCLI, OUTPUT lcResult).
      END.
      IF lcResult EQ "" THEN 
         fCheckKillTS(liTermReason,ldeTS, OUTPUT lcResult). 

      IF lcResult EQ "" THEN DO:
         
         fInitialiseValues(
            INPUT liTermReason,
            INPUT llYoigoCLi,
            OUTPUT liMsisdnStat,
            OUTPUT liSimStat,
            OUTPUT liQuarTime).

         IF OrderDelivery.LoStatusId = 875 THEN liSimStat = 7.
         /*YPR-2486:*/
         ELSE IF fcParamI("UseTempSimStatus") EQ 1 AND liSimStat EQ 1 THEN DO:
            liSimStat = {&SIM_SIMSTAT_TEMP}.
         END.

         IF fIsConvergenceTariff(MobSub.CLIType) 
            THEN lcTermType = {&TERMINATION_TYPE_PARTIAL}.
         ELSE lcTermType = {&TERMINATION_TYPE_FULL}.

         liReq = fTerminationRequest(
                        Order.MsSeq,
                        ldeTS,    /* when request should be handled */
                        liMsisdnStat, /* msisdn status code: available */
                        liSimStat, /* sim status code : available */
                        liQuarTime, /* quarantie time */
                        INT(llPenaltyFee), /*penalty fee calcultion */
                        "", /* opcode */
                        STRING(liTermReason), /* POS order cancelation */
                        {&REQUEST_SOURCE_ORDER_CANCELLATION},
                        "",
                        0,
                        lcTermType,
                        OUTPUT lcResult).
   
         IF liReq > 0 THEN
            fAdditionalLineSTC(liReq,
                               fMake2Dt(TODAY + 1, 0),
                               "DELETE").
      END.
      
      IF lcResult > "" THEN DO:
        DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.Custnum,
                          "SUBCRIPTION TERMINATION FAILED",
                          lcResult).
         RETURN "ERROR:Subscription termination failed:" + lcResult.
      END.

   END.
END.

RETURN "".

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.
