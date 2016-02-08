/**
 * Cancel new orders in POS channel created on the same day.
 * Cancel renewal orders created in POS channel.
 * Called only from POS tool.
 *
 * @input    orderid;int;order id 
 * @output   success;boolean
 */

{xmlrpc/xmlrpc_access.i} 
{Syst/commpaa.i}
gcBrand = "1".
{Func/timestamp.i}
{Func/fsubstermreq.i}
{Syst/tmsconst.i}
{Func/msisdn_prefix.i}
{Func/fmakemsreq.i}
{Func/ordercancel.i}

/* Input parameters */
DEF VAR piOrderId    AS INT  NO-UNDO.
/* Local variables */
DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liError AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeToday AS DECIMAL NO-UNDO.
DEFINE VARIABLE liReq AS INTEGER NO-UNDO. 
DEFINE VARIABLE llCloseOrder AS LOGICAL NO-UNDO.
/* Output parameters */
DEF BUFFER bSubMsRequest FOR MsRequest.

IF validate_request(param_toplevel_id, "int") = ? THEN RETURN.
piOrderId = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* check order exist and is delivered */
FIND Order WHERE
     Order.Brand = gcBrand AND
     Order.OrderId = piOrderId NO-LOCK NO-ERROR.
IF NOT AVAIL Order THEN 
   RETURN appl_err("Order not found").

ASSIGN
   katun = "VISTA_" + Order.Salesman
   llCloseOrder = FALSE.
  
/* check that order is from today and coming from POS channel */
IF Order.OrderType = {&ORDER_TYPE_NEW} THEN DO:
   ldeToday = fHMS2TS(TODAY,"00:00:00").
   IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) = 0 THEN
      RETURN appl_err("Order channel is not POS").
   ELSE IF Order.CrStamp < ldeToday THEN
      RETURN appl_err("Order is not from today date").

   IF Order.StatusCode NE {&ORDER_STATUS_DELIVERED} THEN llCloseOrder = TRUE.
   ELSE RUN pTerminateSubscription(OUTPUT liReq).

END. /* IF Order.OrderType = {&ORDER_TYPE_NEW} THEN DO: */
ELSE IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN DO:

   IF NOT Order.OrderChannel BEGINS "RENEWAL_POS" THEN
      RETURN appl_err("Order channel is not renewal POS").

   IF Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} THEN DO:

      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq      = Order.MSSeq  AND
                 MsRequest.ReqType    = {&REQTYPE_AFTER_SALES_ORDER} AND
                 MsRequest.ReqIParam1 = Order.OrderId NO-LOCK NO-ERROR.

      IF AVAIL MsRequest THEN
         FOR EACH bSubMsRequest NO-LOCK WHERE
            bSubMsRequest.OrigRequest = MsRequest.MsRequest AND
           (bSubMsRequest.ReqType     = {&REQTYPE_CONTRACT_ACTIVATION} OR
            bSubMsRequest.ReqType     = {&REQTYPE_CONTRACT_TERMINATION}) AND
           LOOKUP(STRING(bSubMsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0:

            /* pending q25 extension will be cancelled in the handing phase */
            IF bSubMsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
               bSubMsRequest.ReqStatus EQ 0 AND
               bSubMsRequest.ReqCparam3 EQ "RVTERM12" THEN NEXT.

            RETURN appl_err("After sales request is ongoing").
         END.

      RUN pCancelRenewalOrder(OUTPUT liReq).

   END.
   ELSE IF Order.StatusCode EQ {&ORDER_STATUS_ONGOING} THEN 
      RETURN appl_err("After sales request is ongoing").
   ELSE llCloseOrder = TRUE.
END.
ELSE RETURN appl_err("Invalid Order type").

IF llCloseOrder THEN DO:
   RUN Mc/closeorder.p (INPUT piOrderId, INPUT TRUE).
   ocResult = RETURN-VALUE. 
   IF ocResult NE "" THEN 
     RETURN appl_err(ocResult).
   ELSE DO:
      fReleaseImei(Order.OrderId).
      add_boolean(response_toplevel_id, "", true).
   END.
  RETURN .
END. /* IF llCloseOrder THEN DO: */

IF liReq > 0 THEN
   add_boolean(response_toplevel_id, "", true).
ELSE
   add_boolean(response_toplevel_id, "", false).


PROCEDURE pTerminateSubscription:

  DEF OUTPUT PARAMETER oiReq AS INT NO-UNDO.

  DEFINE VARIABLE ldeTS AS DECIMAL NO-UNDO.
  DEFINE VARIABLE llYoigoCLi AS LOGICAL NO-UNDO. 
  DEFINE VARIABLE liMsisdnStat AS INTEGER NO-UNDO. 
  DEFINE VARIABLE liSimStat AS INTEGER NO-UNDO. 
  DEFINE VARIABLE liQuarTime AS INTEGER NO-UNDO. 
  DEFINE VARIABLE llPenaltyFee AS LOGICAL NO-UNDO. 
  DEFINE VARIABLE liTermReason AS INT NO-UNDO. 

  /* terminate the subscription */
  ASSIGN liTermReason = {&SUBSCRIPTION_TERM_REASON_POS_ORDER_CANCELATION}
         ldeTS = fMakeTS().

  /* some validations*/
  liError = fDeleteMsValidation(Order.MsSeq,ocResult).
  IF liError EQ 3 THEN RETURN appl_err("Ongoing termination requests"). 
  IF liError NE 0 THEN RETURN appl_err(ocResult). 

  FIND FIRST MobSub WHERE
             MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.

  llYoigoCLI = fIsYoigoCLI(MobSub.CLI).
  llPenaltyFee = fIsPenalty(liTermReason,Order.MsSeq).
  liError = fCheckOrderer(liTermReason, llYoigoCLI, ocResult).
  IF liError NE 0 THEN RETURN appl_err(ocResult).

  liError = fCheckKillTS(liTermReason,ldeTS, OUTPUT ocResult).
  IF liError NE 0 THEN RETURN appl_err(ocResult).

  fInitialiseValues(INPUT liTermReason,
                    INPUT llYoigoCLi,
                    OUTPUT liMsisdnStat,
                    OUTPUT liSimStat,
                    OUTPUT liQuarTime).

  oiReq = fTerminationRequest(
               Order.MsSeq,
               ldeTS,    /* when request should be handled */
               liMsisdnStat, /* msisdn status code: available */
               liSimStat, /* sim status code : available */
               liQuarTime, /* quarantie time */
               INT(llPenaltyFee), /*penalty fee calcultion */
               "", /* opcode */
               STRING(liTermReason), /* POS order cancelation */
               "6",
               "",
               0,
               OUTPUT ocResult).
END PROCEDURE.

PROCEDURE pCancelRenewalOrder:

  DEF OUTPUT PARAMETER oiReq AS INT NO-UNDO.

  oiReq = fRevertRenewalOrderRequest(
               Order.MsSeq,
               Order.OrderId,
               "",
               fMakeTS(),
               "6",
               OUTPUT ocResult).
END PROCEDURE.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
