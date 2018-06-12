/*-----------------------------------------------------------------------------
  MODULE .......: digital_signature.i
  FUNCTION .....: RES-538 Digital Signature functions.
  SOVELLUTUS ...: TMS
  AUTHOR .......: kahannul
  CREATED ......: 
  CHANGED.. ....: 14.02.18
              
  Version ......: 
  -------------------------------------------------------------------------- */

&IF "{&digital_signature}" NE "YES"
&THEN

&GLOBAL-DEFINE digital_signature YES

{Mc/offer.i}

/*
   Function for Digital Signature checks order status and ActionLog. Creates 
   new ActionLog row with ActionTS = 0. ActionLog will be searched by cron and 
   ActionTS is set to time value when signature request or signature cancel request
   has been sent to Adapter (Signature API).
*/
FUNCTION fHandleSignature RETURNS CHAR
   (iiOrderId AS INT,
    icStatus  AS CHAR):

   DEF VAR lcStatus AS CHAR NO-UNDO INIT "".
   DEF VAR lcActionID AS CHAR NO-UNDO INIT "".
   DEF VAR ldeInstallment AS DECIMAL NO-UNDO.
   DEF VAR ldeMonthlyFee  AS DECIMAL NO-UNDO.
   DEF VAR liMonths AS INT NO-UNDO INIT 0.
   DEF VAR ldeFinalFee AS DECIMAL NO-UNDO.
   DEF VAR lcSignatureStatus AS CHAR NO-UNDO INIT "".
   DEF VAR lcExcludedDSStatuses AS CHAR NO-UNDO.

   DEF BUFFER bOrder FOR Order.
   DEF BUFFER bActionLog FOR ActionLog.

   FIND FIRST bOrder NO-LOCK WHERE
              bOrder.Brand EQ Syst.Var:gcBrand AND
              bOrder.OrderId EQ iiOrderId NO-ERROR.

   IF NOT AVAIL bOrder THEN
      RETURN "Error".

   IF bOrder.OrderType NE {&ORDER_TYPE_NEW} AND
      bOrder.OrderType NE {&ORDER_TYPE_MNP} AND
      bOrder.OrderType NE {&ORDER_TYPE_RENEWAL} AND
      bOrder.OrderType NE {&ORDER_TYPE_STC} THEN RETURN "Unsupported order type".

   /* Check that we are handling Tienda or Telesales order */
   
   IF /* bOrder.Logistics NE "" AND*/
      bOrder.ContractID NE "" AND
      (LOOKUP(bOrder.OrderChannel, {&ORDER_CHANNEL_DIRECT}) > 0 OR
       LOOKUP(bOrder.OrderChannel, {&ORDER_CHANNEL_DIRECT_RENEWAL}) > 0) THEN DO:
      /* Financed orders cannot be digitally signed */
      ldeInstallment = fGetOfferDeferredPayment(bOrder.Offer,
                                                bOrder.CrStamp,
                                                OUTPUT ldeMonthlyFee,
                                                OUTPUT liMonths,
                                                OUTPUT ldeFinalFee).
      IF liMonths NE 0 THEN DO:
         RETURN "Financed". /* financed, not error */
      END.
   END.
   ELSE
      RETURN "Not Telesales". /* not error */

   /* Process ActionLog */
   ASSIGN
      /* logic changed and using excluded statuses now */
      lcExcludedDSStatuses = fCParam("SignatureApi", "ExcludedStatuses").

   IF LOOKUP(icStatus, lcExcludedDSStatuses) = 0 THEN
      lcActionID = "dssent".
   ELSE IF LOOKUP(icStatus, {&ORDER_CLOSE_STATUSES}) > 0 THEN DO: /* 7,8,9 */
      /* send cancel only if dssent sent */
      IF NOT CAN-FIND (FIRST bActionLog NO-LOCK WHERE
                             bActionLog.Brand     = Syst.Var:gcBrand AND
                             bActionLog.TableName = "Order" AND
                             bActionLog.KeyValue  = STRING(bOrder.OrderId) AND
                             bActionLog.ActionID  = "dssent") THEN 
         RETURN "No need to send".
         lcActionID = "dscancel".
   END.
   ELSE DO:
      RETURN "Error".
   END.

   /* Check if already sent. No need to resend */
   FIND FIRST bActionLog EXCLUSIVE-LOCK WHERE
              bActionLog.Brand     = Syst.Var:gcBrand AND
              bActionLog.TableName = "Order" AND
              bActionLog.KeyValue  = STRING(bOrder.OrderId) AND
              bActionLog.ActionID  = lcActionID /* AND
              bActionLog.ActionTS  = DEC(0)*/ USE-INDEX TableName NO-ERROR.
   IF NOT AVAIL bActionLog THEN DO:
      CREATE bActionLog.
      ASSIGN
         bActionLog.Brand        = Syst.Var:gcBrand
         bActionLog.ActionID     = lcActionID
         bActionLog.ActionTS     = 0
         bActionLog.TableName    = "Order"
         bActionLog.KeyValue     = STRING(bOrder.OrderId)
         bActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}
         bActionLog.UserCode     = Syst.Var:katun
         bActionLog.CustNum      = bOrder.CustNum
         bActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
         bActionLog.toDate       = TODAY
         bActionLog.ActionChar   = icStatus.

      lcStatus = "".
   END.
   ELSE DO:
      lcStatus = "Already sent". /* not error */
   END.
   RELEASE bActionLog.

   RETURN lcStatus.

END FUNCTION.

&ENDIF.
