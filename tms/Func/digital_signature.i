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
{Syst/commali.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/forderstamp.i}
{Func/dextra.i}
{Func/cparam2.i}
{Func/main_add_lines.i}
{Func/msisdn.i}
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

   DEF BUFFER bOrder FOR Order.
   DEF BUFFER bActionLog FOR ActionLog.

   FIND FIRST bOrder NO-LOCK WHERE
              bOrder.Brand EQ Syst.Var:gcBrand AND
              bOrder.OrderId EQ iiOrderId NO-ERROR.

   IF AVAIL bOrder THEN 
         bOrder.StatusCode = icStatus.

   IF NOT AVAIL bOrder THEN
      RETURN "Error".

   /* Check that we are handling Tienda or Telesales order */
   
   IF bOrder.Logistics NE "" AND
      bOrder.ContractID NE "" AND
      LOOKUP(bOrder.OrderChannel, "Self,TeleSales") > 0 THEN DO:
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

   /* Process ActionLog */

   IF bOrder.statusCode EQ {&ORDER_STATUS_DELIVERED} THEN
      lcActionID = "ContractStatusSent".
   ELSE IF LOOKUP(bOrder.StatusCode, {&ORDER_CLOSE_STATUSES}) > 0 THEN /* 7,8,9 */
      lcActionID = "ContractStatusCancelled".
   ELSE DO:
      RETURN "Error".
   END.

   /* Check if already sent. No need to resend */
   FIND FIRST bActionLog EXCLUSIVE-LOCK WHERE
              bActionLog.Brand     = Syst.Var:gcBrand AND
              bActionLog.TableName = "Order" AND
              bActionLog.KeyValue  = STRING(bOrder.OrderId) AND
              bActionLog.ActionID  = lcActionID /* AND
              bActionLog.ActionTS  = DEC(0)*/ AND
              bActionLog.CustNum   = bOrder.CustNum USE-INDEX TableName NO-ERROR.
   IF NOT AVAIL bActionLog THEN DO:
      CREATE ActionLog.
         ASSIGN
            ActionLog.Brand        = Syst.Var:gcBrand
            ActionLog.ActionID     = lcActionID
            ActionLog.ActionTS     = 0
            ActionLog.TableName    = "Order"
            ActionLog.KeyValue     = STRING(bOrder.OrderId)
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}
            ActionLog.UserCode     = Syst.Var:katun
            ActionLog.CustNum      = bOrder.CustNum
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
            ActionLog.toDate       = TODAY.

      lcStatus = "".
   END.
   ELSE DO:
      lcStatus = "Already sent". /* not error */
   END.
   RELEASE bActionLog.

   RETURN lcStatus.

END FUNCTION.

&ENDIF.
