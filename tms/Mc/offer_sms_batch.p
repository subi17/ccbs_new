/* ----------------------------------------------------------------------
  module .......: offer_sms_batch.p
  task .........: Send 2nd offer sms after 72 hours or close orders
                  without response after 10 days. YPR-1875
  application ..: tms
  author .......: anttis
  created ......: 26.3.2015
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{Func/timestamp.i}
{Func/cparam2.i}
{Func/fmakesms.i}
{Syst/tmsconst.i}
{Func/msisdn_prefix.i}
{Func/fgettxt.i}
{Func/order_data.i}
{Func/smsmessage.i}

DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile         AS CHAR NO-UNDO.
DEF VAR lcMessage         AS CHAR NO-UNDO.
DEF VAR ldePickTo AS DEC NO-UNDO.
DEF VAR lcMobileNumber AS CHAR NO-UNDO.
DEF VAR ldeSMSStamp AS DEC NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR liSMSSent AS INT NO-UNDO.
DEF VAR ldeCurrStamp AS DEC NO-UNDO.
DEF VAR ldeSMSActTS AS DEC NO-UNDO.

DEF STREAM Sout.

lcLogDir = fCParam("Order","OfferSMSLogDIr").

IF lcLogDir > "" THEN DO:
   lcLogFile = lcLogDir + "offer_sms_batch_" +
               STRING(YEAR(TODAY)) +
               STRING(MONTH(TODAY),"99") +
               STRING(DAY(TODAY),"99") + "_" +
               REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

   OUTPUT STREAM Sout TO VALUE(lcLogFile).
   put stream sout unformatted
      "ORDER_ID;SMS_SENT_TO;ORDER_CREATED;NOTE" SKIP.
END.

ASSIGN
   ldePickTo = fOffSet(fMakeTs(),-72)
   ldeCurrStamp = fMakeTS().

FOR EACH Order NO-LOCK WHERE
         Order.Brand = gcBrand AND
         Order.StatusCode = {&ORDER_STATUS_OFFER_SENT} AND /* shouldn't never get this value because of YDR-2575 */
         Order.Crstamp <= ldePickTo,
   FIRST OrderCustomer NO-LOCK WHERE
         OrderCustomer.Brand = Order.Brand AND
         OrderCustomer.OrderId = Order.OrderId AND
         OrderCustomer.RowType = 1:

   ASSIGN
      liSMSSent = 0
      lcMobileNumber = "".

   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN DO:
      FIND FIRST MobSub NO-LOCK WHERE
                 MobSub.MsSeq = Order.MsSeq NO-ERROR.
      IF NOT AVAIL MobSub THEN NEXT.
   END.

   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} OR
      Order.OrderType EQ {&ORDER_TYPE_MNP} THEN
      lcMobileNumber = Order.CLI.
   ELSE IF fIsMobileNumber(OrderCustomer.MobileNumber) THEN
      lcMobileNumber = OrderCustomer.MobileNumber.
   ELSE IF fIsMobileNumber(OrderCustomer.FixedNumber) THEN
      lcMobileNumber = OrderCustomer.FixedNumber.
   ELSE NEXT.

   FOR EACH SMSMessage NO-LOCK WHERE
            SMSMessage.OrderID = Order.OrderID AND
            SMSMessage.SMSType = {&SMS_TYPE_OFFER} AND
            SMSMessage.DeliType = {&SMS_DELITYPE_OUT}:
      liSMSSent = liSMSSent + 1.
   END.

   IF fOffSet(Order.CrStamp, 10 * 24) < ldeCurrStamp THEN DO:

      RUN Mc/closeorder.p(Order.OrderId, TRUE).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         IF lcLogDir > "" THEN
            PUT STREAM Sout UNFORMATTED
            Order.OrderID ";" lcMobileNumber ";" fts2hms(Order.CrStamp) ";" RETURN-VALUE skip.
         NEXT.
      END.

      IF lcLogDir > "" THEN
         PUT STREAM Sout UNFORMATTED
         Order.OrderID ";" lcMobileNumber ";" fts2hms(Order.CrStamp) ";Order closed"  skip.

      lcMessage = fGetSMSTxt(
         (IF Order.OrderType EQ 2
          THEN "OfferRespMissingRenewal"
          ELSE "OfferRespMissingNew"),
         TODAY,
         1,
         OUTPUT ldeSMSActTS).

      IF lcMessage NE "" AND lcMessage NE ? THEN DO:

         lcMessage = REPLACE(lcMessage,"#CONTRACTID", Order.ContractId).
         fCreateSMS(OrderCustomer.Custnum,
                    lcMobileNumber,
                    Order.MsSeq,
                    Order.OrderId,
                    lcMessage,
                    "622100100",
                    {&SMS_TYPE_OFFER}).
      END.

      NEXT.
   END.

   IF liSMSSent NE 1 THEN NEXT.

   lcMessage = fGetOrderOfferSMS(Order.OrderID,
                                 FALSE).

   IF lcMessage EQ "" OR lcMessage EQ ? THEN NEXT.

   fCreateSMS(Order.CustNum,
              lcMobileNumber,
              Order.MsSeq,
              Order.Orderid,
              lcMessage,
              "622100100",
              {&SMS_TYPE_OFFER}).

   IF lcLogDir > "" THEN
      PUT STREAM Sout UNFORMATTED
         Order.OrderID ";" lcMobileNumber ";" fts2hms(Order.CrStamp) ";2nd SMS sent" SKIP.
END.

IF lcLogDir > "" THEN OUTPUT STREAM Sout CLOSE.
