{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{Func/order_data.i}
{Func/msisdn_prefix.i}
{Func/smsmessage.i}

DEF VAR liOrderId LIKE Order.OrderID.
DEF VAR llFirstSMS AS LOG NO-UNDO INIT TRUE.
DEF VAR lcMobileNumber AS CHAR NO-UNDO.
DEF VAR lcOfferSMSText AS CHAR NO-UNDO.
DEF VAR llCreate AS LOG NO-UNDO.

UPDATE liOrderId  LABEL "Order ID     " SKIP
       llFirstSMS LABEL "First SMS    " SKIP
       llCreate   LABEL "Create record"
WITH FRAME row1 side-labels TITLE "Create Test Offer SMS".

IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0
   or liOrderId = 0 then return.

FIND FIRST Order WHERE
   Order.Brand = "1" and
   Order.OrderId = liOrderId NO-LOCK NO-ERROR.
IF NOT AVAILABLE Order THEN DO:
   MESSAGE  "ERROR:Order not available" VIEW-AS ALERT-BOX.
   QUIT.
END.

FIND FIRST OrderCustomer OF Order WHERE
           OrderCustomer.RowType = 1 NO-ERROR.

IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} OR
   Order.OrderType EQ {&ORDER_TYPE_MNP} THEN
   lcMobileNumber = Order.CLI.
ELSE IF fIsMobileNumber(OrderCustomer.MobileNumber) THEN
   lcMobileNumber = OrderCustomer.MobileNumber.
ELSE IF fIsMobileNumber(OrderCustomer.FixedNumber) THEN
   lcMobileNumber = OrderCustomer.FixedNumber.

IF lcMobileNumber EQ "" THEN DO:
   MESSAGE "Empty or not mobile contact number" VIEW-AS ALERT-BOX.
   RETURN.
END.

lcOfferSMSText = fGetOrderOfferSMS(Order.OrderID,
                                   llFirstSMS).

IF llCreate THEN
fCreateSMS(Order.CustNum,
           lcMobileNumber,
           Order.MsSeq,
           Order.OrderId,
           lcOfferSMSText,
           (IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL}
            THEN "622"
            ELSE "+346221001002"),
           {&SMS_TYPE_OFFER}).

RELEASE smsmessage.

MESSAGE SKIP
        "ContactNumber:" lcMobileNumber SKIP(1)
        lcOfferSMSText
VIEW-AS ALERT-BOX TITLE "MESSAGE " + STRING(llCreate,"CREATED/SIMULATED").
