DEFINE VARIABLE lcLogFile           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcSpoolDir          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcOutDir            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcDel               AS CHARACTER  NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand             AS CHARACTER  NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp        AS DATETIME   NO-UNDO INIT ?.
DEFINE VARIABLE liEvents            AS INTEGER    NO-UNDO.
DEFINE VARIABLE lcMessage           AS CHARACTER  NO-UNDO.

DEFINE VARIABLE lcCustId            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcCustIdType        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE liPaymentType       AS INTEGER    NO-UNDO INIT ?.
DEFINE VARIABLE ldDeliveryTime      AS DECIMAL    NO-UNDO INIT ?.
DEFINE VARIABLE ldLastUpdateStamp   AS DECIMAL    NO-UNDO INIT ?.
DEFINE VARIABLE ldClosedTimeStamp   AS DECIMAL    NO-UNDO INIT ?.
DEFINE VARIABLE lcImei              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcTerminalBillCode  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcFormRequest       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcPortRequest       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ldPortingTime       AS DECIMAL    NO-UNDO INIT ?.
DEFINE VARIABLE lcPayTermContr      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcTermContr         AS CHARACTER  NO-UNDO.

DEFINE VARIABLE ldTermAmt           AS DECIMAL    NO-UNDO INIT ?.
DEFINE VARIABLE ldTermDiscount      AS DECIMAL    NO-UNDO INIT ?.
DEFINE VARIABLE ldTermLeasAmt       AS DECIMAL    NO-UNDO INIT ?.
DEFINE VARIABLE ldMnpUpdateSt       AS DECIMAL    NO-UNDO INIT ?.
DEFINE VARIABLE liMnpStatusCode     AS INTEGER    NO-UNDO INIT ?.
DEFINE VARIABLE ldMnpCreateStamp    AS DECIMAL    NO-UNDO INIT ?.
DEFINE VARIABLE lcMnpStatusReason   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE liOnlySimAmt        AS INTEGER    NO-UNDO.
DEFINE VARIABLE lcTopUpScheme       AS CHARACTER  NO-UNDO.

{cparam2.i}
{timestamp.i}
{ftransdir.i}
{tmsconst.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/order_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fToUTF8 RETURNS CHARACTER 
         (INPUT pcText AS CHARACTER):

   RETURN  CODEPAGE-CONVERT(pcText,"UTF-8",SESSION:CHARSET).

END FUNCTION.

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH Order WHERE
         Order.Brand = gcBrand NO-LOCK:

   ASSIGN
      lcCustId = ""
      lcCustIdType = ""
      liPaymentType = ?
      ldDeliveryTime = ?
      ldLastUpdateStamp = ?
      ldClosedTimeStamp = ?
      lcImei = ""
      lcTerminalBillCode = ""
      lcFormRequest = ""
      lcPortRequest = ""
      ldPortingTime = ?
      lcPayTermContr = ""
      lcTermContr = ""
      ldTermAmt = ?
      ldTermDiscount = ?
      ldTermLeasAmt = ?
      ldMnpUpdateSt = ?
      liMnpStatusCode = ?
      ldMnpCreateStamp = ?
      lcMnpStatusReason = "".
 
   /* custid custidtype */
   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand   = gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
   IF AVAILABLE OrderCustomer THEN
      ASSIGN lcCustId     = OrderCustomer.CustId
             lcCustIdType = OrderCustomer.CustIdType.

   /* payment_type */
   FIND FIRST OrderPayment WHERE
              OrderPayment.Brand   = gcBrand AND
              OrderPayment.OrderId = Order.OrderId 
              NO-LOCK NO-ERROR.
   IF AVAILABLE OrderPayment THEN
      ASSIGN liPaymentType = OrderPayment.Method.

   /* delivery_time  */
   FIND FIRST OrderTimeStamp WHERE
              OrderTimeStamp.Brand   = gcBrand AND
              OrderTimeStamp.OrderId = Order.OrderId AND
              OrderTimeStamp.RowType = {&ORDERTIMESTAMP_DELIVERY}
              NO-LOCK NO-ERROR.
   IF AVAILABLE OrderTimeStamp THEN
      ASSIGN ldDeliveryTime = OrderTimeStamp.Timestamp.

   /* last_update_stamp */
   FIND FIRST OrderTimeStamp WHERE
              OrderTimeStamp.Brand   = gcBrand AND
              OrderTimeStamp.OrderId = Order.OrderId AND
              OrderTimeStamp.RowType = {&ORDERTIMESTAMP_CHANGE}
              NO-LOCK NO-ERROR.
   IF AVAILABLE OrderTimeStamp THEN 
      ASSIGN ldLastUpdateStamp = OrderTimeStamp.TimeStamp.
  
   /* closed_time */
   FIND FIRST OrderTimeStamp WHERE
              OrderTimeStamp.Brand   = gcBrand AND
              OrderTimeStamp.OrderId = Order.OrderId AND
              OrderTimeStamp.RowType = {&ORDERTIMESTAMP_CLOSE}
              NO-LOCK NO-ERROR.
   IF AVAILABLE OrderTimeStamp THEN
      ASSIGN ldClosedTimeStamp = OrderTimeStamp.TimeStamp.

   /* IMEI */
   FIND FIRST OrderAccessory WHERE
              OrderAccessory.Brand        = gcBrand AND
              OrderAccessory.OrderId      = Order.OrderId AND
              OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} 
              NO-LOCK NO-ERROR.
   IF AVAILABLE OrderAccessory THEN
      ASSIGN lcImei             = OrderAccessory.IMEI
             lcTerminalBillCode = OrderAccessory.ProductCode
             ldTermAmt          = OrderAccessory.Amount
             ldTermDiscount     = OrderAccessory.Discount.
   /* MNP requests  */
   FOR EACH MNPProcess WHERE
            MNPProcess.OrderId = Order.OrderId
            NO-LOCK BY CrStamp DESC:
      ASSIGN lcFormRequest     = MNPProcess.FormRequest
             lcPortRequest     = MNPProcess.PortRequest
             ldPortingTime     = MNPProcess.PortingTime
             ldMnpUpdateSt     = MNPProcess.UpdateTS
             liMnpStatusCode   = MNPProcess.StatusCode
             ldMnpCreateStamp  = MNPProcess.CreatedTS
             lcMnpStatusReason = MNPProcess.StatusReason.

      LEAVE.
   END.

   /* contracts  */
   IF Order.Offer > "" THEN DO:

      FIND FIRST OfferItem WHERE
                 OfferItem.Brand       = gcBrand AND
                 OfferItem.Offer       = Order.Offer AND
                 OfferItem.ItemType    = "PerContract" AND
                 OfferItem.ItemKey     BEGINS "PAYTERM" AND
                 OfferItem.EndStamp   >= Order.CrStamp  AND
                 OfferItem.BeginStamp <= Order.CrStamp  NO-LOCK NO-ERROR.

      IF AVAIL OfferItem THEN
         ASSIGN lcPayTermContr = OfferItem.ItemKey
                ldTermLeasAmt  = OfferItem.Amount.

      FIND FIRST OfferItem WHERE
                 OfferItem.Brand       = gcBrand AND
                 OfferItem.Offer       = Order.Offer AND
                 OfferItem.ItemType    = "PerContract" AND
                 OfferItem.ItemKey     BEGINS "TERM" AND
                 OfferItem.EndStamp   >= Order.CrStamp AND
                 OfferItem.BeginStamp <= Order.CrStamp NO-LOCK NO-ERROR.

      IF AVAIL OfferItem THEN
         lcTermContr = OfferItem.ItemKey.
      
      /* TopUp  */
      FOR EACH OfferItem NO-LOCK WHERE
               OfferItem.Brand       = gcBrand AND
               OfferItem.Offer       = Order.Offer AND
               OfferItem.ItemType    = "BillItem" AND
               OfferItem.EndStamp   >= Order.CrStamp AND
               OfferItem.BeginStamp <= Order.CrStamp,
         FIRST BillItem NO-LOCK WHERE
               BillItem.Brand    = gcBrand AND
               BillItem.BillCode = OfferItem.ItemKey,
         FIRST BitemGroup NO-LOCK WHERE
               BitemGroup.Brand   = gcBrand AND
               BitemGroup.BIGroup = BillItem.BIGroup AND
               BItemGroup.BIGroup EQ "9":

            liOnlySimAmt = OfferItem.Amount.
            LEAVE.
      END.

      FOR FIRST OfferItem NO-LOCK WHERE
                OfferItem.Brand       = gcBrand AND
                OfferItem.Offer       = Order.Offer AND
                OfferItem.ItemType    = "TopUp" AND
                OfferItem.EndStamp   >= Order.CrStamp AND
                OfferItem.BeginStamp <= Order.CrStamp,
         FIRST TopupScheme NO-LOCK WHERE
               TopupScheme.Brand       = gcBrand AND
               TopupScheme.TopupScheme = OfferItem.ItemKey:
         
         lcTopUpScheme = TopupScheme.TopupScheme.
      END.

   END.

   ASSIGN liEvents  = liEvents + 1
          lcMessage = "Order"                                  + lcDel +
                      "CREATE"                                 + lcDel +
                      fNotNull(STRING(RECID(Order)))           + lcDel +
                      fNotNull(STRING(Order.OrderID))          + lcDel +
                      fNotNull(STRING(ldtTimeStamp))           + lcDel +
                      fNotNull(STRING(Order.OrderID))          + lcDel +
                      fNotNull(Order.CLI)                      + lcDel +
                      fNotNull(STRING(Order.MSSeq))            + lcDel +
                      fNotNull(STRING(Order.OrderType))        + lcDel +
                      fNotNull(Order.OrderChannel)             + lcDel +
                      fNotNull(Order.StatusCode)               + lcDel +
                      fNotNull(Order.ContractID)               + lcDel +
                      fNotNull(Order.Salesman)                 + lcDel +
                      fNotNull(Order.Campaign)                 + lcDel +
                      fNotNull(STRING(Order.CrStamp))          + lcDel +
                      fNotNull(Order.Referee)                  + lcDel +
                      fNotNull(STRING(liPaymentType))          + lcDel +
                      fNotNull(STRING(ldDeliveryTime))         + lcDel +
                      fNotNull(STRING(ldLastUpdateStamp))      + lcDel +
                      fNotNull(STRING(ldClosedTimeStamp))      + lcDel +
                      fNotNull(Order.ICC)                      + lcDel +
                      fNotNull(STRING(Order.PayType))          + lcDel +
                      fNotNull(Order.CurrOper)                 + lcDel +
                      fNotNull(Order.OldIcc)                   + lcDel +
                      fNotNull(STRING(Order.OldPayType))       + lcDel +
                      fNotNull(Order.CLIType)                  + lcDel +
                      fNotNull(Order.Offer)                    + lcDel +
                      fNotNull(lcPayTermContr)                 + lcDel +
                      fNotNull(lcTermContr)                    + lcDel +
                      fNotNull(STRING(Order.DeliveryType))     + lcDel +
                      fNotNull(STRING(Order.DeliverySecure))   + lcDel +
                      fNotNull(lcImei)                         + lcDel +
                      fNotNull(lcTerminalBillCode)             + lcDel +
                      fNotNull(STRING(liMnpStatusCode))        + lcDel +
                      fNotNull(lcFormRequest)                  + lcDel +
                      fNotNull(lcPortRequest)                  + lcDel +
                      fNotNull(STRING(ldPortingTime))          + lcDel +
                      fNotNull(lcCustId)                       + lcDel +
                      fNotNull(lcCustIdType)                   + lcDel +
                      fNotNull(STRING(ldTermAmt))              + lcDel +
                      fNotNull(STRING(ldTermDiscount))         + lcDel +
                      fNotNull(STRING(ldTermLeasAmt))          + lcDel +
                      fNotNull(STRING(Order.PortingDate))      + lcDel +
                      fNotNull(STRING(ldMnpUpdateSt))          + lcDel +
                      fNotNull(STRING(ldMnpCreateStamp))       + lcDel +
                      fNotNull(lcMnpStatusReason)              + lcDel +
                      fNotNull(STRING(liOnlySimAmt))           + lcDel +
                      fNotNull(lcTopUpScheme).

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "Order" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   lcMessage = fToUTF8(lcMessage).

   PUT STREAM slog UNFORMATTED lcMessage SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).
