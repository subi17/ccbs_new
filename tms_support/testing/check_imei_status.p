DEFINE VARIABLE ttOrderId AS INTEGER FORMAT "zzzzzzzz" NO-UNDO.

UPDATE ttorderid LABEL "OrderId".

FIND OrderAccessory WHERE
   OrderAccessory.OrderId = ttorderId NO-LOCK.

   DISPLAY
        Amount
        VatAmount
        ProductCode
        IMEI
        Brand
        Discount
        Model
        ModelColor
        Manufacturer
        SIMLockCode
        TerminalType
        IMEIStatus
        IMEIReleased
   WITH 1 COL.

