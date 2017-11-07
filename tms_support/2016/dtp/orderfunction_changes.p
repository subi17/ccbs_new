FOR FIRST OrderFunction EXCLUSIVE-LOCK WHERE
   OrderFunction.OFID = 57:

  OrderFunction.OFName = "Release in control with secure Correos".
      
END.

FIND FIRST OrderFunction EXCLUSIVE-LOCK WHERE
   OrderFunction.OFID = 58
NO-ERROR.

IF NOT AVAILABLE OrderFunction
THEN CREATE OrderFunction.
ASSIGN
   OrderFunction.OFID     = 58
   OrderFunction.OFModule = "Mc/orderinctrl.p,iiOrderId,2"
   OrderFunction.OFName   = "Release in control with secure POS"
   .


DEFINE BUFFER lbOFItem FOR OFItem.

FOR EACH OFItem NO-LOCK WHERE
   OFItem.OFID = 57:

   FIND FIRST lbOFItem NO-LOCK WHERE
      lbOFItem.OFId = 58 AND
      lbOFItem.StatusCode = OFItem.StatusCode
   NO-ERROR.

   IF AVAILABLE lbOFItem
   THEN NEXT.

   CREATE lbOFItem.
   ASSIGN
      lbOFItem.OFId = 58
      lbOFItem.StatusCode = OFItem.StatusCode.
END.
