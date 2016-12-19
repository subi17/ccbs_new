CREATE TMSCodes.
ASSIGN
   TMSCodes.TableName = "Order"
   TMSCodes.FieldName = "DeliveryType"
   TMSCodes.CodeGroup = "Orders"
   TMSCodes.CodeValue = "5"
   TMSCodes.CodeName = "Delivery to POS secure"
   TMSCodes.InUse = 1
   .

CREATE TMSCodes.
ASSIGN
   TMSCodes.TableName = "Order"
   TMSCodes.FieldName = "DeliveryType"
   TMSCodes.CodeGroup = "Orders"
   TMSCodes.CodeValue = "6"
   TMSCodes.CodeName = "Delivery to POS"
   TMSCodes.InUse = 1
   .

CREATE MenuText.
ASSIGN
   MenuText.MenuNum  = 9846
   MenuText.MenuText = "SECPOS  ADDRESS "
   .