FUNCTION fUpdateInvText RETURNS LOGICAL
   (icTarget     AS CHARACTER, /*General*/
    icKeyValue   AS CHARACTER, /*EmailConfEInv*/
    iiLanguage   AS INTEGER,
    icTemplateID AS CHARACTER, /*EinvoiceTemplate*/
    icParamKey   AS CHARACTER ): /*Actual text*/

   DO WHILE TRUE:

      FIND LAST InvText NO-LOCK WHERE
         InvText.Brand    = "1"        AND
         InvText.Target   = icTarget   AND
         InvText.KeyValue = icKeyValue AND
         InvText.FromDate <= TODAY     AND
         InvText.ToDate   >= TODAY     AND
         InvText.Language = iiLanguage
      NO-ERROR.

      IF NOT AVAILABLE InvText AND iiLanguage NE 1
      THEN iiLanguage = 1.
      ELSE LEAVE.

   END.

   IF NOT AVAILABLE InvText
   THEN DO:
      RETURN FALSE.
   END.

   FIND CURRENT InvText EXCLUSIVE-LOCK.
   
   ASSIGN
      InvText.TemplateID    = icTemplateID
      InvText.UseMMan       = TRUE
      InvText.ParamKeyValue = icParamKey.

   RETURN TRUE.

END FUNCTION.

fUpdateInvText("SMS", "EmailConfEInv", 1, "EinvoiceTemplate", 
"MsSeq=#MSSEQ, MSISDN=#MSISDN, Amt=#AMOUNT, InvoiceDate=#INVDATE, InvoiceNumber=#INVNUM, InvoiceNumberCrypted=#INVNUMCRYPTED").

fUpdateInvText("SMS", "EmailConfEInv", 2, "EinvoiceTemplate", 
"MsSeq=#MSSEQ, MSISDN=#MSISDN, Amt=#AMOUNT, InvoiceDate=#INVDATE, InvoiceNumber=#INVNUM, InvoiceNumberCrypted=#INVNUMCRYPTED").

fUpdateInvText("SMS", "EmailConfEInv", 3, "EinvoiceTemplate", 
"MsSeq=#MSSEQ, MSISDN=#MSISDN, Amt=#AMOUNT, InvoiceDate=#INVDATE, InvoiceNumber=#INVNUM, InvoiceNumberCrypted=#INVNUMCRYPTED").

fUpdateInvText("SMS", "EmailConfEInv", 4, "EinvoiceTemplate", 
"MsSeq=#MSSEQ, MSISDN=#MSISDN, Amt=#AMOUNT, InvoiceDate=#INVDATE, InvoiceNumber=#INVNUM, InvoiceNumberCrypted=#INVNUMCRYPTED").

fUpdateInvText("SMS", "EmailConfEInv", 5, "EinvoiceTemplate", 
"MsSeq=#MSSEQ, MSISDN=#MSISDN, Amt=#AMOUNT, InvoiceDate=#INVDATE, InvoiceNumber=#INVNUM, InvoiceNumberCrypted=#INVNUMCRYPTED").

