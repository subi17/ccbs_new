FUNCTION fUpdateInvText RETURNS LOGICAL
   (icTarget     AS CHARACTER, /*General*/
    icKeyValue   AS CHARACTER, /*EmailConfEInv*/
    iiLanguage   AS INTEGER,
    icTemplateID AS CHARACTER, /*EinvoiceTemplate*/
    icParamKey   AS CHARACTER ): /*Actual text*/
   DEF BUFFER bInvText FOR InvText.
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
message "1" VIEW-AS ALERT-BOX.
   END.

   IF NOT AVAILABLE InvText
   THEN DO:
   message "1b" VIEW-AS ALERT-BOX.
      FIND FIRST bInvtext NO-LOCK WHERE
                 bInvtext.target EQ "sms" AND
                 bInvText.KeyValue EQ "SMSInvoice" AND
                 bInvText.FromDate <= TODAY     AND
                 bInvText.ToDate   >= TODAY     AND
                 bInvText.Language EQ 1.
      IF AVAIL bInvText THEN DO:
         CREATE InvText.
         message "1cc" VIEW-AS ALERT-BOX.
         BUFFER-COPY bInvText except bInvText.itnum to InvText.
         message "1c" VIEW-AS ALERT-BOX.
         ASSIGN
            InvText.KeyValue = icKeyValue
            InvText.ItNum = NEXT-VALUE(it-seq)
            InvText.FromDate = TODAY
            InvText.TemplateID = icTemplateId
            InvText.UseMMan       = TRUE
            InvText.ParamKeyValue = icParamKey.
      END.

      message "Created new" + STRING(InvText.ItNum) VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.
message "2" VIEW-AS ALERT-BOX.
   FIND CURRENT InvText EXCLUSIVE-LOCK.

   ASSIGN
      InvText.TemplateID    = icTemplateID
      InvText.UseMMan       = TRUE
      InvText.ParamKeyValue = icParamKey.

   RETURN TRUE.

END FUNCTION.
/*First and last message*/
fUpdateInvText("SMS", "EInvMessageStarted", 1, "Invoice/ElectronicStarted", "").
fUpdateInvText("SMS", "EInvMessageDone", 1, "Invoice/ElectronicDone", "").
/*fUpdateInvText("SMS", "EInvMessageStarted", 2, "Invoice/ElectronicStarted", "").
fUpdateInvText("SMS", "EInvMessageDone", 2, "Invoice/ElectronicDone", "").
fUpdateInvText("SMS", "EInvMessageStarted", 3, "Invoice/ElectronicStarted", "").
fUpdateInvText("SMS", "EInvMessageDone", 3, "Invoice/ElectronicDone", "").
fUpdateInvText("SMS", "EInvMessageStarted", 5, "Invoice/ElectronicStarted", "").
fUpdateInvText("SMS", "EInvMessageDone", 5, "Invoice/ElectronicDone", "").
*/
fUpdateInvText("SMS", "EInvMessage", 1, "Invoice/Electronic", 
"Link=#LINK, MSISDN=#MSISDN, Amt=#AMOUNT, InvoiceDate=#INVDATE, InvoiceNumber=#INVNUM").
/*
fUpdateInvText("SMS", "EinvMessage", 2, "Invoice/Electronic", 
"Link=#LINK, MSISDN=#MSISDN, Amt=#AMOUNT, InvoiceDate=#INVDATE, InvoiceNumber=#INVNUM").

fUpdateInvText("SMS", "EInvMessage", 3, "Invoice/Electronic", 
"Link=#LINK, MSISDN=#MSISDN, Amt=#AMOUNT, InvoiceDate=#INVDATE, InvoiceNumber=#INVNUM").

fUpdateInvText("SMS", "EInvMessage", 5, "Invoice/Electronic", 
"Link=#LINK, MSISDN=#MSISDN, Amt=#AMOUNT, InvoiceDate=#INVDATE, InvoiceNumber=#INVNUM").

*/
