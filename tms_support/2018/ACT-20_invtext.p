FUNCTION fUpdateInvText RETURNS LOGICAL
   (icTarget     AS CHARACTER,
    icKeyValue   AS CHARACTER,
    iiLanguage   AS INTEGER,
    icTemplateID AS CHARACTER,
    icParamKey   AS CHARACTER ):

   FIND LAST InvText EXCLUSIVE-LOCK WHERE
      InvText.Brand    = "1"        AND
      InvText.Target   = icTarget   AND
      InvText.KeyValue = icKeyValue AND
      InvText.FromDate <= TODAY     AND
      InvText.ToDate   >= TODAY     AND
      InvText.Language = iiLanguage
   NO-ERROR.

   IF NOT AVAILABLE InvText
   THEN DO:
      CREATE InvText.
      ASSIGN
         InvText.ITNum    = NEXT-VALUE(it-seq)
         InvText.Brand    = "1"
         InvText.Target   = icTarget
         InvText.KeyValue = icKeyValue
         InvText.FromDate = TODAY
         InvText.ToDate   = DATE(12,31,2049)
         InvText.Language = iiLanguage
         .
   END.

   ASSIGN
      InvText.TemplateID    = icTemplateID
      InvText.UseMMan       = TRUE
      InvText.ParamKeyValue = icParamKey.

   RETURN TRUE.

END FUNCTION.

fUpdateInvText("SMS", "STC_Requested_To_Convergent", 1, "stc/reqtocvg", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE_28_29", 1, "stc/done2829", "monthlyCost=#MONTHLYCOST|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_28_29", 1, "stc/req2829", "monthlyCost=#MONTHLYCOST|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_DONE_29_28", 1, "stc/done2928", "monthlyCost=#MONTHLYCOST|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_29_28", 1, "stc/req2928", "monthlyCost=#MONTHLYCOST|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_DONE_From_Convergent", 1, "stc/donefromcvg", "monthlyCost=#MONTHLYCOST|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_From_Convergent", 1, "stc/reqfromcvg", "monthlyCost=#MONTHLYCOST|msisdn=#MSISDN").


fUpdateInvText("SMS", "STC_DONE", 5, "stc/done", "cliType=#CLITYPE").



fUpdateInvText("SMS", "STC_DONE", 3, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 4, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 5, "stc/done", "cliType=#CLITYPE").
