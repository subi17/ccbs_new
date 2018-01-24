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
/*
fUpdateInvText("SMS", "STC_DONE", 1, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 2, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 3, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 4, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 5, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_Requested", 1, "stc/req", "cliType=#CLITYPE|actDate=#DATE").
fUpdateInvText("SMS", "STC_Requested", 2, "stc/req", "cliType=#CLITYPE|actDate=#DATE").
fUpdateInvText("SMS", "STC_Requested", 3, "stc/req", "cliType=#CLITYPE|actDate=#DATE").
fUpdateInvText("SMS", "STC_Requested", 4, "stc/req", "cliType=#CLITYPE|actDate=#DATE").
fUpdateInvText("SMS", "STC_Requested", 5, "stc/req", "cliType=#CLITYPE|actDate=#DATE").
*/
fUpdateInvText("SMS", "STC_Requested_Any_to_Cvg", 1, "stc/reqtocvg", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE_28_to_29", 1, "stc/done2829", "monthlyCost=9|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_28_to_29", 1, "stc/req2829", "monthlyCost=9|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_DONE_29_to_28", 1, "stc/done2928", "monthlyCost=0|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_29_to_28", 1, "stc/req2928", "monthlyCost=0|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_DONE_28_to_NonCvg", 1, "stc/done28noncvg", "monthlyCost=14|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_28_to_NonCvg", 1, "stc/req28noncvg", "monthlyCost=14|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_DONE_29_to_NonCvg", 1, "stc/done29noncvg", "monthlyCost=14|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_29_to_NonCvg", 1, "stc/req29noncvg", "monthlyCost=14|msisdn=#MSISDN").

