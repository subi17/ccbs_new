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
fUpdateInvText("SMS", "STC_Requested", 1, "stc/requested", "cliType=#CLITYPE|date=#DATE").
fUpdateInvText("SMS", "STC_Requested", 2, "stc/requested", "cliType=#CLITYPE|date=#DATE").
fUpdateInvText("SMS", "STC_Requested", 3, "stc/requested", "cliType=#CLITYPE|date=#DATE").
fUpdateInvText("SMS", "STC_Requested", 4, "stc/requested", "cliType=#CLITYPE|date=#DATE").
fUpdateInvText("SMS", "STC_Requested", 5, "stc/requested", "cliType=#CLITYPE|date=#DATE").
*/
fUpdateInvText("SMS", "STC_Requested_Any_to_Cvg", 1, "stc/req-to-cvg", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE_28_to_29", 1, "stc/done-28-29", "monthlyCost=9|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_28_to_29", 1, "stc/req-28-29", "monthlyCost=9|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_DONE_29_to_28", 1, "stc/done-29-28", "monthlyCost=0|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_29_to_28", 1, "stc/req-29-28", "monthlyCost=0|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_DONE_28_to_NoMainline", 1, "stc/done-28-no-mainline", "monthlyCost=14|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_28_to_NoMainline", 1, "stc/req-28-no-mainline", "monthlyCost=14|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_DONE_29_to_NoMainline", 1, "stc/done-29-no-mainline", "monthlyCost=14|msisdn=#MSISDN").
fUpdateInvText("SMS", "STC_Requested_29_to_NoMainline", 1, "stc/req-29-no-mainline", "monthlyCost=14|msisdn=#MSISDN").

