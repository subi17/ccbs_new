FUNCTION fUpdateInvText RETURNS LOGICAL
   (icTarget     AS CHARACTER,
    icKeyValue   AS CHARACTER,
    iiLanguage   AS INTEGER,
    icTemplateID AS CHARACTER,
    icParamKey   AS CHARACTER ):

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

fUpdateInvText("SMS", "STC_DONE", 1, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 2, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 3, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 4, "stc/done", "cliType=#CLITYPE").
fUpdateInvText("SMS", "STC_DONE", 5, "stc/done", "cliType=#CLITYPE").
