FUNCTION fPopulateLangCode RETURNS LOGICAL
   (iiLangID AS INTEGER,
    icLangCode AS CHARACTER ):

   FIND Language EXCLUSIVE-LOCK WHERE Language.Language = iiLangID NO-ERROR.
   
   IF NOT AVAILABLE Language 
   THEN RETURN FALSE.
   
   Language.LanguageCode = icLangCode.
   
   RETURN TRUE.

END FUNCTION.


fPopulateLangCode(1, "es").
fPopulateLangCode(2, "ca").
fPopulateLangCode(3, "eu").
fPopulateLangCode(4, "gl").
fPopulateLangCode(5, "en").

FOR EACH InvText EXCLUSIVE-LOCK TRANSACTION:

   InvText.UseMman = FALSE.

END.