{Syst/tmsconst.i}

/* RepText */
FUNCTION fCreateRepText RETURNS LOGICAL
   ( iiTextType AS INTEGER,
     icLinkCode AS CHARACTER,
     iiLanguage AS INTEGER,
     icRepText  AS CHARACTER ):
   
   FIND FIRST RepText EXCLUSIVE-LOCK WHERE
      RepText.Brand     = "1"       AND
      RepText.TextType = iiTextType AND
      RepText.LinkCode = icLinkCode AND
      RepText.Language = iiLanguage AND
      RepText.ToDate   = DATE(12,31,2049)
   NO-ERROR.
   
   IF NOT AVAILABLE RepText
   THEN DO:
      CREATE RepText.
   END.
   
   ASSIGN
      RepText.Brand     = "1"
      RepText.TextType = iiTextType
      RepText.LinkCode = icLinkCode
      RepText.Language = iiLanguage
      RepText.FromDate = DATE(6,1,2017)
      RepText.ToDate   = DATE(12,31,2049)
      RepText.RepText  = icRepText.

END FUNCTION.

DEFINE STREAM instr.

INPUT STREAM instr FROM VALUE("../tms_support/2018/discount_comm_names.txt").

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.

FUNCTION fLanguageId RETURNS INTEGER 
   ( icLanguageCode AS CHARACTER ):

   DEFINE BUFFER Language FOR Language.

   FOR FIRST Language NO-LOCK WHERE
      Language.LanguageCode = icLanguageCode:
      RETURN Language.Language.
   END.

   RETURN ?.

END FUNCTION.

REPEAT:

   IMPORT STREAM instr UNFORMATTED lcLine.

   IF NUM-ENTRIES(lcLine,";") NE 3
   THEN MESSAGE "Invalid line: " + lcLine
        VIEW-AS ALERT-BOX.
    
   ELSE IF fLanguageId(ENTRY(2,lcLine,";")) EQ ?
   THEN MESSAGE "Invalid language code: " + lcLine
        VIEW-AS ALERT-BOX.

   ELSE fCreateRepText({&REPTEXT_DISCOUNTPLAN},
                       ENTRY(1,lcLine, ";"),
                       fLanguageId(ENTRY(2,lcLine,";")),
                       ENTRY(3,lcLine,";")).

END.