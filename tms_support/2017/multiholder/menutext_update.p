FUNCTION fUpdateMenuText RETURNS LOGICAL
   (iiMenuNum  AS INTEGER,
    icMenuText AS CHARACTER):
       
   FIND MenuText EXCLUSIVE-LOCK WHERE
      MenuText.MenuNum = iiMenuNum
   NO-ERROR.
   
   IF NOT AVAILABLE MenuText
   THEN DO:
      CREATE MenuText.
      ASSIGN
         MenuText.MenuNum = iiMenuNum.
   END.
   
   ASSIGN
      MenuText.MenuText = icMenuText.

END FUNCTION.

fUpdateMenuText(2247, "USER    CUSTOMER").
fUpdateMenuText(2248, "MOBILE  DONOR").
fUpdateMenuText(2249, "FIXED   DONOR").
fUpdateMenuText(2250, "CUSTOMERMANAGEM.").
