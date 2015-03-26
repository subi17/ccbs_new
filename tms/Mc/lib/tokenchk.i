DEF VAR lcRight AS CHAR NO-UNDO.

lcRight = getTMSTableRight({1}).

if lcRight = '' THEN DO:
   MESSAGE 
      "You don't have any" SKIP
      "rights for this module" SKIP
      "Abort" 
   VIEW-AS ALERT-BOX.   
   RETURN.
END.
