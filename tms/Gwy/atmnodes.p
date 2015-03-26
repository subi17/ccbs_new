DEFINE VARIABLE lcValue AS CHARACTER NO-UNDO.


lcValue = UPPER(SESSION:PARAMETER).

IF lcValue NE "A" AND lcValue NE "B" AND
   lcValue NE "AB" THEN DO:

   MESSAGE
      "Value must be A,B or AB !"
   VIEW-AS ALERT-BOX TITLE lcValue.
   QUIT.

END.

FIND FIRST TMSParam WHERE
           TMSParam.Brand      = "1" AND
           TMSParam.ParamGroup = "ATM" AND
           TMSParam.ParamCode  = "Nodes"
EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL TMSParam THEN DO:
   TMSParam.CharVal = SESSION:PARAMETER.
   MESSAGE
      "Parameter is set to:" TMSParam.CharVal SKIP
      "Press ENTER to quit"
   VIEW-AS ALERT-BOX.
END.

QUIT.
