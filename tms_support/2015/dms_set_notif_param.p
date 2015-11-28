FUNCTION fUpdateMessageParams RETURNS CHAR
   (icCase AS CHAR, /*A*/
    icCaseVal AS CHAR): /*3*/
   FIND FIRST TMSParam NO-LOCK  WHERE
      Tmsparam.ParamGroup EQ "DMS" AND
      TMSParam.ParamCode EQ "DMSMsgID_" + icCase NO-ERROR. 
   IF AVAIL TMSParam THEN DO:   
      MESSAGE TMSParam.ParamCode  + " exists" VIEW-AS ALERT-BOX.
      RETURN TMSParam.ParamCode  + " exists".
   END.
   CREATE TMSParam.
   ASSIGN
      TMSParam.Brand = "1"
      TMSParam.CharVal = icCaseVal
      TMSParam.Online = FALSE
      TMSParam.ParamCode = "DMSMsgID_" + icCase
      TMSParam.ParamGroup = "DMS"
      TMSParam.ParamName = "DMS notification sending for status " + icCase
      TMSParam.ParamType = "C".
   
   RETURN "".
END.   

fUpdateMessageParams("A", "2").
fUpdateMessageParams("B", "5").
fUpdateMessageParams("B1", "6").
fUpdateMessageParams("C", "3").
fUpdateMessageParams("F", "9").
fUpdateMessageParams("F1", "11").
fUpdateMessageParams("G", "8").
fUpdateMessageParams("J", "10").
fUpdateMessageParams("N", "8").
fUpdateMessageParams("20", "1").
fUpdateMessageParams("21", "1").
fUpdateMessageParams("44", "1").


