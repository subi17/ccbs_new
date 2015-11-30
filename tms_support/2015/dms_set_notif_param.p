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

FUNCTION fUpdateOtherParamsInt RETURNS CHAR
   (icParamCode AS CHAR, /* Parameter name*/
    icParamName AS CHAR,
    iiCaseVal AS INT): /*3*/
   FIND FIRST TMSParam NO-LOCK  WHERE
      Tmsparam.ParamGroup EQ "DMS" AND
      TMSParam.ParamCode EQ icParamCode  NO-ERROR. 
   IF AVAIL TMSParam THEN DO:   
      MESSAGE TMSParam.ParamCode  + " exists" VIEW-AS ALERT-BOX.
      RETURN TMSParam.ParamCode  + " exists".
   END.
   CREATE TMSParam.
   ASSIGN
      TMSParam.Brand = "1"
      TMSParam.IntVal = iiCaseVal
      TMSParam.Online = FALSE
      TMSParam.ParamCode = icParamCode
      TMSParam.ParamGroup = "DMS"
      TMSParam.ParamName = icParamName
      TMSParam.ParamType = "I".
   
   RETURN "".
END.   


FUNCTION fUpdateOtherParamsChar RETURNS CHAR
   (icParamCode AS CHAR, /* Parameter name*/
    icParamName AS CHAR,
    icCaseVal AS CHAR): /*3*/
   FIND FIRST TMSParam NO-LOCK  WHERE
      Tmsparam.ParamGroup EQ "DMS" AND
      TMSParam.ParamCode EQ icParamCode  NO-ERROR. 
   IF AVAIL TMSParam THEN DO:   
      MESSAGE TMSParam.ParamCode  + " exists" VIEW-AS ALERT-BOX.
      RETURN TMSParam.ParamCode  + " exists".
   END.
   CREATE TMSParam.
   ASSIGN
      TMSParam.Brand = "1"
      TMSParam.CharVal = icCaseVal
      TMSParam.Online = FALSE
      TMSParam.ParamCode = icParamCode
      TMSParam.ParamGroup = "DMS"
      TMSParam.ParamName = icParamName
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


fUpdateOtherParamsInt("DMS_doc_no_provided_time", 
                       "Doc never provided waiting time", 
                       3).

fUpdateOtherParamsChar("DMS_doc_no_provided_statuses", 
                       "Doc never provided alarm statuses", 
                       "A0,C").

fUpdateOtherParamsChar("DMS_MQ", 
                       "Message Queue for DMS sms/email", 
                       "angela_in").

