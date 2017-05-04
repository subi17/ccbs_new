FUNCTION fCreateTMSParam RETURNS LOGICAL
   ( icGroup AS CHARACTER,
     icCode  AS CHARACTER,
     icParamType AS CHARACTER,
     icParamValue AS CHARACTER ):

   FIND TMSParam NO-LOCK WHERE
      TMSParam.Brand = "1" AND
      TMSParam.ParamGroup = icGroup AND
      TMSParam.ParamCode = icCode
   NO-ERROR.
   
   IF NOT AVAILABLE TMSParam
   THEN DO:
      CREATE TMSParam.
   END.
   
   ASSIGN
      TMSParam.Brand = "1"
      TMSParam.ParamGroup = icGroup
      TMSParam.ParamCode = icCode
      TMSParam.ParamType = icParamType
      .
  
   CASE IcParamType:
      WHEN "I"
      THEN TMSParam.IntVal = INTEGER(icParamValue).
      WHEN "C"
      THEN TMSParam.CharVal = icParamValue.
   END CASE.

   RETURN FALSE.

END FUNCTION.


fCreateTMSParam("MMan.Interface","MMan.Queue.Host","C","172.20.51.97").
fCreateTMSParam("MMan.Interface","MMan.Queue.Port","I","61613").
fCreateTMSParam("MMan.Interface","MMan.Queue.Username","C","tms").
fCreateTMSParam("MMan.Interface","MMan.Queue.Password","C","aMSqevibpMFUdv").
fCreateTMSParam("MMan.Interface","MMan.Queue.Queue","C","/exchange/message-manager.input/v1").
fCreateTMSParam("MMan.Interface","MMan.Queue.TransactSize","I","5000").
