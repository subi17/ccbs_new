FUNCTION fUpdateTMSParam RETURNS LOGICAL
   (icParamCode AS CHARACTER,
    icParamName AS CHARACTER,
    icCharVal   AS CHARACTER):
       
   FIND TMSParam EXCLUSIVE-LOCK WHERE
      TMSParam.ParamGroup = "DMS" AND
      TMSParam.ParamCode  = icParamCode
   NO-ERROR.
   
   IF NOT AVAILABLE TMSParam
   THEN DO:
      CREATE TMSParam.
      ASSIGN
         TMSParam.Brand      = "1"
         TMSParam.ParamGroup = "DMS"
         TMSParam.ParamCode  = icParamCode.
   END.
   
   ASSIGN
      TMSParam.CharVal   = icCharVal
      TMSParam.ParamName = icParamName
      TMSParam.ParamType = "C".

END FUNCTION.

fUpdateTMSParam("DMS_S20_T1.1", "DMS Matrix Stat 20 / Row 1.1", "1,2,3,5,9,11,13").
fUpdateTMSParam("DMS_S21_T1.1", "DMS Matrix Stat 21 / Row 1.1", "3,7,9,11,13").
fUpdateTMSParam("DMS_S33_T1.1", "DMS Matrix Stat 33 / Row 1.1", "3,7,9,11,13").
fUpdateTMSParam("DMS_S44_T1.1", "DMS Matrix Stat 44 / Row 1.1", "4,5,9,11,13").
