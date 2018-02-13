/*
   RES-538 - Update TMSParam for Digital Signature.
*/


FUNCTION fUpdateTMSParamI RETURNS LOGICAL
   (icParamCode AS CHARACTER,
    icParamName AS CHARACTER,
    icIntVal    AS INT):

   FIND TMSParam EXCLUSIVE-LOCK WHERE
      TMSParam.ParamGroup = "SignatureApi" AND
      TMSParam.ParamCode  = icParamCode NO-ERROR.

   IF NOT AVAILABLE TMSParam THEN DO:
      CREATE TMSParam.
      ASSIGN
         TMSParam.Brand      = "1"
         TMSParam.ParamGroup = "SignatureApi"
         TMSParam.ParamCode  = icParamCode
         TMSParam.IntVal     = icIntVal
         TMSParam.ParamName  = icParamName
         TMSParam.ParamType  = "I".
   END.

END FUNCTION.


FUNCTION fUpdateTMSParamC RETURNS LOGICAL
   (icParamCode AS CHARACTER,
    icParamName AS CHARACTER,
    icCharVal   AS CHARACTER):
       
   FIND TMSParam EXCLUSIVE-LOCK WHERE
      TMSParam.ParamGroup = "SignatureApi" AND
      TMSParam.ParamCode  = icParamCode NO-ERROR.
   
   IF NOT AVAILABLE TMSParam THEN DO:
      CREATE TMSParam.
      ASSIGN
         TMSParam.Brand      = "1"
         TMSParam.ParamGroup = "SignatureApi"
         TMSParam.ParamCode  = icParamCode
         TMSParam.CharVal    = icCharVal
         TMSParam.ParamName  = icParamName
         TMSParam.ParamType  = "C".
   END.

END FUNCTION.

fUpdateTMSParamC("UrlAdapter", "Adapter URL Path", "").
fUpdateTMSParamC("SignatureApi", "LogDir", "/scratch/log/digitalsignature/").
fUpdateTMSParamI("LogRequest", "Log Request", 1).

