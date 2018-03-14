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

fUpdateTMSParamC("UrlAdapter", "Adapter URL Path", "http://217.168.2.239:8080/digital-signature/").
fUpdateTMSParamC("LogDir", "Logging directory", "/scratch/log/digitalsignature/").
fUpdateTMSParamI("LogRequest", "Log Request", 1).
/* fUpdateTMSParamC("NewStatuses", "Triggered NEW statutes", "1,3,12,30").*/
fUpdateTMSParamC("NewStatuses", "Triggered NEW statutes", "1,2,3,4,6,10,12,15,16,17,22,23,30,31,32,33,50,51,73,74,75,76,77,78,79,80,99").

