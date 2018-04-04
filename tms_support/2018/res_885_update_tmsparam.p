/*
   RES-885 - Update TMSParam for NRTR.
*/


FUNCTION fUpdateTMSParamC RETURNS LOGICAL
   (icParamGroup AS CHARACTER,
    icParamCode  AS CHARACTER,
    icParamName  AS CHARACTER,
    icCharVal    AS CHARACTER):
       
   FIND TMSParam EXCLUSIVE-LOCK WHERE
      TMSParam.ParamGroup = icParamGroup AND
      TMSParam.ParamCode  = icParamCode NO-ERROR.
   
   IF NOT AVAILABLE TMSParam THEN DO:
      CREATE TMSParam.
      ASSIGN
         TMSParam.Brand      = "1"
         TMSParam.ParamGroup = icParamGroup
         TMSParam.ParamCode  = icParamCode
         TMSParam.CharVal    = icCharVal
         TMSParam.ParamName  = icParamName
         TMSParam.ParamType  = "C".
   END.

END FUNCTION.

fUpdateTMSParamC("NWProfileBob", "RootDir", "NW Subsc profile BOB root dir", "/store/riftp/profile/subscription/").
fUpdateTMSParamC("NWCustProfileBob", "RootDir", "NW cust profile BOB root dir" ,"/store/riftp/profile/customer/").

