FUNCTION faddTMSParam RETURNS LOGICAL (INPUT icBaseDCEvent AS CHAR,
                                                 INPUT icDCEvent AS CHAR,
                                                 INPUT iiUpdateMode AS INT):
   IF iiUpdateMode NE 0 THEN DO:
      FOR EACH TMSParam WHERE LOOKUP(icBaseDCEvent,
                                       tmsParam.charval) > 0:
         IF LOOKUP(icDCEvent, tmsParam.charval) > 0 THEN NEXT.
         IF tmsParam.paramcode EQ "DATA_BUNDLE_BASED_CLITYPES" THEN NEXT.
         
         tmsParam.charval = tmsParam.charval + "," + icDCEvent.
      END.
   END.
   RETURN TRUE.
END FUNCTION.

FIND FIRST TMSParam NO-LOCK WHERE
           TMSParam.Brand EQ "1" AND
           TMSParam.ParamGroup EQ "Convergent" AND
           TMSParam.ParamCode EQ "AllConvergentTariffs" NO-ERROR.
IF AVAIL TMSParam THEN 
   message TMSParam.ParamCode + " already found" VIEW-AS ALERT-BOX.
ELSE DO:
   CREATE TMSParam.
       
   ASSIGN TMSParam.Brand = "1" 
          TMSParam.ParamGroup = "Convergent"
          TMSParam.ParamCode = "AllConvergentTariffs" 
          TMSParam.ParamName = "All Convergent Tariffs"
          TMSParam.CharVal = "CONTDSL45,CONTDSL55,CONTFH45_50,CONTFH55_50,CONTFH55_300,CONTFH65_300"
          TMSParam.ParamType = "C".

END.

/* add same as CONT24 
ALL_POSTPAID_CONTRACTS       ,CONTS2GB,CONTS10GB
BB_PROFILE_1                ,CONTS2GB,CONTS10GB
DATA_BUNDLE_BASED_CLITYPES  ,CONTS2GB,CONTS10GB 
NATIVE_VOIP_BASE_BUNDLES    ,CONTS2GB,CONTS10GB not needed anymore?
POSTPAID_DATA_CONTRACTS     ,CONTS2GB,CONTS10GB
POSTPAID_VOICE_TARIFFS      ,CONTS2GB,CONTS10GB

*/

faddTMSParam("CONT24", "CONTS2GB", 0).
faddTMSParam("CONT24", "CONTS10GB", 0).

FIND FIRST TMSParam WHERE TMSParam.ParamCode EQ "DATA_BUNDLE_BASED_CLITYPES"
   NO-ERROR.

IF LOOKUP("CONTDSL45", TMSParam.charval) = 0 THEN
TMSParam.charval = tmsParam.charval + ",CONTDSL45,CONTDSL55,CONTFH45_50," +
                   "CONTFH55_50,CONTFH55_300,CONTFH65_300".

FIND FIRST TMSCodes WHERE 
           TMSCodes.tablename EQ "Daycampaign" AND
           TMSCodes.fieldname EQ "DCType" AND
           TMSCodes.codevalue EQ "9" NO-ERROR.
IF NOT AVAIL TMSCodes THEN DO:
   CREATE TMSCodes.
   ASSIGN
      TMSCodes.tablename = "Daycampaign"
      TMSCodes.fieldname = "DCType"
      TMSCodes.codevalue = "9"
      TMSCodes.codegroup = "PerContr"
      TMSCodes.codename = "Fixed line periodical contract" 
      TMSCodes.inuse = 1.


END.

