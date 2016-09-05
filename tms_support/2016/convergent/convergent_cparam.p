FUNCTION faddTMSParam RETURNS LOGICAL (INPUT icBaseDCEvent AS CHAR,
                                                 INPUT icDCEvent AS CHAR,
                                                 INPUT iiUpdateMode AS INT):
   IF iiUpdateMode NE 0 THEN DO:
      FOR EACH TMSParam WHERE LOOKUP(icBaseDCEvent,
                                       tmsParam.charval) > 0:
         tmsParam.charval = tmsParam.charval + "," +
                                        icDCEvent.
      END.
   END.
   RETURN TRUE.
END FUNCTION.

/* add same as CONT24 
ALL_POSTPAID_CONTRACTS       ,CONTS2GB,CONTS8GB
BB_PROFILE_1                ,CONTS2GB,CONTS8GB
DATA_BUNDLE_BASED_CLITYPES  ,CONTS2GB,CONTS8GB 
NATIVE_VOIP_BASE_BUNDLES    not needed anymore?
POSTPAID_DATA_CONTRACTS     ,CONTS2GB,CONTS8GB
POSTPAID_VOICE_TARIFFS      ,CONTS2GB,CONTS8GB

*/
faddTMSParam("CONT24", "CONTS2GB", 1).
faddTMSParam("CONT24", "CONTS5GB", 1).
