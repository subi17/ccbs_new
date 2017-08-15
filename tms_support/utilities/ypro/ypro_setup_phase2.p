/* YPRO phase 2 setups */

FUNCTION fUpdateTMSParam RETURNS LOGICAL (INPUT icParam AS CHAR,
                                       INPUT icgroup AS CHAR,
                                       INPUT icValue AS CHAR):
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramcode EQ icParam NO-ERROR.
   IF AVAIL TMSParam THEN DO:
         IF LOOKUP(icValue, TMSParam.charval) EQ 0 THEN
            TMSParam.charval = TMSParam.charval + "," + icValue.
   END.
   RETURN TRUE.
END FUNCTION.


fUpdateTMSParam("PRO_CHANNELS", "YPRO", "POS_PRO").
fUpdateTMSParam("PRO_CHANNELS", "YPRO", "Fusion_POS_PRO").

