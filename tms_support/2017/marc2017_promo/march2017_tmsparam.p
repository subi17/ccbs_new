
FIND FIRST TMSParam NO-LOCK WHERE
           TMSParam.Brand EQ "1" AND
           TMSParam.ParamGroup EQ "March2017Promo" AND
           TMSParam.ParamCode EQ "March2017LogDir"  NO-ERROR.
IF AVAIL TMSParam THEN
   message TMSParam.ParamCode + " already found" VIEW-AS ALERT-BOX.
ELSE DO:
   CREATE TMSParam.

   ASSIGN TMSParam.Brand = "1"
          TMSParam.ParamGroup = "March2017Promo"
          TMSParam.ParamCode = "March2017LogDir"
          TMSParam.ParamName = "Log directory for files March2017Promo"
          TMSParam.CharVal = "/store/riftp/upsells/outgoing/logs"
          TMSParam.ParamType = "C".
END.
