/*YPR-3071* Add matrix value 9 t oall matrix entries*/

FOR EACH TmsParam WHERE 
   TMSParam.Brand EQ "1" AND
   TMSParam.ParamGroup EQ "DMS" AND
   TMSParam.ParamCode BEGINS "DMS_S":
   
   IF NOT TMSParam.CharVal MATCHES "*9*" THEN DO:
    TMSParam.CharVal   = TMSParam.CharVal + ",9".
      MESSAGE "Added 9 to " + TmsParam.ParamCode + " : " 
         + TMSParam.CharVal VIEW-AS ALERT-BOX.
   END.
 /*  DISP TMSParam.*/

END.


