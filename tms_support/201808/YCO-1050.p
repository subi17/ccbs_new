/*---------------------------------------------------------------------- 
YCO-1050. New tariffs mapping for PRO migration 
/tms_support/201808/YCO-1050.p 
jotorres 23.08.2018
-----------------------------------------------------------------------*/

FIND TMSParam WHERE 
     TMSParam.Brand      EQ Syst.Var:gcBrand AND 
     TMSParam.ParamGroup EQ "YPRO"           AND 
     TMSParam.ParamCode  EQ "STCMappingForActiveTariffs" NO-ERROR. 
IF NOT AVAILABLE TMSParam THEN DO:
   MESSAGE "ERROR: TMSParam not found" VIEW-AS ALERT-BOX.
   RETURN.
END.  

/* To avoid more than 1 execution of he script */
IF LOOKUP("CONT15,CONT26=CONT37", TMSParam.CharVal, "|") > 0 THEN 
   RETURN.
 
/* Updating parameter */ 
ASSIGN    
   TMSParam.CharVal = REPLACE(TMSParam.CharVal, "CONT23,CONT33=CONT26|","CONT23,CONT33=CONT38|")
   TMSParam.CharVal = REPLACE(TMSParam.CharVal, "CONT34=CONT15|","CONT34=CONT36|")        
   TMSParam.CharVal = "CONT15,CONT26=CONT37|CONT25=CONT39|" + TMSParam.CharVal.  
   
RELEASE TMSParam.  
