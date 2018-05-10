
/* ----------------------------------------------------------------------
  MODULE .......: tmsparam_fix_ydr2851.p
  TASK .........: Add pro clitypes to tms param group ProSubsMigrationMappings 
  APPLICATION ..: TMS
  AUTHOR .......: Vandana 
  CREATED ......: 03-05-2018
  Version ......: Yoigo
----------------------------------------------------------------------- */    
DEFINE VARIABLE lcValue AS CHARACTER NO-UNDO INITIAL "CONT23,CONT33=CONT26|CONT26,CONT34=CONT15|CONTDSL2G,CONTDSL3G=CONTDSL48|CONTDSL7G=CONTDSL52|CONTFH2G_50,CONTFH3G_50=CONTFH48_50|CONTFH7G_50=CONTFH52_50|CONTFH2G_300,CONTFH3G_300=CONTFH58_300|CONTFH7G_300=CONTFH62_300|CONTFH2G_1000,CONTFH3G_1000=CONTFH76_1000|CONTFH7G_1000=CONTFH82_1000".  
DEFINE VARIABLE lcParam AS CHARACTER NO-UNDO INITIAL "STCMappingForActiveTariffs".
DEFINE VARIABLE lcGroup AS CHARACTER NO-UNDO INITIAL "YPRO".
DEFINE VARIABLE llOK    AS LOGICAL   NO-UNDO.  
  
MESSAGE "Are you sure to create TMSParam" 
    VIEW-AS ALERT-BOX QUESTION
    BUTTONS YES-NO
    SET llOk.
IF NOT llOK THEN RETURN.

FIND FIRST TMSParam 
     WHERE TMSParam.brand       EQ "1" 
       AND TMSParam.Paramcode   EQ lcParam NO-ERROR.
IF NOT AVAIL TMSParam THEN 
DO:
   CREATE TMSParam.
   ASSIGN
        TMSParam.brand      = "1"
        TMSParam.paramcode  = lcParam
        TMSParam.paramgroup = lcGroup
        TMSParam.paramname  = "PRO Contracts"
        TMSParam.paramtype  = "C"
        TMSParam.charval    = lcValue.
END.
MESSAGE "TMSParam Created successfully" 
    VIEW-AS ALERT-BOX.  


          
    