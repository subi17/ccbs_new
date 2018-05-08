
/* ----------------------------------------------------------------------
  MODULE .......: tmsparam_fix_ydr2851.p
  TASK .........: Add pro clitypes to tms param group ProSubsMigrationMappings 
  APPLICATION ..: TMS
  AUTHOR .......: Vandana 
  CREATED ......: 03-05-2018
  Version ......: Yoigo
----------------------------------------------------------------------- */
DEFINE VARIABLE lcValue AS CHARACTER NO-UNDO INITIAL "CONT15,CONT26,CONT34=CONT15|CONT25=CONT25|CONT23,CONT33=CONT26|CONTDSL39=CONTDSL39|CONTDSL2G,CONTDSL3G,CONTDSL48=CONTDSL48|CONTDSL52,CONTDSL7G=CONTDSL52|CONTFH39_50=CONTFH39_50|CONTFH2G_50,CONTFH3G_50,CONTFH48_50=CONTFH48_50|CONTFH49_300=CONTFH49_300|CONTFH7G_50,CONTFH52_50=CONTFH52_50|CONTFH2G_300,CONTFH3G_300,CONTFH58_300=CONTFH58_300|CONTFH7G_300,CONTFH62_300=CONTFH62_300|CONTFH69_1000=CONTFH69_1000|CONTFH2G_1000,CONTFH3G_1000,CONTFH76_1000=CONTFH76_1000|CONTFH7G_1000,CONTFH82_1000=CONTFH82_1000".
DEFINE VARIABLE llOK    AS LOGICAL   NO-UNDO.

MESSAGE "Are you sure to update TMSParam" 
    VIEW-AS ALERT-BOX QUESTION
    BUTTONS YES-NO
    SET llOk.
IF NOT llOK THEN RETURN.

FIND FIRST TMSParam EXCLUSIVE-LOCK
     WHERE TMSParam.brand         = "1" 
       AND TMSParam.ParamGroup    = "YPRO"
       AND TMSParam.Paramcode     = "ProSubsMigrationMappings" NO-ERROR.
IF AVAIL TMSParam THEN 
    ASSIGN TMSParam.charval = RIGHT-TRIM(TMSParam.charval,"|") + "|" + lcValue.
        
MESSAGE "TMSParam updated successfully" 
    VIEW-AS ALERT-BOX. 


          
    