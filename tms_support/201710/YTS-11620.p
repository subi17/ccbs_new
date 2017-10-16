/* YTS-11620.p */
DEFINE STREAM strlog.
DEFINE VARIABLE cNewVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLstVal AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogPath 
   AS CHARACTER FORMAT "X(75)" 
   LABEL "Log path" 
   INITIAL "/apps/yoigo/tms_support/201710/YTS-11620-backup.log" NO-UNDO.
DEFINE VARIABLE cLogData AS CHARACTER NO-UNDO.

/* Getting log path */
UPDATE cLogPath.

/* Opening log file */
OUTPUT STREAM strlog TO VALUE(cLogPath) APPEND.

PUT STREAM strlog 
    UNFORMATTED "TMSParam.ParamCode                                 >>>  TMSParam.CharVal" SKIP 
                "---------------------------------------------------------------------------------------------------------" SKIP.
           
/* Processing records */
FOR EACH TMSParam WHERE
         TMSParam.Brand      = "1" /*Syst.Parameters:gcBrand*/ AND
         TMSParam.ParamGroup = "DMS"
    NO-LOCK /* EXCLUSIVE-LOCK */ :

    cNewVal = "".
   
    IF NOT(TMSParam.CharVal MATCHES "*,12*" OR 
           TMSParam.CharVal MATCHES "*,14*") THEN
    NEXT.
 
    DO iLstVal = 1 TO NUM-ENTRIES(TMSParam.CharVal):
        IF ENTRY(iLstVal, TMSParam.CharVal) <> "12" AND 
           ENTRY(iLstVal, TMSParam.CharVal) <> "14" THEN 
           cNewVal = cNewVal + "," + entry(iLstVal, TMSParam.CharVal).      
    END.
    cNewVal = TRIM(cNewVal,",").
    
    MESSAGE "TMSParam.ParamCode:" TMSParam.ParamCode SKIP(1)
            "Original TMSParam.CharVal:" TMSParam.CharVal SKIP 
            "New value to be assigned:" cNewVal VIEW-AS ALERT-BOX QUESTION 
            BUTTONS YES-NO UPDATE lchg AS LOGICAL.
    IF lchg THEN 
    DO:
       cLogData = STRING(TMSParam.ParamCode,"X(50)") + " >>> " + STRING(TMSParam.CharVal,"X(70)"). 
       PUT STREAM strlog UNFORMATTED cLogData SKIP.
       /* 
       TMSParam.CharVal = cNewVal.*/
    END.
    
END.

/* Closing log path */
OUTPUT STREAM strLog CLOSE.

