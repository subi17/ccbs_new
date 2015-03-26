/*------------------------------------------------------------------------
  MODULE .......: shaperconfcreation.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Mon Feb 09 20:39:04 EET 2015
  Version ......: Yoigo
  ----------------------------------------------------------------------*/
  
/* ***************************  Definitions  ************************** */
{commpaa.i}
katun = "Cron".
gcBrand = "1".
{cparam2.i}
{eventlog.i}
{ftransdir.i}
{tariffconfig.i}
{tariffcons.i}

DEFINE INPUT PARAMETER icIncDir   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icSpoolDir AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE liFirstLine AS INTEGER   NO-UNDO INITIAL 1.

DEFINE TEMP-TABLE ttShaperConf NO-UNDO 
   FIELD Template   AS CHARACTER 
   FIELD TariffType AS CHARACTER
   FIELD Tariff     AS CHARACTER 
   FIELD LUnshaped  AS CHARACTER 
   FIELD LShaped    AS CHARACTER
   FIELD SConfId    AS CHARACTER.
    
DEFINE STREAM SCFile.
DEFINE STREAM SCIn.
DEFINE STREAM SCLog.

/* ********************  Functions  ******************** */

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   icMessage = "ERROR:" + icMessage.
   
   PUT STREAM SCLog UNFORMATTED
      TODAY                   " | " 
      STRING(TIME,"HH:MM:SS") " | "
      icMessage SKIP.
      
END FUNCTION.

/* ***************************  Main Block  *************************** */

ASSIGN 
   lcInputFile = icIncDir + "shaperconf.txt"    
   lcLogFile   = icSpoolDir + "shaperconf.log". 
   
INPUT STREAM SCIn FROM VALUE(lcInputFile).
OUTPUT STREAM SCLog TO VALUE(lcLogFile) APPEND.
                          
REPEAT:
   IMPORT STREAM SCIn UNFORMATTED lcLine.
   
   /* Ignore the first line - (Header) */
   IF liFirstLine = 1 THEN DO:
      liFirstLine = liFirstLine + 1.
      NEXT.
   END.
   
   CREATE ttShaperConf.
   ASSIGN 
      ttShaperConf.SConfId    = TRIM(ENTRY(1,lcLine,";")) 
      ttShaperConf.Template   = TRIM(ENTRY(2,lcLine,";"))
      ttShaperConf.TariffType = TRIM(ENTRY(3,lcLine,";"))
      ttShaperConf.Tariff     = TRIM(ENTRY(4,lcLine,";"))
      ttShaperConf.LUnshaped  = TRIM(ENTRY(5,lcLine,";"))
      ttShaperConf.LShaped    = TRIM(ENTRY(6,lcLine,";")) 
      NO-ERROR.
      
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Incorrect input data").
      RETURN "ERROR". 
   END.                                     
END.
                          
RUN pValidateFileData.

IF RETURN-VALUE <> "OK" THEN 
   RETURN RETURN-VALUE.

RUN pCreateShaperConf.

IF RETURN-VALUE <> "OK" THEN 
   RETURN RETURN-VALUE.

OUTPUT STREAM SCLog CLOSE.
INPUT STREAM SCIn CLOSE.

RETURN "OK".

/* ***************************  Main End  *************************** */

PROCEDURE pValidateFileData:
    
    SHAPER-CONF:
    FOR EACH ttShaperConf NO-LOCK:
       IF ttShaperConf.Template EQ "" THEN DO:
          fError("No Template data available").
          RETURN "ERROR".
       END.   
       IF ttShaperConf.TariffType EQ "" THEN DO:
          fError("No Tariff type data available").
          RETURN "ERROR".
       END.
       IF ttShaperConf.Tariff EQ "" THEN DO:
          fError("No Tariff data available").
          RETURN "ERROR".
       END.
       IF ttShaperConf.LUnshaped EQ "" THEN DO:
          fError("No Limit unshaped data available").
          RETURN "ERROR".
       END.
       IF ttShaperConf.LShaped EQ "" THEN DO:
          fError("No Limit data available").
          RETURN "ERROR".
       END.
    END.
            
    RETURN "OK".
    
END PROCEDURE.

PROCEDURE pCreateShaperConf:
DEFINE VARIABLE lcTemplate      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTariffType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTariff        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLimitUNShaped AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLimitShaped   AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcShaperConfID AS CHARACTER NO-UNDO.

   FOR EACH ttShaperConf NO-LOCK:
      FIND FIRST ShaperConf WHERE 
                 ShaperConf.ShaperConfID = ttShaperConf.SConfId 
      NO-LOCK NO-ERROR.
                  
      IF AVAILABLE ShaperConf THEN DO:
         ASSIGN 
            ShaperConf.Template      = ttShaperConf.Template
            ShaperConf.TariffType    = ttShaperConf.TariffType
            ShaperConf.Tariff        = ttShaperConf.Tariff
            ShaperConf.LimitUnshaped = DECIMAL(ttShaperConf.LUnshaped)
            ShaperConf.LimitShaped   = DECIMAL(ttShaperConf.LShaped) NO-ERROR.
      END.     
      ELSE DO:                 
         CREATE ShaperConf.
         ASSIGN 
            ShaperConf.Brand         = gcBrand
            ShaperConf.ShaperConfID  = ttShaperConf.SConfId          
            ShaperConf.Template      = ttShaperConf.Template
            ShaperConf.TariffType    = ttShaperConf.TariffType
            ShaperConf.Tariff        = ttShaperConf.Tariff
            ShaperConf.LimitUnshaped = DECIMAL(ttShaperConf.LUnshaped)
            ShaperConf.LimitShaped   = DECIMAL(ttShaperConf.LShaped) 
            ShaperConf.Active        = YES NO-ERROR.
      END.
             
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Error in creating ShaperConf").
         RETURN "ERROR".     
      END.        
   END.
   
   RETURN "OK".
      
END PROCEDURE. 
