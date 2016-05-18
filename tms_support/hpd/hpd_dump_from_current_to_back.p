DEFINE VARIABLE gcLogDir AS CHARACTER INITIAL "/scratch/log/hpd" NO-UNDO.

/*
   Before running the script make sure that the DumpFile is disabled and
   DumpFile.FullCollModule (Collect All in the UI) has a correct value
   how many days per a file there should (normally a value between 1 to 30).

   REMEMEBER TO VERIFY THAT THE ONGOING DIRECTORY IS CORRECT IN DUMPFILE RECORDS     

   NOTE: The values on the following examples are written when
         the full dump was started on 21.4.2016

   Invoice:
      30 days to one file

      /opt/local/bin/xfear -batch /apps/yoigo/tms_support/hpd/hpd_dump_from_current_to_back.p tms.pf 240,1.3.2015,1.4.2016


   Payment:
      30 days to one file

      /opt/local/bin/xfear -batch /apps/yoigo/tms_support/hpd/hpd_dump_from_current_to_back.p tms.pf 244,1.3.2015,1.4.2016
      
      After the dump is complete check the log file.
      There is a line telling to set a new DumpFile.ModCollModule value.
      Do the change and set "Check EventLog" to value Yes  

   MsRequest:
      30 days to one file            
   
      /opt/local/bin/xfear -batch /apps/yoigo/tms_support/hpd/hpd_dump_from_current_to_back.p tms.pf 221,1.10.2015,1.4.2016

      After the dump is complete check the log file.
      There is a line telling to set a new DumpFile.ModCollModule value.
      Do the change and set "Check EventLog" to value Yes  


   PrePaidRequest:
      30 days to one file            
   
      /opt/local/bin/xfear -batch /apps/yoigo/tms_support/hpd/hpd_dump_from_current_to_back.p tms.pf 228,1.10.2015,1.4.2016

      After the dump is complete check the log file.
      There is a line telling to set a new DumpFile.ModCollModule value.
      Do the change and set "Check EventLog" to value Yes  


   Order:
      365 days to one file   
   
      /opt/local/bin/xfear -batch /apps/yoigo/tms_support/hpd/hpd_dump_from_current_to_back.p tms.pf 223,1.12.2006,1.5.2015


   PrepCDR:
      1 day to one file            
   
      /opt/local/bin/xfear -batch /apps/yoigo/tms_support/hpd/hpd_dump_from_current_to_back.p tms.pf 329,64,0

      After the dump is started check the log file.
      There is a line telling to set a new DumpFile.ModCollModule value.
      Do the change to dumpid 229 and activate the dump (set "Check EventLog" to value Yes if not already set)  
      Verify also that the batchid is suitable.

   PrepEDR:
      7 days to one file
   
      /opt/local/bin/xfear -batch /apps/yoigo/tms_support/hpd/hpd_dump_from_current_to_back.p tms.pf 230,64,6

      After the dump is completed check the log file.
      There is a line telling to set a new DumpFile.ModCollModule value.
      Do the change and activate the dump (set "Check EventLog" to value Yes if not already set).
      Verify also that the batchid is suitable.

   MobCDR:
      1 day to one file            
   
      /opt/local/bin/xfear -batch /apps/yoigo/tms_support/hpd/hpd_dump_from_current_to_back.p tms.pf 319,64,0

      After the dump is started check the log file.
      There is a line telling to set a new DumpFile.ModCollModule value.
      Do the change to dumpid 219 and activate the dump (set "Check EventLog" to value Yes if not already set)  
      Verify also that the batchid is suitable.
*/

{Syst/commpaa.i}
gcBrand = "1".

DEFINE VARIABLE gcSessionParam AS CHARACTER NO-UNDO.

gcSessionParam = SESSION:PARAMETER.

IF NUM-ENTRIES(gcSessionParam) NE 3
THEN RETURN ERROR "Session parameter incorrect".
   
DEFINE VARIABLE gdaFirstOccurance AS DATE      NO-UNDO.
DEFINE VARIABLE gdaStartDate      AS DATE      NO-UNDO.
DEFINE VARIABLE giDumpId          AS INTEGER   NO-UNDO.

ASSIGN
   giDumpId = INTEGER(ENTRY(1,gcSessionParam))
   NO-ERROR.
   
IF ERROR-STATUS:ERROR
THEN RETURN ERROR "Invalid dumpid".


FUNCTION fGiveDate RETURNS DATE
   (icParam AS CHARACTER):

   DEFINE VARIABLE lii           AS INTEGER NO-UNDO.
   DEFINE VARIABLE ldaReturnDate AS DATE    NO-UNDO.
   
   ASSIGN
      lii = INTEGER(icParam)
      NO-ERROR.

   IF ERROR-STATUS:ERROR
   THEN DO:
      ERROR-STATUS:ERROR = NO.
      
      IF NUM-ENTRIES(icParam,".") NE 3
      THEN RETURN ?.
      
      ldaReturnDate = DATE(INTEGER(ENTRY(2,icParam,".")),
                      INTEGER(ENTRY(1,icParam,".")),
                      INTEGER(ENTRY(3,icParam,"."))) NO-ERROR.

      IF ERROR-STATUS:ERROR
      THEN DO:
         ERROR-STATUS:ERROR = NO.
         RETURN ?.
      END.

      RETURN ldaReturnDate.
  END.

  RETURN TODAY - lii.

END FUNCTION.

ASSIGN
   gdaFirstOccurance = fGiveDate(ENTRY(2,gcSessionParam))
   gdaStartDate      = fGiveDate(ENTRY(3,gcSessionParam))
   .
   
IF gdaFirstOccurance = ? OR gdaStartDate = ?
THEN RETURN ERROR "Invalid parameters".

DEFINE VARIABLE giDaysPerFile     AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcTable           AS CHARACTER NO-UNDO.

DEFINE STREAM dumplog.

FUNCTION fMake2Date RETURNS DATE
   (INPUT ideTS     AS DECIMAL):

   DEFINE VARIABLE liYY    AS INTEGER  NO-UNDO.
   DEFINE VARIABLE liMM    AS INTEGER  NO-UNDO.
   DEFINE VARIABLE liDD    AS INTEGER  NO-UNDO.
   DEFINE VARIABLE ldaDate AS DATE     NO-UNDO.

   ASSIGN
      liYY  = TRUNCATE(ideTS,0)
      liMM  = liYY MOD 10000
      liDD  = liMM MOD 100
      liYY  = (liYY - liMM) / 10000
      liMM  = (liMM - liDD) / 100 
      ldaDate = DATE(liMM,liDD,liYY)
      NO-ERROR.

   IF ERROR-STATUS:ERROR
   THEN RETURN ?.
   
   RETURN ldaDate.         
   
END FUNCTION.

FUNCTION fMake2Time RETURNS INTEGER
   (INPUT ideTS     AS DECIMAL):
   
   DEFINE VARIABLE liReturnValue AS INTEGER NO-UNDO.
   
   liReturnValue = INTEGER((ideTS - TRUNCATE(ideTS,0)) * 100000) NO-ERROR.
   
   IF ERROR-STATUS:ERROR
   THEN RETURN ?.
   
   RETURN liReturnValue. 
   
END FUNCTION.

FUNCTION fMake2DT RETURNS DECIMAL 
   (INPUT ldaTSDate AS DATE,
    INPUT liTSTime  AS INTEGER):

   DEFINE VARIABLE liYY   AS INTEGER NO-UNDO.
   DEFINE VARIABLE liMM   AS INTEGER NO-UNDO.
   DEFINE VARIABLE liDD   AS INTEGER NO-UNDO.
   DEFINE VARIABLE ldeRet AS DECIMAL NO-UNDO FORMAT "99999999.99999".

   ASSIGN
      liYY   = YEAR(ldaTSDate)
      liMM   = MONTH(ldaTSDate)
      liDD   = DAY(ldaTSDate)
      ldeRet = liYY * 10000 + liMM * 100 + liDD.
      ldeRet = ldeRet + (liTSTime / 100000)         
      .

   RETURN ldeRet.
   
END FUNCTION.

FUNCTION fTS2DateHMS RETURNS CHARACTER
   (INPUT ldeTS AS DECIMAL):
      
   DEFINE VARIABLE ldaDate AS DATE    NO-UNDO.
   DEFINE VARIABLE liTime  AS INTEGER NO-UNDO.
   
   ASSIGN
      ldaDate = fMake2Date(ldeTS)
      liTime  = fMake2Time(ldeTS)
      .
   
   IF ldaDate = ? OR liTime = ?
   THEN RETURN ?.
   
   ELSE RETURN STRING(DAY(ldaDate),"99")    + "." + 
               STRING(MONTH(ldaDate),"99")  + "." +
               STRING(YEAR(ldaDate),"9999") + " " +
               STRING(liTime, "hh:mm:ss").

END FUNCTION.

FUNCTION fLog RETURNS LOGICAL
   (icText AS CHARACTER):
      
   OUTPUT STREAM dumplog TO VALUE(gcLogDir + "/" + gcTable + ".log") APPEND.
   
   PUT STREAM dumplog UNFORMATTED
      ISO-DATE(NOW) + " " + icText SKIP.
      
   FINALLY:
      OUTPUT STREAM dumplog CLOSE.	
   END FINALLY.

END FUNCTION.


FUNCTION fUpdateDumpFromDate RETURNS LOGICAL
   (iiDumpID  AS INTEGER,
    idaDate   AS DATE,
    ilLogTime AS LOGICAL):
     
   DO TRANSACTION:
   
      FIND DumpFile EXCLUSIVE-LOCK WHERE DumpFile.DumpID = iiDumpID.
      
      IF ilLogTime
      THEN fLog("After the run set DumpFile.ModCollModule to value " + DumpFile.ModCollModule).
      
      DumpFile.ModCollModule = fTS2DateHMS(fMake2DT(idaDate,0)).
      
      RELEASE DumpFile.
      
   END.    
         
   RETURN FALSE.         
         
END FUNCTION.

PROCEDURE pProcess:

   DEFINE VARIABLE ldaDate       AS DATE                    NO-UNDO.
   DEFINE VARIABLE llStop        AS LOGICAL  INITIAL FALSE  NO-UNDO.
   DEFINE VARIABLE liEvents      AS INTEGER                 NO-UNDO.
   DEFINE VARIABLE llInterrupted AS LOGICAL  INITIAL FALSE  NO-UNDO.
   DEFINE VARIABLE llLogTime    AS LOGICAL   INITIAL TRUE   NO-UNDO.
   
   ldaDate = gdaStartDate.

   fLog("The run will process time range " + STRING(gdaFirstOccurance) + " - " + STRING(ldaDate + giDaysPerFile)).

   fUpdateDumpFromDate(giDumpID, ldaDate, NO).
   
   DO WHILE TRUE:
      
      liEvents = 0.
      
      fLog("Started processing date range " + STRING(ldaDate) + " - " + STRING(ldaDate + giDaysPerFile)).
      
      RUN HPD/hpd_filedump.p(giDumpID,"","",0,"","",OUTPUT liEvents, OUTPUT llInterrupted).
      
      IF llInterrupted
      THEN DO:
         fLog("Interrupted when event amount was " +
              STRING(liEvents)).
         LEAVE.      
      END.      

      fLog("Processed " + STRING(liEvents) + " events").
            
      IF llStop
      THEN LEAVE.
            
      ldaDate = ldaDate - giDaysPerFile.
      
      fUpdateDumpFromDate(giDumpId, ldaDate, llLogTime).
      
      llLogTime = FALSE.
      
      IF ldaDate <= gdaFirstOccurance
      THEN llStop = TRUE.
      
   END.  
   
   IF llStop
   THEN fLog("Stopped because stopdate " + STRING(gdaFirstOccurance) + " was reached").   
   
END PROCEDURE.

FIND DumpFile NO-LOCK WHERE DumpFile.DumpID = giDumpID NO-ERROR.

IF AVAILABLE DumpFile AND DumpFile.FileCategory = "HPD"
THEN ASSIGN
        giDaysPerFile = INTEGER(DumpFile.FullCollModule)
        gcTable       = LC(DumpFile.MainTable)
        .

ELSE DO:
   gcTable = "error".
   fLog("Cannot find the dump or it is not HPD dump").
   RETURN.      
END.

IF DumpFile.Active = TRUE
THEN DO:
   fLog("DumpFile is active. Cannot continue.").
   RETURN.   
END.

IF giDaysPerFile < 1 OR giDaysPerFile > 365
THEN DO:
   fLog("DumpFile.FullCollModule has incorrect or not supported value").
   RETURN.         
END.

RUN pProcess.