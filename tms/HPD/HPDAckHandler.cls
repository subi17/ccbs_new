USING Progress.Lang.*.

{HPD/HPDEnvironment.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDAckHandler:
   
   DEFINE PRIVATE VARIABLE lhRepLogTable    AS HANDLE    NO-UNDO.
   DEFINE PRIVATE VARIABLE llFound          AS LOGICAL   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcDB             AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lcTableName      AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lcExportDir      AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lhRepLogQuery    AS HANDLE    NO-UNDO.
   DEFINE PRIVATE VARIABLE liRetryMinutes   AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE liMaxSendCount   AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE llConnectionDone AS LOGICAL INITIAL NO NO-UNDO.

   DEFINE PRIVATE STREAM strexp.

   CONSTRUCTOR PUBLIC HPDAckHandler (icDB AS CHARACTER):

      ASSIGN
         liMaxSendCount = Syst.Parameters:geti("HPD.Ack.MaxSendCount", "HPD.Interface")
         liRetryMinutes = Syst.Parameters:geti("HPD.Ack.RetryMinutes", "HPD.Interface")
         lcExportDir    = Syst.Parameters:getc("HPD.Ack.ExportDir", "HPD.Interface")
         lcDB           = icDB
         lcTableName    = lcDB + ".RepLog"
         . 
      
      IF NOT CONNECTED(lcDB)
      THEN mMakeConnection().

      CREATE BUFFER lhRepLogTable FOR TABLE lcTableName.
      
      CREATE QUERY lhRepLogQuery.
      lhRepLogQuery:SET-BUFFERS(lhRepLogTable).
      lhRepLogQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK USE-INDEX SendTime",
                                             lcTableName)).
        
   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC HPDAckHandler():

      IF VALID-HANDLE(lhRepLogQuery)
      THEN DO:
         lhRepLogQuery:QUERY-CLOSE().
         DELETE OBJECT lhRepLogQuery.
      END.      
        
      IF VALID-HANDLE(lhRepLogTable)
      THEN DELETE OBJECT lhRepLogTable.

      IF llConnectionDone AND CONNECTED(lcDB)
      THEN DO:
         DISCONNECT VALUE(lcDB).
         IF LOG-MANAGER:LOGFILE-NAME <> ?
         THEN LOG-MANAGER:WRITE-MESSAGE("Disconnected database " + lcDB, "INFO").
      END.
      
   END DESTRUCTOR.   

   
   METHOD PRIVATE VOID mMakeConnection():
      
      DEFINE VARIABLE llFound             AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lcLogicalFieldName  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcLogicalFieldValue AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcPathFieldName     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcPathFieldValue    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcTableName         AS CHARACTER NO-UNDO.      
      DEFINE VARIABLE lhDBConfig          AS HANDLE    NO-UNDO.
      
      &IF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
      ASSIGN
         lcTableName        = "DBConf"
         lcLogicalFieldName = "Name"
         lcPathFieldName    = "Path"
         .      
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
      ASSIGN
         lcTableName        = "DBConfig"
         lcLogicalFieldName = "DBConnName"
         lcPathFieldName    = "DirectConnect"
         .
      &ELSE      
      RETURN ERROR NEW Progress.Lang.AppError("Unknown HPD environment", 1).
      &ENDIF

      CREATE BUFFER lhDBConfig FOR TABLE lcTableName.
      
      llFound = lhDBConfig:FIND-FIRST("WHERE " + lcLogicalFieldName + " = " + QUOTER(lcDB), NO-LOCK) NO-ERROR.
      
      IF NOT llFound
      THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot find dbconfig for database " + lcDB, 1).       

      ASSIGN
         lcLogicalFieldValue = lhDBConfig:BUFFER-FIELD(lcLogicalFieldName):BUFFER-VALUE()
         lcPathFieldValue    = lhDBConfig:BUFFER-FIELD(lcPathFieldName):BUFFER-VALUE()
         .

      &IF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
      CONNECT VALUE(lcPathFieldValue + " -ld " + lcLogicalFieldValue) NO-ERROR.
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
      CONNECT VALUE(lcPathFieldValue + "/" + lcLogicalFieldValue) NO-ERROR.
      &ELSE
      RETURN ERROR NEW Progress.Lang.AppError("Unknown HPD environment", 1).
      &ENDIF

      IF ERROR-STATUS:ERROR = TRUE
      THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot connect to database " + lcDB, 1).                 

      llConnectionDone = YES.

      IF LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Connected database " + lcDB, "INFO").    
   
      FINALLY:
         
         IF VALID-HANDLE(lhDBConfig)
         THEN DELETE OBJECT lhDBConfig. 
      
      END FINALLY.
   
   END METHOD.


   METHOD PRIVATE VOID mExportReplog
      ( icType      AS CHARACTER,
        icError     AS CHARACTER ):
      
      DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.
      
      lcLine = HPD.HPDCommon:mDynExport(lhReplogTable, " ").
      
      OUTPUT STREAM strexp TO
         VALUE(lcExportDir + "/" + icType + "_" + lcDB + "_" +
               STRING(DAY(TODAY),"99") + STRING(MONTH(TODAY),"99") +
               STRING(YEAR(TODAY),"9999") + ".log") APPEND.

      IF icError > "" AND SEEK(strexp) = 0
      THEN PUT STREAM strexp UNFORMATTED icError SKIP.

      PUT STREAM strexp UNFORMATTED lcLine SKIP.       
   
      FINALLY:
         OUTPUT STREAM strexp CLOSE.	
      END FINALLY.
   
   END METHOD.
      
   
   METHOD PUBLIC LOGICAL mDelete
      ( icROWID AS CHARACTER,
        idttzEventTime AS DATETIME-TZ,
        iiErrorCode AS INTEGER,
        icError AS CHARACTER):

      llFound = lhRepLogTable:FIND-BY-ROWID(TO-ROWID(icROWID), EXCLUSIVE-LOCK, NO-WAIT) NO-ERROR.
      
      IF NOT llFound
      THEN DO:
         IF lhRepLogTable:LOCKED
         THEN RETURN FALSE.

         LOG-MANAGER:WRITE-MESSAGE("Cannot find replog record. ROWID=" + icROWID +
                                   " EventTime=" + STRING(idttzEventTime), "INFO").
         RETURN TRUE.
      END.

      IF lhRepLogTable::EventTime NE idttzEventTime
      THEN DO:
         LOG-MANAGER:WRITE-MESSAGE("Eventtime mismatch. ROWID=" + icROWID +
                                   " Replog eventtime=" +
                                   STRING(lhRepLogTable::EventTime) +
                                   " ACK message eventtime=" +
                                   STRING(idttzEventTime), "INFO").
         mExportReplog("eventtime_mismatch", "").
         
         RETURN TRUE.     
      END.

      IF icError > ""
      THEN DO:
         LOG-MANAGER:WRITE-MESSAGE("HPD Ack message returns an error. ROWID=" + icROWID +
                                   " Replog eventtime=" +
                                   STRING(lhRepLogTable::EventTime) +
                                   " ACK message eventtime=" +
                                   STRING(idttzEventTime) + " error=" + icError, "INFO").
         mExportReplog("error_" + STRING(iiErrorCode), icError).
      END.

      lhRepLogTable:BUFFER-DELETE().
      
      RETURN TRUE.      
      
   END METHOD.
   
   
   METHOD PUBLIC VOID mProcessWaitingReplog():

      DEFINE VARIABLE llOK     AS LOGICAL     NO-UNDO.
      DEFINE VARIABLE ldtzLast AS DATETIME-TZ NO-UNDO.

      ldtzLast = ADD-INTERVAL(NOW, liRetryMinutes * (-1), "minutes").

      DO TRANSACTION ON ERROR UNDO, THROW:

         lhRepLogQuery:QUERY-OPEN().

         DO WHILE TRUE ON ERROR UNDO, THROW:

            llOK = lhRepLogQuery:GET-NEXT(EXCLUSIVE-LOCK, NO-WAIT).

            IF llOK = ?
            THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process query", 1).
         
            /* No more records, or query is not open */
            IF NOT llOK
            THEN LEAVE.

            /* If for some reason RepLog table is locked then skip it */
            IF lhRepLogTable:LOCKED
            THEN NEXT.
         
            IF lhRepLogTable::SendTime = ? OR lhRepLogTable::SendTime > ldtzLast
            THEN LEAVE.
         
            IF lhRepLogTable::SendCount >= liMaxSendCount
            THEN DO:
               LOG-MANAGER:WRITE-MESSAGE("Max retries reached. ROWID=" + STRING(lhRepLogTable:ROWID), "INFO").
               mExportReplog("max_retries_reached", "").
               lhRepLogTable:BUFFER-DELETE().
               NEXT.
            END.
         
            lhRepLogTable::SendTime  = ?.
            lhRepLogTable:BUFFER-RELEASE().

         END.

         FINALLY:
            lhRepLogQuery:QUERY-CLOSE().
         END FINALLY.

      END.

   END METHOD.

END CLASS.