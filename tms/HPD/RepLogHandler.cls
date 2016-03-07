/*------------------------------------------------------------------------
    File        : RepLogHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:19:10 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING HPD.DumpHandler.

{HPD/HPDEnvironment.i}
{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.RepLogHandler FINAL:
   
   DEFINE PRIVATE VARIABLE objMsgPublisher    AS CLASS Gwy.MqPublisher NO-UNDO.   
   DEFINE PRIVATE VARIABLE lhRepLogTable      AS HANDLE                NO-UNDO.
   DEFINE PRIVATE VARIABLE lhRepLogQuery      AS HANDLE                NO-UNDO.
   
   DEFINE PRIVATE TEMP-TABLE ttDumpHandler NO-UNDO
      FIELD TableName     AS CHARACTER
      FIELD objHandler    AS CLASS Progress.Lang.Object /*HPD.DumpHandler*/
      FIELD VersionNumber AS CHARACTER
      FIELD QuoteKeyValue AS LOGICAL
      INDEX TableName IS PRIMARY UNIQUE TableName
      .   

   DEFINE PRIVATE TEMP-TABLE ttRepLog NO-UNDO
      FIELD RepLogRowID AS ROWID
      INDEX RepLogRowID IS PRIMARY RepLogRowID
      .
            
   DEFINE PRIVATE VARIABLE lcDataBase               AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcMessage                AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcTail                   AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE llFound                  AS LOGICAL     NO-UNDO.
   DEFINE PRIVATE VARIABLE liTransactionSize        AS INTEGER     NO-UNDO.
   DEFINE PRIVATE VARIABLE llUseSeparateQueue       AS LOGICAL     NO-UNDO.
   DEFINE PRIVATE VARIABLE llUseAck                 AS LOGICAL     NO-UNDO.
   DEFINE PRIVATE VARIABLE ldttzPublisherValidUntil AS DATETIME-TZ NO-UNDO.

   CONSTRUCTOR PUBLIC RepLogHandler
      ( icDB AS CHARACTER ):

      DEFINE VARIABLE lcRepLogTableName AS CHARACTER  NO-UNDO.

      ASSIGN
         lcDataBase        = icDB
         lcRepLogTableName = lcDataBase + ".RepLog"
         .

      DO ON ERROR UNDO, THROW:

         llUseSeparateQueue = Syst.Parameters:geti("HPD.Queue" + lcDataBase + ".Separate", "HPD.Interface") = 1.

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then no separate queue is used */
            llUseSeparateQueue = NO.
         END CATCH.

      END.

      DO ON ERROR UNDO, THROW:

         llUseAck = Syst.Parameters:geti("HPD.Queue" + lcDataBase + ".Ack", "HPD.Interface") = 1.

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then assume no ack is expected */
            llUseAck = ?.
         END CATCH.

      END.

      DO ON ERROR UNDO, THROW:

         liTransactionSize = Syst.Parameters:geti("HPD.Queue" + lcDataBase + ".TransactSize", "HPD.Interface").

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            Syst.Parameters:seti("HPD.Queue" + lcDataBase + ".TransactSize", "HPD.Interface", 10000).
            liTransactionSize = Syst.Parameters:geti("HPD.Queue" + lcDataBase + ".TransactSize", "HPD.Interface").
         END CATCH.

      END.

      CREATE BUFFER lhRepLogTable FOR TABLE lcRepLogTableName.

      IF NOT mPopulateDumpHandlers()
      THEN RETURN ERROR NEW Progress.Lang.AppError("No tables defined to online HPD interface for database " + lcDataBase, 1).

      objMsgPublisher = NEW Gwy.MqPublisher("HPD.Queue" +
                                            IF llUseSeparateQueue
                                            THEN lcDataBase
                                            ELSE "").

      CREATE QUERY lhRepLogQuery.
      lhRepLogQuery:SET-BUFFERS(lhRepLogTable).

      IF llUseAck
      THEN lhRepLogQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK WHERE SendTime = ? USE-INDEX SendTime MAX-ROWS &2",lcRepLogTableName, STRING(liTransactionSize))).
      
      &IF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
      ELSE lhRepLogQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK USE-INDEX UTCTime MAX-ROWS &2",lcRepLogTableName, STRING(liTransactionSize))).
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
      ELSE lhRepLogQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK WHERE SendTime = ? USE-INDEX SendTime MAX-ROWS &2",lcRepLogTableName, STRING(liTransactionSize))).
      &ELSE
      RETURN ERROR NEW Progress.Lang.AppError("Unsupported HPD environment", 1).
      &ENDIF

   END CONSTRUCTOR.


   METHOD PRIVATE VOID mPurgeDumpHandlers():

      FOR EACH ttDumpHandler:
         
         IF VALID-OBJECT(ttDumpHandler.objHandler)
         THEN DELETE OBJECT ttDumpHandler.objHandler.
         
      END.
      
      EMPTY TEMP-TABLE ttDumpHandler.

   END METHOD.


   DESTRUCTOR PUBLIC RepLogHandler( ):

      mPurgeDumpHandlers().

      IF VALID-HANDLE(lhRepLogQuery)
      THEN DO:
         lhRepLogQuery:QUERY-CLOSE().
         DELETE OBJECT lhRepLogQuery.
      END.
      
      IF VALID-HANDLE(lhRepLogTable)
      THEN DELETE OBJECT lhRepLogTable.  

      IF VALID-OBJECT(objMsgPublisher)
      THEN DELETE OBJECT objMsgPublisher.
     
   END DESTRUCTOR.


   METHOD PRIVATE CHARACTER mGetVersionNumber
      ( iiDumpID AS INTEGER ):
   
      FIND FIRST DFField NO-LOCK WHERE
         DFField.DumpID   = iiDumpID AND
         DFField.OrderNbr = 2        AND
         DFField.DFField  = "@Version"
      NO-ERROR.
      
      IF AVAILABLE DFField AND DFField.DFLabel > ""
      THEN RETURN DFField.DFLabel.
      
      RETURN "1".
   
   END METHOD.


   METHOD PRIVATE LOGICAL mQuoteKeyValue
      ( iiDumpID AS INTEGER ):

      FIND FIRST DFField NO-LOCK WHERE
         DFField.DumpID   = iiDumpID AND
         DFField.OrderNbr = 1
      NO-ERROR.

      IF AVAILABLE DFField AND DFField.DFField BEGINS "¤"
      THEN RETURN TRUE.

      RETURN FALSE.

   END METHOD.


   METHOD PRIVATE LOGICAL mPopulateDumpHandlers( ):
      
      DEFINE VARIABLE llFoundHandler AS LOGICAL INITIAL FALSE NO-UNDO.

      FOR EACH DumpFile NO-LOCK WHERE
         DumpFile.FileCategory = "HPD" AND
         DumpFile.SideTables = lcDataBase
         ON ERROR UNDO, THROW:
            
         CREATE ttDumpHandler.
         ttDumpHandler.objHandler = DYNAMIC-NEW DumpFile.LinkKey(DumpFile.DumpID).
         
         ASSIGN
            ttDumpHandler.TableName     = DumpFile.MainTable
            ttDumpHandler.VersionNumber = mGetVersionNumber(DumpFile.DumpID)
            ttDumpHandler.QuoteKeyValue = mQuoteKeyValue(DumpFile.DumpID)
            llFoundHandler = TRUE
            .
      END.

      RETURN llFoundHandler.

   END METHOD.     
   
      
   METHOD PRIVATE VOID mLog
      ( iiLogLevel   AS INTEGER,
        icLogType    AS CHARACTER, /* ERROR, INFO, DEBUG, ... */
        icLogMessage AS CHARACTER ):
      IF LOG-MANAGER:LOGGING-LEVEL GE iiLogLevel
      THEN LOG-MANAGER:WRITE-MESSAGE(icLogMessage, icLogType).
   END METHOD.


   /* Returns TRUE if send failed and otherwise FALSE */
   METHOD PRIVATE LOGICAL mSendRepLog2MQ():

      FIND ttDumpHandler WHERE ttDumpHandler.TableName = lhRepLogTable::TableName NO-ERROR.

      IF NOT AVAILABLE ttDumpHandler
      THEN RETURN ERROR NEW Progress.Lang.AppError(lcDataBase + ".RepLog cannot handle table " +
                                                   lhRepLogTable::TableName, 1).

      IF NOT VALID-OBJECT(ttDumpHandler.objHandler)
      THEN RETURN ERROR NEW Progress.Lang.AppError("Handler object for table " + lhRepLogTable::TableName +
                                                   " has been lost", 1).

      lcMessage = ( IF llUseAck = ?
                    THEN ""
                    ELSE IF llUseAck = YES
                    THEN lhRepLogTable:DBNAME + {&HPDDumpDelimiter} +
                         STRING(lhRepLogTable:ROWID) + {&HPDDumpDelimiter}
                    ELSE {&HPDDumpDelimiter} + {&HPDDumpDelimiter} ) +
                  lhRepLogTable::TableName + {&HPDDumpDelimiter} +
                  lhRepLogTable::EventType + {&HPDDumpDelimiter} +

                  &IF "{&HPD_ENVIRONMENT}" = "Salt"
                  &THEN
                     ISO-DATE(lhRepLogTable::UTCTime).
                  &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
                  &THEN
                     ISO-DATE(lhRepLogTable::EventTime).
                  &ENDIF

      IF lhRepLogTable::EventType = "DELETE"
      THEN lcMessage = lcMessage + {&HPDDumpDelimiter} +
                     ( IF ttDumpHandler.QuoteKeyValue
                       THEN QUOTER(lhRepLogTable::KeyValue)
                       ELSE lhRepLogTable::KeyValue ) +
                     {&HPDDumpDelimiter} + ttDumpHandler.VersionNumber.
      ELSE DO:
         lcTail = CAST(ttDumpHandler.objHandler, DumpHandler):mProcessRepLog(lhRepLogTable::RowID).

         IF lcTail = ""
         THEN DO:
            mLog(2, "INFO",
                 SUBSTITUTE("While processing message: " + lcMessage + 
                            "couldn't find a record where ROWID='&1'",
                            IF lhRepLogTable::RowID = ?
                            THEN "?" ELSE lhRepLogTable::RowID)).
            RETURN FALSE.
         END.

         lcMessage = lcMessage + {&HPDDumpDelimiter} + lcTail.
      END.

      IF NOT objMsgPublisher:send_message(lcMessage)
      THEN DO:
         mLog(2, "INFO", "Failed to send message: " + lcMessage).
         RETURN TRUE.
      END.

      mLog(3, "DEBUG", "Send message: " + lcMessage).

      RETURN FALSE.

   END METHOD.


   METHOD PRIVATE INTEGER mSendBatch():         
      
      DEFINE VARIABLE lii  AS INTEGER NO-UNDO.
      DEFINE VARIABLE llOK AS LOGICAL NO-UNDO.
      
      lhRepLogQuery:QUERY-OPEN().

      DO WHILE TRUE:

         llOK = lhRepLogQuery:GET-NEXT(NO-LOCK).

         IF llOK = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process query", 1).

         /* No more records, or query is not open */
         IF NOT llOK
         THEN LEAVE.

         lii = lii + 1.

         IF mSendRepLog2MQ()
         THEN DO:
            CREATE ttRepLog.
            ttReplog.RepLogRowID = lhRepLogTable:ROWID.   
         END.
         
         ELSE IF NOT objMsgPublisher:mReConnect()
         THEN RETURN ERROR NEW Progress.Lang.AppError("MQ publisher cannot connect to server!", 1).

      END.

      RETURN lii.
      
      FINALLY:      
         lhRepLogQuery:QUERY-CLOSE().
      END FINALLY. 
            
   END METHOD.
      

   METHOD PRIVATE VOID mCleanReplog
      ( iiAmount AS INTEGER ):         

      DEFINE VARIABLE llOK AS LOGICAL NO-UNDO.      

      lhRepLogQuery:QUERY-OPEN().

      DO WHILE iiAmount > 0 TRANSACTION:
         
         ASSIGN
            iiAmount = iiAmount - 1
            llOK     = lhRepLogQuery:GET-NEXT(NO-LOCK)
            .

         IF llOK = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process query", 1).
         
         /* No more records, or query is not open */
         IF NOT llOK
         THEN LEAVE.

         /* If ttRepLog is available then there has been a problem
            while sending the replog data and it is not
            allowed to delete the RepLog record. */         
         IF CAN-FIND(FIRST ttRepLog WHERE ttRepLog.RepLogRowID = lhRepLogTable:ROWID USE-INDEX RepLogRowID)
         THEN NEXT.
         
         lhRepLogQuery:GET-CURRENT(EXCLUSIVE-LOCK, NO-WAIT).
         
         /* If for some reason RepLog table is locked then skip it
            (there will be a duplicate send next time) */         
         IF lhRepLogTable:LOCKED
         THEN mLog(2, "ERROR", SUBSTITUTE("Replog ROWID='&1' locked", STRING(lhRepLogTable:ROWID))).
         ELSE IF llUseAck
         THEN ASSIGN
                 lhRepLogTable::SendTime  = NOW
                 lhRepLogTable::SendCount = lhRepLogTable::SendCount + 1
                 .
         ELSE lhRepLogTable:BUFFER-DELETE().
         
      END.

      FINALLY:
         lhRepLogQuery:QUERY-CLOSE().
      END FINALLY.
            
   END METHOD.


   METHOD PRIVATE VOID mCleanReplogAllOK
      ( iiAmount AS INTEGER ):

      DEFINE VARIABLE llOK AS LOGICAL NO-UNDO.

      lhRepLogQuery:QUERY-OPEN().
      
      DO WHILE iiAmount > 0 TRANSACTION:
         
         ASSIGN
            iiAmount = iiAmount - 1
            llOK     = lhRepLogQuery:GET-NEXT(EXCLUSIVE-LOCK, NO-WAIT)
            .

         IF llOK = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process query", 1).
         
         /* No more records, or query is not open */
         IF NOT llOK
         THEN LEAVE.

         /* If for some reason RepLog table is locked then skip it
            (there will be a duplicate send next time) */         
         IF lhRepLogTable:LOCKED
         THEN mLog(2, "ERROR", SUBSTITUTE("Replog ROWID='&1' locked", STRING(lhRepLogTable:ROWID))).
         ELSE IF llUseAck
         THEN ASSIGN
                 lhRepLogTable::SendTime  = NOW
                 lhRepLogTable::SendCount = lhRepLogTable::SendCount + 1
                 .
         ELSE lhRepLogTable:BUFFER-DELETE().
         
      END.

      FINALLY:
         lhRepLogQuery:QUERY-CLOSE().
      END FINALLY.
            
   END METHOD.


   METHOD PUBLIC VOID mReload():

      mPurgeDumpHandlers().

      IF NOT mPopulateDumpHandlers()
      THEN RETURN ERROR NEW Progress.Lang.AppError("No tables defined to online HPD interface for database " + lcDataBase, 1).

   END METHOD.


   METHOD PUBLIC LOGICAL mIsEmpty():         
      
      IF llUseAck
      THEN llFound = lhRepLogTable:FIND-FIRST("WHERE SendTime = ?", NO-LOCK) NO-ERROR.
      ELSE llFound = lhRepLogTable:FIND-FIRST("", NO-LOCK) NO-ERROR.
      
      RETURN NOT llFound.
            
   END METHOD.


   METHOD PUBLIC VOID mProcess():         

      DEFINE VARIABLE lii  AS INTEGER NO-UNDO.

      lii = mSendBatch().
      
      IF lii = 0
      THEN RETURN.
      
      IF CAN-FIND(FIRST ttRepLog)
      THEN mCleanReplog(lii).
      ELSE mCleanReplogAllOK(lii).
      
      FINALLY:
         EMPTY TEMP-TABLE ttRepLog.      
      END FINALLY.
      
   END METHOD.   
   
END CLASS.