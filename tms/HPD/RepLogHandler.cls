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
   DEFINE PRIVATE VARIABLE llMultiTenancy     AS LOGICAL INITIAL FALSE NO-UNDO.

   DEFINE PRIVATE TEMP-TABLE ttDumpHandler NO-UNDO
      FIELD TableName     AS CHARACTER
      FIELD TenantName    AS CHARACTER
      FIELD objHandler    AS CLASS Progress.Lang.Object /*HPD.DumpHandler*/
      FIELD VersionNumber AS CHARACTER
      FIELD QuoteKeyValue AS LOGICAL
      INDEX TableName IS PRIMARY UNIQUE TableName TenantName
      .   

   DEFINE PRIVATE VARIABLE lcQueueName        AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcRepLogTable      AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcMessage          AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcTail             AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE llFound            AS LOGICAL     NO-UNDO.
   DEFINE PRIVATE VARIABLE liTransactionSize  AS INTEGER     NO-UNDO.
   DEFINE PRIVATE VARIABLE llUseSeparateQueue AS LOGICAL     NO-UNDO.
   DEFINE PRIVATE VARIABLE llUseAck           AS LOGICAL     NO-UNDO.


   CONSTRUCTOR PUBLIC RepLogHandler
      (icDB        AS CHARACTER,
       icQueueName AS CHARACTER):

      llMultiTenancy = IS-DB-MULTI-TENANT(icDB).
      mInitialize(icDB + ".RepLog", icQueueName).

   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC RepLogHandler
      (icDB AS CHARACTER):

      llMultiTenancy = IS-DB-MULTI-TENANT(icDB).
      mInitialize(icDB + ".RepLog", icDB).

   END CONSTRUCTOR.


   METHOD PRIVATE VOID mInitialize
      (icRepLogTable AS CHARACTER,
       icQueueName   AS CHARACTER):

      ASSIGN
         lcRepLogTable = icRepLogTable
         lcQueueName   = icQueueName
         .

      DO ON ERROR UNDO, THROW:

         llUseSeparateQueue = Syst.Parameters:geti("HPD.Queue" + lcQueueName + ".Separate", "HPD.Interface") = 1.

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then no separate queue is used */
            llUseSeparateQueue = NO.
         END CATCH.

      END.

      DO ON ERROR UNDO, THROW:

         llUseAck = Syst.Parameters:geti("HPD.Queue" + lcQueueName + ".Ack", "HPD.Interface") = 1.

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then assume no ack is expected */
            llUseAck = ?.
         END CATCH.

      END.

      DO ON ERROR UNDO, THROW:

         liTransactionSize = Syst.Parameters:geti("HPD.Queue" + lcQueueName + ".TransactSize", "HPD.Interface").

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            Syst.Parameters:seti("HPD.Queue" + lcQueueName + ".TransactSize", "HPD.Interface", 10000).
            liTransactionSize = Syst.Parameters:geti("HPD.Queue" + lcQueueName + ".TransactSize", "HPD.Interface").
         END CATCH.

      END.

      CREATE BUFFER lhRepLogTable FOR TABLE lcRepLogTable.

      IF NOT mPopulateDumpHandlers()
      THEN RETURN ERROR NEW Progress.Lang.AppError("No tables defined to online HPD interface for database " + lcQueueName, 1).

      objMsgPublisher = NEW Gwy.MqPublisher("HPD.Queue" +
                                            IF llUseSeparateQueue
                                            THEN lcQueueName
                                            ELSE "").

      CREATE QUERY lhRepLogQuery.
      lhRepLogQuery:SET-BUFFERS(lhRepLogTable).

      IF llUseAck
      THEN lhRepLogQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK WHERE SendTime = ? USE-INDEX SendTime MAX-ROWS &2",lcRepLogTable, STRING(liTransactionSize))).
      
      &IF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
      ELSE lhRepLogQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK USE-INDEX UTCTime MAX-ROWS &2",lcRepLogTable, STRING(liTransactionSize))).
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
      ELSE lhRepLogQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK WHERE SendTime = ? USE-INDEX SendTime MAX-ROWS &2",lcRepLogTable, STRING(liTransactionSize))).
      &ELSE
      RETURN ERROR NEW Progress.Lang.AppError("Unsupported HPD environment", 1).
      &ENDIF

   END METHOD.


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

      IF AVAILABLE DFField AND DFField.DFField BEGINS "~""
      THEN RETURN TRUE.

      RETURN FALSE.

   END METHOD.


   METHOD PRIVATE LOGICAL mPopulateDumpHandlers
      ( icTenant AS CHARACTER ):
      
      DEFINE VARIABLE llFoundHandler AS LOGICAL INITIAL FALSE NO-UNDO.

      FOR EACH DumpFile NO-LOCK WHERE
         DumpFile.FileCategory = "HPD" AND
         DumpFile.SideTables = lcQueueName
         ON ERROR UNDO, THROW:
            
         CREATE ttDumpHandler.
         ttDumpHandler.objHandler = DYNAMIC-NEW DumpFile.LinkKey(DumpFile.DumpID).
         
         ASSIGN
            ttDumpHandler.TableName     = DumpFile.MainTable
            ttDumpHandler.TenantName    = icTenant
            ttDumpHandler.VersionNumber = mGetVersionNumber(DumpFile.DumpID)
            ttDumpHandler.QuoteKeyValue = mQuoteKeyValue(DumpFile.DumpID)
            llFoundHandler = TRUE
            .
      END.

      RETURN llFoundHandler.

   END METHOD.     


   METHOD PRIVATE LOGICAL mPopulateDumpHandlers( ):

      IF NOT llMultiTenancy
      THEN RETURN mPopulateDumpHandlers("").
      
      DEFINE VARIABLE llOK AS LOGICAL INITIAL FALSE NO-UNDO.
      DEFINE VARIABLE lii AS INTEGER NO-UNDO.
      DEFINE VARIABLE liNumConnectedDBs AS INTEGER NO-UNDO.

      DO ON ERROR UNDO, THROW:

         DO lii = 0 TO 100:

            DO liNumConnectedDBs = 1 TO NUM-DBS:
               SET-EFFECTIVE-TENANT(lii, LDBNAME(liNumConnectedDBs)).
            END.

            llOK = mPopulateDumpHandlers(GET-EFFECTIVE-TENANT-NAME(LDBNAME(liNumConnectedDBs - 1))).

         END.

         CATCH anyError AS Progress.Lang.Error:
            LEAVE.
         END CATCH.

      END.

      RETURN llOK.

   END METHOD.


   METHOD PRIVATE VOID mLog
      ( iiLogLevel   AS INTEGER,
        icLogType    AS CHARACTER, /* ERROR, INFO, DEBUG, ... */
        icLogMessage AS CHARACTER ):
      IF LOG-MANAGER:LOGGING-LEVEL GE iiLogLevel
      THEN LOG-MANAGER:WRITE-MESSAGE(icLogMessage, icLogType).
   END METHOD.


   /*
      Returns
      1: If send was successful
      2: If send failed
      3: If replog record refers to non existing record
   */
   METHOD PRIVATE INTEGER mSendRepLog2MQ():

      IF llMultiTenancy
      THEN FIND ttDumpHandler WHERE
              ttDumpHandler.TableName  = lhRepLogTable::TableName AND
              ttDumpHandler.TenantName = lhRepLogTable::TenantName
           NO-ERROR.
      ELSE FIND ttDumpHandler WHERE
              ttDumpHandler.TableName  = lhRepLogTable::TableName
           NO-ERROR.

      IF NOT AVAILABLE ttDumpHandler
      THEN RETURN ERROR NEW Progress.Lang.AppError("Queue " + lcQueueName + " cannot handle table " +
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

      IF llMultiTenancy
      THEN lcMessage = lcMessage + {&HPDDumpDelimiter} +
      &IF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN IF ttDumpHandler.TenantName = "Default"
            THEN "Yoigo"
            ELSE ttDumpHandler.TenantName.
      &ELSE ttDumpHandler.TenantName.
      &ENDIF

      IF lhRepLogTable::EventType = "DELETE"
      THEN lcMessage = lcMessage + {&HPDDumpDelimiter} +
                     ( IF ttDumpHandler.QuoteKeyValue
                       THEN QUOTER(lhRepLogTable::KeyValue)
                       ELSE lhRepLogTable::KeyValue ) +
                     {&HPDDumpDelimiter} + ttDumpHandler.VersionNumber.
      ELSE DO:
         lcTail = CAST(ttDumpHandler.objHandler, DumpHandler):mProcessRepLog(lhRepLogTable::RowID).

         IF lcTail > ""
         THEN lcMessage = lcMessage + {&HPDDumpDelimiter} + lcTail.

         ELSE DO:
            IF lcTail = ""
            THEN mLog(2, "INFO",
                      SUBSTITUTE("While processing message: " + lcMessage +
                                 " couldn't find a record where ROWID='&1'",
                                 IF lhRepLogTable::RowID = ?
                                 THEN "?" ELSE lhRepLogTable::RowID)).
            RETURN 3.
         END.
      END.

      IF NOT objMsgPublisher:send_message(lcMessage)
      THEN DO:
         mLog(2, "INFO", "Failed to send message: " + lcMessage).
         RETURN 2.
      END.

      mLog(3, "DEBUG", "Send OK").

      RETURN 1.

   END METHOD.


   METHOD PUBLIC VOID mProcess():

      DEFINE VARIABLE liCount     AS INTEGER NO-UNDO.
      DEFINE VARIABLE llOK        AS LOGICAL NO-UNDO.
      DEFINE VARIABLE llReconnect AS LOGICAL NO-UNDO INITIAL FALSE.

      DO TRANSACTION ON ERROR UNDO, THROW:

         lhRepLogQuery:QUERY-OPEN().

         DO WHILE TRUE ON ERROR UNDO, THROW:

            llOK = lhRepLogQuery:GET-NEXT(EXCLUSIVE-LOCK, NO-WAIT).

            IF llOK = ?
            THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process query", 1).

            /* No more records, or query is not open */
            IF NOT llOK
            THEN LEAVE.

            /* If for some reason RepLog table is locked then we must skip it */
            IF lhRepLogTable:LOCKED
            THEN NEXT.

            CASE mSendRepLog2MQ():

               WHEN 2
               THEN DO:
                  /* If message was failed to send then we must leave
                     the transaction and try to reconnect */
                  lhRepLogTable:BUFFER-RELEASE().
                  llReconnect = TRUE.
                  LEAVE.
               END.

               WHEN 1
               THEN DO:
                  IF llUseAck
                  THEN ASSIGN
                          lhRepLogTable::SendTime  = NOW
                          lhRepLogTable::SendCount = lhRepLogTable::SendCount + 1
                          .
                  ELSE lhRepLogTable:BUFFER-DELETE().
               END.

               WHEN 3
               THEN lhRepLogTable:BUFFER-DELETE().

               OTHERWISE LEAVE.

            END CASE.

         END.

         FINALLY:
            lhRepLogQuery:QUERY-CLOSE().
         END FINALLY.

      END.

      FINALLY:
         IF llReconnect AND NOT objMsgPublisher:mReConnect()
         THEN RETURN ERROR NEW Progress.Lang.AppError("MQ publisher cannot connect to server!", 1).
      END FINALLY.

   END METHOD.


   METHOD PUBLIC VOID mReload():

      mPurgeDumpHandlers().

      IF NOT mPopulateDumpHandlers()
      THEN RETURN ERROR NEW Progress.Lang.AppError("No tables defined to online HPD interface for queue " + lcQueueName, 1).

   END METHOD.


   METHOD PUBLIC LOGICAL mIsEmpty():

      IF llUseAck
      THEN llFound = lhRepLogTable:FIND-FIRST("WHERE SendTime = ?", NO-LOCK) NO-ERROR.
      ELSE llFound = lhRepLogTable:FIND-FIRST("", NO-LOCK) NO-ERROR.

      RETURN NOT llFound.

   END METHOD.

END CLASS.