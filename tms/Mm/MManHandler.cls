/*------------------------------------------------------------------------
    File        : MManHandler
    Purpose     : 
    Syntax      : 
    Description : Go through MMLog records and send them to MQ using
                  Stomp protocol. Will delete the MMLog record
                  after is has successfully send.
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING PROGRESS.Json.ObjectModel.*.
USING qstomp.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Mm.MManHandler FINAL:

   DEFINE PRIVATE VARIABLE objOMParser     AS CLASS ObjectModelParser   NO-UNDO.
   DEFINE PRIVATE VARIABLE objProducer     AS CLASS Producer            NO-UNDO.
   DEFINE PRIVATE VARIABLE lcMMLogTable    AS CHARACTER INITIAL "MMLog" NO-UNDO.
   DEFINE PRIVATE VARIABLE lhMMLogTable    AS HANDLE                    NO-UNDO.
   DEFINE PRIVATE VARIABLE lhMMLogQuery    AS HANDLE                    NO-UNDO.

   DEFINE PRIVATE VARIABLE llFound            AS LOGICAL     NO-UNDO.
   DEFINE PRIVATE VARIABLE liTransactionSize  AS INTEGER     NO-UNDO.
   DEFINE PRIVATE VARIABLE lcHost             AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE liPort             AS INTEGER     NO-UNDO.
   DEFINE PRIVATE VARIABLE lcUserName         AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcPassWord         AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcQueue            AS CHARACTER   NO-UNDO.


   CONSTRUCTOR PUBLIC MManHandler():

      DO ON ERROR UNDO, THROW:

         liTransactionSize = Syst.Parameters:geti("MMan.Queue.TransactSize", "MMan.Interface").

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            Syst.Parameters:seti("MMan.Queue.TransactSize", "MMan.Interface", 10000).
            liTransactionSize = Syst.Parameters:geti("MMan.Queue.TransactSize", "MMan.Interface").
         END CATCH.

      END.

      ASSIGN
         lcHost     = Syst.Parameters:getc("MMan.Queue.Host", "MMan.Interface")
         liPort     = Syst.Parameters:geti("MMan.Queue.Port", "MMan.Interface")
         lcUserName = Syst.Parameters:getc("MMan.Queue.Username", "MMan.Interface")
         lcPassWord = Syst.Parameters:getc("MMan.Queue.Password", "MMan.Interface")
         lcQueue    = Syst.Parameters:getc("MMan.Queue.Queue", "MMan.Interface")
         .
         
      objOMParser = NEW ObjectModelParser().

      CREATE BUFFER lhMMLogTable FOR TABLE lcMMLogTable.

      objProducer = NEW Producer(lcHost,
                                 liPort,
                                 lcUserName,
                                 lcPassWord,
                                 lcQueue,
                                 ""). /* client-id in connect frame */

      CREATE QUERY lhMMLogQuery.
      lhMMLogQuery:SET-BUFFERS(lhMMLogTable).

      lhMMLogQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK USE-INDEX RequestTime MAX-ROWS &2",lcMMLogTable, STRING(liTransactionSize))).

   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC MManHandler( ):

      IF VALID-HANDLE(lhMMLogQuery)
      THEN DO:
         lhMMLogQuery:QUERY-CLOSE().
         DELETE OBJECT lhMMLogQuery.
      END.
      
      IF VALID-HANDLE(lhMMLogTable)
      THEN DELETE OBJECT lhMMLogTable.  

      IF VALID-OBJECT(objOMParser)
      THEN DELETE OBJECT objOMParser.

      IF VALID-OBJECT(objProducer)
      THEN DELETE OBJECT objProducer.
     
   END DESTRUCTOR.

   METHOD PRIVATE CHARACTER mConvertName
      ( icFieldName AS CHARACTER ):

      CASE icFieldName:
         
         WHEN "RequestId" THEN RETURN "requestId".
         WHEN "BrandName" THEN RETURN "brand".
         WHEN "Originator" THEN RETURN "originator".
         WHEN "SMSRecipient" THEN RETURN "smsRecipient".
         WHEN "EmailRecipient" THEN RETURN "emailRecipient".
         WHEN "PushRecipient" THEN RETURN "pushRecipient".
         WHEN "TemplateId" THEN RETURN "templateId".
         WHEN "Language" THEN RETURN "language".
         WHEN "MessageBody" THEN RETURN "messageBody".
         WHEN "Category" THEN RETURN "category".
         WHEN "SchedulingPolicy" THEN RETURN "schedulingPolicy".
         WHEN "SchedulingPriority" THEN RETURN "schedulingPriority".
         WHEN "Callbacks" THEN RETURN "callbacks".
         WHEN "JsonParam" THEN RETURN "params".
         WHEN "RequestTime" THEN RETURN "".
         OTHERWISE RETURN ERROR NEW Progress.Lang.AppError("Cannot convert field " + icFieldName, 1). 
      
      END CASE.

   END METHOD.


   METHOD PRIVATE LONGCHAR mGiveJsonData
      ( ihBuffer AS HANDLE ):

      DEFINE VARIABLE lii           AS INTEGER           NO-UNDO.
      DEFINE VARIABLE lhField       AS HANDLE            NO-UNDO.
      DEFINE VARIABLE lcElemName    AS CHARACTER         NO-UNDO.
      DEFINE VARIABLE lcJsonData    AS LONGCHAR          NO-UNDO.
      DEFINE VARIABLE objJsonObject AS CLASS JsonObject  NO-UNDO.
      DEFINE VARIABLE objJOParam    AS CLASS JsonObject  NO-UNDO.

      objJsonObject = NEW JsonObject().

      DO lii = 1 TO ihBuffer:NUM-FIELDS:

         lhField = ihBuffer:BUFFER-FIELD(lii).

         IF lhField:BUFFER-VALUE() = lhField:DEFAULT-VALUE AND
            LENGTH(lhField:BUFFER-VALUE()) = LENGTH(lhField:DEFAULT-STRING)
         THEN NEXT.

         lcElemName = mConvertName(lhField:NAME).

         IF lcElemName = ""
         THEN NEXT.
         
         IF lcElemName NE "params"
         THEN objJsonObject:Add(mConvertName(lhField:NAME), lhField:BUFFER-VALUE()).
         ELSE IF lhField:BUFFER-VALUE() NE ""
         THEN objJOParam = CAST(objOMParser:Parse(lhField:BUFFER-VALUE()),JsonObject).

      END.

      IF VALID-OBJECT(objJOParam)
      THEN objJsonObject:Add(mConvertName("JsonParam"),objJOParam).

      objJsonObject:Write(OUTPUT lcJsonData).

      RETURN lcJsonData.

      FINALLY:
         IF VALID-OBJECT(objJOParam)
         THEN DELETE OBJECT objJOParam.

         IF VALID-OBJECT(objJsonObject)
         THEN DELETE OBJECT objJsonObject.
      END FINALLY.

   END METHOD.


   METHOD PUBLIC VOID mProcess():

      DEFINE VARIABLE llOK        AS LOGICAL NO-UNDO.

      DO TRANSACTION ON ERROR UNDO, THROW:

         lhMMLogQuery:QUERY-OPEN().

         DO WHILE TRUE ON ERROR UNDO, THROW:

            llOK = lhMMLogQuery:GET-NEXT(EXCLUSIVE-LOCK, NO-WAIT).

            IF llOK = ?
            THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process query", 1).

            /* No more records, or query is not open */
            IF NOT llOK
            THEN LEAVE.

            /* If for some reason MMLog table is locked then we must skip it */
            IF lhMMLogTable:LOCKED
            THEN NEXT.

            objProducer:mSend(mGiveJsonData(lhMMLogTable),lhMMLogTable::SchedulingPriority).

            lhMMLogTable:BUFFER-DELETE().

         END.

         FINALLY:
            lhMMLogQuery:QUERY-CLOSE().
         END FINALLY.

      END.

   END METHOD.


   METHOD PUBLIC LOGICAL mIsEmpty():

      llFound = lhMMLogTable:FIND-FIRST("", NO-LOCK) NO-ERROR.

      RETURN NOT llFound.

   END METHOD.

END CLASS.