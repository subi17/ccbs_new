/* Recreate replog records from hpd log data.
   Log data lines should be presented like:

ordercanal|0x0000000000016608|OrderAction|CREATE|2016-09-21T14:24:35.197+02:00|70039109ÿQ25Discountÿ35|1|70039109|Q25Discount|35|Q25Discount
ordercanal|0x0000000000016600|DMSDoc|CREATE|2016-09-21T14:31:01.947+02:00|15303ÿ4ÿ20160921.52261|1|15303|4||SENT||   
   
*/

DEFINE VARIABLE gcHPDDataFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcDataBase    AS CHARACTER NO-UNDO.

ASSIGN
   gcHPDDataFile = "/apps/yoigo/var/log/resend_this.txt"
   gcDataBase    = "ordercanal"
   .

/* No need to change the code after this line ... */

DEFINE TEMP-TABLE ttKeyValueQuery NO-UNDO
   FIELD TableName     AS CHARACTER
   FIELD KeyValueQuery AS CHARACTER
   INDEX TableName IS PRIMARY UNIQUE TableName.

FUNCTION fStoreKeyValueQuery RETURNS LOGICAL
   (icTableName AS CHARACTER,
    icKeyValueQuery AS CHARACTER):
   
   CREATE ttKeyValueQuery.
   ASSIGN
      ttKeyValueQuery.TableName     = icTableName
      ttKeyValueQuery.KeyValueQuery = icKeyValueQuery
      .

   RETURN FALSE.

END FUNCTION.

FUNCTION fGetQuery RETURNS CHARACTER
   (icTableName AS CHARACTER):

   FIND ttKeyValueQuery WHERE ttKeyValueQuery.TableName = icTableName NO-ERROR.

   IF AVAILABLE ttKeyValueQuery
   THEN RETURN ttKeyValueQuery.KeyValueQuery.

   RETURN "".

END FUNCTION.

FUNCTION fReturnFullQuery RETURNS CHARACTER
   ( icTableName AS CHARACTER,
     icValues    AS CHARACTER ):
   
   DEFINE VARIABLE lii            AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcQuery        AS CHARACTER NO-UNDO.
   
   lcQuery = fGetQuery(icTableName).
   
   IF lcQuery = ""
   THEN RETURN "".
   
   DO lii = 1 TO NUM-ENTRIES(icValues, CHR(255)):
      lcQuery = REPLACE(lcQuery, "&" + STRING(lii), ENTRY(lii, icValues, CHR(255))).
   END.
   
   RETURN SUBSTITUTE("WHERE &1",lcQuery).

END FUNCTION.

CASE gcDataBase:   
   WHEN "ordercanal"
   THEN DO:
      fStoreKeyValueQuery("Barring", "MsSeq = &1 AND BarringCode = '&2' AND EventTS = &4 AND BarringStatus = '&3'").
      fStoreKeyValueQuery("BarringConf", "BarringCode = '&1'").
      fStoreKeyValueQuery("DMS", "DMSID = &1").
      fStoreKeyValueQuery("DMSDoc", "DMSID = &1 AND DocTypeID = '&2' AND DocStatusTS = &3").
      fStoreKeyValueQuery("MobSub", "MSSeq = &1").
      fStoreKeyValueQuery("Order", "Brand = '1' AND OrderId = &1").
      fStoreKeyValueQuery("OrderAction", "Brand = '1' AND OrderId = &1 AND ItemType = '&2' AND ItemKey = '&3'").
      fStoreKeyValueQuery("OrderCustomer", "Brand = '1' AND OrderId = &1 AND RowType = &2").
      fStoreKeyValueQuery("OrderDelivery", "Brand = '1' AND OrderId = &1 AND LOTimeStamp = &2").
      fStoreKeyValueQuery("OrderFusion", "Brand = '1' AND OrderId = &1").
      fStoreKeyValueQuery("PrepaidRequest", "Brand = '1' AND PPRequest = &1").
      fStoreKeyValueQuery("SubsTerminal", "TerminalID = &1").
      fStoreKeyValueQuery("TermReturn", "OrderId = &1 AND ReturnTS = &2").
      fStoreKeyValueQuery("TopupSchemeRow", "TopupSchemeRowID = &1").
      fStoreKeyValueQuery("CLIType", "CLIType = '&1'").
   END.
   OTHERWISE DO:
      MESSAGE "Keyvalue specifications for database " + gcDatabase + " is not yet specified"
             VIEW-AS ALERT-BOX.
      RETURN.
   END.
END CASE.

DEFINE TEMP-TABLE ttBufferHandle NO-UNDO
   FIELD TableName    AS CHARACTER
   FIELD BufferHandle AS HANDLE
   INDEX TableName IS PRIMARY UNIQUE TableName.

DEFINE STREAM instr.

RUN pCreateReplog.

FUNCTION fGetBufferHandle RETURNS HANDLE
   (icTableName AS CHARACTER):

   DEFINE VARIABLE lhBuffer AS HANDLE NO-UNDO.

   FIND ttBufferHandle WHERE ttBufferHandle.TableName = icTableName NO-ERROR.
   IF NOT AVAILABLE ttBufferHandle
   THEN DO:
      CREATE BUFFER lhBuffer FOR TABLE icTableName. 
      CREATE ttBufferHandle.
      ASSIGN
         ttBufferHandle.TableName    = icTableName
         ttBufferHandle.BufferHandle = lhBuffer
         .
   END.

   RETURN ttBufferHandle.BufferHandle.

END FUNCTION.

PROCEDURE pCreateReplog:

   DEFINE VARIABLE lhTableBuffer  AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhRepLogBuffer AS HANDLE NO-UNDO.
   
   CREATE BUFFER lhRepLogBuffer FOR TABLE gcDataBase + ".RepLog".
   
   DEFINE VARIABLE lcTableName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcEventType AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcKeyValue  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcQuery     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llFindOK    AS LOGICAL   NO-UNDO.

   INPUT STREAM instr FROM VALUE(gcHPDDataFile).

   REPEAT:
      
      IMPORT STREAM instr UNFORMATTED lcLine.

      IF NUM-ENTRIES(lcLine, "|") < 6 THEN NEXT.
      
      ASSIGN
         lcTableName   = ENTRY(3,lcLine,"|")
         lhTableBuffer = fGetBufferHandle(lcTableName)
         lcEventType   = ENTRY(4,lcLine,"|")
         lcKeyValue    = ENTRY(6,lcLine,"|")
         .
         
      IF lcEventType NE "DELETE"
      THEN DO:   
         lcQuery = fReturnFullQuery(lcTableName, lcKeyValue).

         IF lcQuery = ""
         THEN NEXT.
         
         llFindOK = lhTableBuffer:FIND-FIRST(lcQuery, NO-LOCK) NO-ERROR.

         IF NOT llFindOK
         THEN NEXT.
      END.

      DO TRANSACTION:
         lhReplogBuffer:BUFFER-CREATE().
   
         ASSIGN
            lhReplogBuffer::TableName = lcTableName
            lhReplogBuffer::EventType = lcEventType
            lhReplogBuffer::EventTime = NOW
            lhReplogBuffer::KeyValue  = IF lcEventType = "DELETE" THEN lcKeyValue ELSE ""
            lhReplogBuffer::RowID     = IF lcEventType = "DELETE" THEN "" ELSE STRING(lhTableBuffer:ROWID)
            .
      END.
   END.
   
   FINALLY:
      INPUT STREAM instr CLOSE.
      IF VALID-HANDLE(lhRepLogBuffer)
      THEN DELETE OBJECT lhRepLogBuffer.
      
      FOR EACH ttBufferHandle:
         IF VALID-HANDLE(ttBufferHandle.BufferHandle)
         THEN DELETE OBJECT ttBufferHandle.BufferHandle.
      END.
   END FINALLY.
END.