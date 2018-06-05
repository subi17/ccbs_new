/* ----------------------------------------------------------------------
  MODULE .......: ordercust_dump.p
  TASK .........: Collect rows to a delta dump file for Order Customer
  APPLICATION ..: tms
  AUTHOR .......: vikas
  CREATED ......: 04.04.2011
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}
{Syst/tmsconst.i}

DEF INPUT-OUTPUT PARAMETER TABLE-HANDLE ihTempTable.
DEF INPUT  PARAMETER idLastDump       AS DECIMAL   NO-UNDO.
DEF INPUT  PARAMETER icEventSource    AS CHARACTER NO-UNDO.
DEF INPUT  PARAMETER icEventFields    AS CHARACTER NO-UNDO.
DEF INPUT  PARAMETER icModifiedFields AS CHARACTER NO-UNDO.

DEF VAR lhTable          AS HANDLE     NO-UNDO.
DEF VAR lhCollect        AS HANDLE     NO-UNDO.
DEF VAR liPicked         AS INTEGER    NO-UNDO.
DEF VAR liCnt            AS INTEGER    NO-UNDO.
DEF VAR lcTableName      AS CHARACTER  NO-UNDO.
DEF VAR lhOrderField     AS HANDLE     NO-UNDO.
DEF VAR lhRowTypeField   AS HANDLE     NO-UNDO.

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD OrderId AS INTEGER
   FIELD RowType AS INTEGER
   INDEX OrderId IS PRIMARY OrderId RowType.

FORM
    lcTableName AT 2  LABEL "Table  " FORMAT "X(15)"    SKIP
    liPicked    AT 2  LABEL "Picked " FORMAT ">>>>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY 
    TITLE " Collecting " FRAME fColl.

FUNCTION fCollect RETURNS LOGIC:

   lhOrderField = lhTable:BUFFER-FIELD("OrderId").
   lhRowTypeField = lhTable:BUFFER-FIELD("RowType").

   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.OrderId = lhOrderField:BUFFER-VALUE AND
                     ttPicked.RowType = lhRowTypeField:BUFFER-VALUE)
   THEN RETURN FALSE.
 
   CREATE ttPicked.
   ASSIGN ttPicked.OrderId = lhOrderField:BUFFER-VALUE
          ttPicked.RowType = lhRowTypeField:BUFFER-VALUE.  

   lhCollect:BUFFER-CREATE.
   lhCollect:BUFFER-COPY(lhTable).

   liPicked = liPicked + 1.
   IF NOT SESSION:BATCH AND liPicked MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP lcTableName liPicked WITH FRAME fColl.
   END.

END FUNCTION.
                          

/***** MAIN start ********/

ASSIGN
   lhCollect   = ihTempTable:DEFAULT-BUFFER-HANDLE
   lhTable     = BUFFER OrderCustomer:HANDLE
   lcTableName = lhTable:NAME.

DEF VAR ldaEventDate AS DATE NO-UNDO. 
DEF VAR ldeEventTime AS DEC NO-UNDO. 
DEF VAR liEventTime AS INT NO-UNDO. 
DEF VAR lcBrand AS CHAR NO-UNDO. 
DEF VAR liOrderId AS INT NO-UNDO. 

Func.Common:mSplitTS(idLastDump,
         OUTPUT ldaEventDate,
         OUTPUT liEventTime).

FOR EACH EventLog WHERE
         EventLog.EventDate >= ldaEventDate AND
         EventLog.TableName = "Order" NO-LOCK USE-INDEX EventDate:
         
   ldeEventTime = Func.Common:mHMS2TS(EventLog.EventDate,
                         EventLog.EventTime).

   IF ldeEventTime < idLastDump THEN NEXT.

   ASSIGN
      lcBrand = ENTRY(1,EventLog.Key,chr(255))
      liOrderId = int(ENTRY(2,EventLog.Key,chr(255))) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN NEXT.

   FOR FIRST Order WHERE
             Order.Brand = lcBrand AND
             Order.OrderID = liOrderId AND
             Order.OrderType NE {&ORDER_TYPE_ACC} NO-LOCK,
      EACH OrderCustomer OF Order NO-LOCK:
      fCollect().
   END.
END.

DO liCnt = 1 TO NUM-ENTRIES(icEventSource,"|"):
   
   /* check modification from eventlog */
   IF ENTRY(liCnt,icEventSource,"|") = "EventLog" THEN 
      RUN pFindFromEventLog(lhTable,
                            ENTRY(liCnt,icEventFields,"|"),
                            "",
                            idLastDump,
                            "fCollect").

END. 

IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE. 

/****  MAIN end ********/
