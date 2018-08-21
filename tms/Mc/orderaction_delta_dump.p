/* ----------------------------------------------------------------------
  MODULE .......: orderaction_delta_dump.p
  TASK .........: Collect rows to a dump file for new orderaction records
                  YDR-494
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 16.5.2012
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/dumpfile_run.i}
{Syst/tmsconst.i}

DEF INPUT-OUTPUT PARAMETER TABLE-HANDLE ihTempTable.
DEF INPUT PARAMETER idLastDump       AS DEC  NO-UNDO.
DEF INPUT PARAMETER icEventSource    AS CHAR NO-UNDO.
DEF INPUT PARAMETER icEventFields    AS CHAR NO-UNDO.
DEF INPUT PARAMETER icModifiedFields AS CHAR NO-UNDO.

DEF VAR lhTable      AS HANDLE NO-UNDO.
DEF VAR lhCollect    AS HANDLE NO-UNDO.
DEF VAR liPicked     AS INT    NO-UNDO.
DEF VAR liCnt        AS INT    NO-UNDO.
DEF VAR lcTableName  AS CHAR   NO-UNDO.

FORM
    lcTableName AT 2  LABEL "Table  " FORMAT "X(15)"    SKIP
    liPicked    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY 
    TITLE " Collecting " FRAME fColl.

FUNCTION fCollect RETURNS LOGIC:

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
   lhTable     = BUFFER OrderAction:HANDLE
   lcTableName = lhTable:NAME.

DEF VAR ldaLastDumpDate AS DATE NO-UNDO. 
DEF VAR liLastDumpTime AS INT NO-UNDO. 
DEF VAR lcLastDumpTime AS CHAR NO-UNDO. 
DEF VAR liOrderId AS INT NO-UNDO. 

Func.Common:mSplitTS(idLastDump, OUTPUT ldaLastDumpDate, OUTPUT liLastDumpTime).

lcLastDumpTime = STRING(liLastDumpTime,"hh:mm:ss").
 
FOR EACH EventLog NO-LOCK where
         EventLog.eventdate >= ldaLastDumpDate and
         EventLog.tablename = "order" and
         EventLog.action = "CREATE" use-index eventdate:

   IF EventLog.EventDate EQ ldaLastDumpDate AND
      EventLog.eventtime < lcLastDumpTime THEN NEXT.
   
   liOrderID = int(entry(2,key,chr(255))) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN NEXT.

   FIND Order NO-LOCK WHERE
        Order.Brand = Syst.Var:gcBrand AND
        Order.OrderID = liOrderID NO-ERROR.
   IF AVAIL Order AND Order.OrderType EQ {&ORDER_TYPE_ACC} THEN NEXT.
   
   FOR EACH OrderAction NO-LOCK WHERE
            OrderAction.Brand = Syst.Var:gcBrand AND
            OrderAction.OrderId = liOrderID:
      fCollect().
   END.
END.

IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE. 

/****  MAIN end ********/
