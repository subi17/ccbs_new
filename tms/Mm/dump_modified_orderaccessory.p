/* ----------------------------------------------------------------------
  MODULE .......: dump_modified_orderaccessory.p
  TASK .........: Collect rows to a dump file for new orderaccessory
  APPLICATION ..: tms
  AUTHOR .......: rafaeldv
  CREATED ......: 
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}

DEF INPUT-OUTPUT PARAMETER TABLE-HANDLE ihTempTable.
DEF INPUT  PARAMETER idLastDump       AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icModifiedFields AS CHAR NO-UNDO.

DEF VAR lhTable      AS HANDLE NO-UNDO.
DEF VAR lhCollect    AS HANDLE NO-UNDO.
DEF VAR liPicked     AS INT    NO-UNDO.
DEF VAR liCnt        AS INT    NO-UNDO.
DEF VAR lcTableName  AS CHAR   NO-UNDO.
DEF VAR lhOrderField     AS HANDLE NO-UNDO.
DEF VAR lhProductCodeField   AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD OrderId AS INT
   FIELD ProductCode AS CHAR
   INDEX OrderId OrderId ProductCode.

FORM
    lcTableName AT 2  LABEL "Table  " FORMAT "X(15)"    SKIP
    liPicked    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY 
    TITLE " Collecting " FRAME fColl.

FUNCTION fCollect RETURNS LOGIC:

   lhOrderField = lhTable:BUFFER-FIELD("OrderId").
   lhProductCodeField = lhTable:BUFFER-FIELD("ProductCode").

   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.OrderId = lhOrderField:BUFFER-VALUE AND
                     ttPicked.ProductCode =  lhProductCodeField:BUFFER-VALUE)
   THEN RETURN FALSE.
 
   CREATE ttPicked.
   ttPicked.OrderId = lhOrderField:BUFFER-VALUE.
   ttPicked.ProductCode = lhProductCodeField:BUFFER-VALUE.  

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
   lhTable     = BUFFER OrderAccessory:HANDLE
   lcTableName = lhTable:NAME.

DEF VAR ldaLastDumpDate AS DATE NO-UNDO. 
DEF VAR liLastDumpTime AS INT NO-UNDO. 
DEF VAR lcLastDumpTime AS CHAR NO-UNDO. 
DEF VAR liOrderId AS INT NO-UNDO. 

fSplitTs(idLastDump, OUTPUT ldaLastDumpDate, OUTPUT liLastDumpTime).

lcLastDumpTime = STRING(liLastDumpTime,"hh:mm:ss").

FOR EACH EventLog NO-LOCK where
         EventLog.eventdate >= ldaLastDumpDate and
         EventLog.tablename = "order" and
         EventLog.action = "CREATE" use-index eventdate:

   IF EventLog.EventDate EQ ldaLastDumpDate AND
      EventLog.eventtime < lcLastDumpTime THEN NEXT.
   
   liOrderID = int(entry(2,key,chr(255))) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN NEXT.
   
   FOR EACH OrderAccessory NO-LOCK WHERE
            OrderAccessory.Brand = gcBrand AND
            OrderAccessory.OrderId = liOrderID:
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







