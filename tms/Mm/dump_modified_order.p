/* ----------------------------------------------------------------------
  MODULE .......: dump_modified_order.p
  TASK .........: dump file for modified orders with Status Change Stamp
  APPLICATION ..: TMS
  AUTHOR .......: ivvekov
  CREATED ......: 21.07.14
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}
{Func/create_eventlog.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/forderstamp.i}
{Func/fixedlinefunc.i}
{Func/profunc.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric    AS CHAR     NO-UNDO.
DEF VAR lcDelimiter  AS CHAR     NO-UNDO.
DEF VAR ldaModified  AS DATE     NO-UNDO.
DEF VAR liCnt        AS INT      NO-UNDO.
DEF VAR lhTable      AS HANDLE   NO-UNDO.
DEF VAR lcKeyFields  AS CHAR     NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lhField      AS HANDLE   NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO.
DEF VAR ldtLastDump  AS DATETIME NO-UNDO.
DEF VAR lcModFields  AS CHAR     NO-UNDO.
DEF VAR lhReqField   AS HANDLE   NO-UNDO.
DEF VAR lhCollect    AS HANDLE   NO-UNDO.
DEF VAR ldCrStamp    AS DEC      NO-UNDO.

DEF TEMP-TABLE ttOrder NO-UNDO LIKE Order.
  
FUNCTION fCollect RETURNS LOGIC:

   lhReqField = lhTable:BUFFER-FIELD("OrderId").
   IF CAN-FIND(FIRST ttOrder WHERE
                     ttOrder.Brand   = Syst.Var:gcBrand AND
                     ttOrder.OrderId = lhReqField:BUFFER-VALUE)
   THEN RETURN FALSE.

   lhCollect:BUFFER-CREATE.
   lhCollect:BUFFER-COPY(lhTable).

END FUNCTION.

DEF STREAM sFile.

lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   ASSIGN lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
          lcModFields = DumpFile.EventLogFields.
   
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN 
      lcDelimiter = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

FOR EACH DFField OF DumpFile NO-LOCK WHERE
         DFField.ToDate   >= TODAY AND
         DFField.FromDate <= TODAY
BY DFField.OrderNbr:
   lcDumpFields = lcDumpFields + (IF lcDumpFields > "" THEN "," ELSE "") +
                  DFField.DFField.
END.

Func.Common:mSplitTS(idLastDump,
         OUTPUT ldaModified,
         OUTPUT liCnt).

ASSIGN
   lhCollect   = BUFFER ttOrder:HANDLE
   lhTable     = BUFFER Order:HANDLE
   lcKeyFields = fEventKeyFields(lhTable)
   ldtLastDump = Func.Common:mTimeStamp2DateTime(idLastDump)
   ldCrStamp   = Func.Common:mMakeTS().

OUTPUT STREAM sFile TO VALUE(icFile).

DEF VAR liOrderStatus AS INT NO-UNDO. 
DEF VAR liRowType     AS INT NO-UNDO. 

IF icDumpMode = "Full" THEN DO:
   FOR EACH Order NO-LOCK WHERE
            Order.Brand = Syst.Var:gcBrand:
      IF Order.OrderType EQ {&ORDER_TYPE_ACC} THEN NEXT.
      fCollect().
   END.
END.
ELSE DO:
 /* Dump all the orders which are not in final stage
    (eg: closed,delivered,closed by fraud and auto closed) */
   DO liOrderStatus = 1 TO 100:
   
      IF LOOKUP(STRING(liOrderStatus),{&ORDER_INACTIVE_STATUSES}) > 0 THEN NEXT.

      FOR EACH Order NO-LOCK WHERE
               Order.Brand = Syst.Var:gcBrand AND
               Order.StatusCode = STRING(liOrderStatus):
         IF Order.OrderType EQ {&ORDER_TYPE_ACC} THEN NEXT.
         fCollect().
      END.
   END.

   /* Dump only orders which are in final stage based on the timestamp */
   DO liRowType = 1 TO 4:
      FOR EACH OrderTimeStamp NO-LOCK WHERE
               OrderTimeStamp.Brand = Syst.Var:gcBrand AND
               OrderTimeStamp.RowType = liRowType AND
               OrderTimeStamp.TimeStamp >= idLastDump,
          FIRST Order NO-LOCK WHERE
                Order.Brand = Syst.Var:gcBrand AND
                Order.OrderID = OrderTimeStamp.OrderID:
         IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 THEN NEXT.
         fCollect().
      END.
   END.

   /* collect only those that have been modified in the eventlog 
      collect also the new ones */
   DO liCnt = 1 TO NUM-ENTRIES(icEventSource,"|"):
   
      /* check modification from eventlog */
      IF ENTRY(liCnt,icEventSource,"|") = "EventLog" THEN 
         RUN pFindFromEventLog(lhTable,
                               ENTRY(liCnt,icEventFields,"|"),
                               "",
                               idLastDump,
                               "fCollect").
      
      /* check new orders */
      ELSE IF ENTRY(liCnt,icEventSource,"|") = "field" AND
              ENTRY(liCnt,icEventFields,"|") = "CrStamp" 
      THEN DO:
      
         FOR EACH Order NO-LOCK  USE-INDEX Stamp WHERE
                  Order.Brand = Syst.Var:gcBrand AND
                  Order.CrStamp >= idLastDump:

                  fCollect().
         END.
      END.
   END.
END.

RUN pWriteFile.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.

EMPTY TEMP-TABLE ttOrder NO-ERROR.
 
PROCEDURE pWriteFile:

FOR EACH ttOrder NO-LOCK:

    DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

        lcField = ENTRY(liCnt,lcDumpFields).
         
        IF lcField BEGINS "#" THEN DO:
            CASE lcField:
            WHEN "#Segment" THEN DO:
               FIND FIRST Ordercustomer WHERE
                          Ordercustomer.brand EQ Syst.Var:gcBrand AND
                          Ordercustomer.orderid EQ ttOrder.orderid AND
                          Ordercustomer.rowtype EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-LOCK NO-ERROR.
               IF AVAIL ordercustomer THEN           
                  lcValue = fGetSegment(ordercustomer.CustNum, 
                                        ordercustomer.orderid).
            END.
            WHEN "#SCStamp" THEN DO:
               lcValue = STRING(fGetOrderStamp(ttOrder.OrderID,"1")).
               IF icDumpMode = "Full" AND
                  lcValue = "0" THEN lcValue = STRING(ldCrStamp).
            END.
            WHEN "#OrderChannel" THEN DO:
               IF fIsConvergenceTariff(ttOrder.CLIType) THEN
                  lcValue = REPLACE(ttOrder.orderChannel,"fusion","conv").
               ELSE
                  lcValue = ttOrder.orderChannel.
            END.              
            OTHERWISE lcValue = "".
            END CASE.
        END.

        ELSE DO:
           lhField = lhCollect:BUFFER-FIELD(lcField).
           lcValue = lhField:BUFFER-VALUE.
        END.

        PUT STREAM sFile UNFORMATTED
           lcValue.

        IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
        PUT STREAM sFile UNFORMATTED
        lcDelimiter.

    END.
 
    PUT STREAM sFile UNFORMATTED
        SKIP.

    oiEvents = oiEvents + 1.

    IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
        PAUSE 0. 
        DISP oiEvents LABEL "Order" 
        WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
           TITLE " Collecting " FRAME fQty.
    END.

END.

END PROCEDURE.

