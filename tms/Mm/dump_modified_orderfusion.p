/* ------------------------------------------------------------------------
  MODULE .......: dump_modified_orderfusion.p
  TASK .........: Collect rows to a dump file for new/modified orderfusion
  APPLICATION ..: tms
  AUTHOR .......: Vikas
  CREATED ......: 
  CHANGED ......: 
  Version ......: yoigo
-------------------------------------------------------------------------*/

{Syst/commali.i}
{Syst/dumpfile_run.i}

DEF INPUT-OUTPUT PARAMETER TABLE-HANDLE ihTempTable.
DEF INPUT  PARAMETER idLastDump       AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icModifiedFields AS CHAR NO-UNDO.

DEF VAR liPicked           AS INT  NO-UNDO.
DEF VAR liCnt              AS INT  NO-UNDO.
DEF VAR lcTableName        AS CHAR NO-UNDO.
DEF VAR liOrderId          AS INT  NO-UNDO. 

DEF VAR lhTable            AS HANDLE NO-UNDO.
DEF VAR lhCollect          AS HANDLE NO-UNDO.
DEF VAR lhOrderField       AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD OrderId AS INT
   INDEX OrderId OrderId.

FORM
    lcTableName AT 2  LABEL "Table  " FORMAT "X(15)"    SKIP
    liPicked    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY 
    TITLE " Collecting " FRAME fColl.

FUNCTION fCollect RETURNS LOGIC:

   lhOrderField = lhTable:BUFFER-FIELD("OrderId").

   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.OrderId = lhOrderField:BUFFER-VALUE)
   THEN RETURN FALSE.
 
   CREATE ttPicked.
          ttPicked.OrderId = lhOrderField:BUFFER-VALUE.

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
   lhTable     = BUFFER OrderFusion:HANDLE
   lcTableName = lhTable:NAME.

FOR EACH OrderFusion NO-LOCK WHERE
         OrderFusion.UpdateTS >= idLastDump:
   fCollect().
END.

IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE. 

/****  MAIN end ********/

