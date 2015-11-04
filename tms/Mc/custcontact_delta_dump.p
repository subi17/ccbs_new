/* ----------------------------------------------------------------------
  MODULE .......: custcontact_delta_dump.p
  TASK .........: Collect rows to a dump file for customer contact records
  APPLICATION ..: tms
  AUTHOR .......: ivailo
  CREATED ......: 8.7.2015
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}

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
   lhTable     = BUFFER CustContact:HANDLE
   lcTableName = lhTable:NAME.

DEF VAR ldaLastDumpDate AS DATE NO-UNDO.
DEF VAR liLastDumpTime AS INT NO-UNDO.
DEF VAR lcLastDumpTime AS CHAR NO-UNDO.
DEF VAR liCustNum AS INT NO-UNDO.

fSplitTs(idLastDump, OUTPUT ldaLastDumpDate, OUTPUT liLastDumpTime).

lcLastDumpTime = STRING(liLastDumpTime,"hh:mm:ss").

FOR EACH EventLog NO-LOCK WHERE
         EventLog.EventDate >= ldaLastDumpDate AND
         EventLog.TableName = "CustContact"
         USE-INDEX EventDate:

   IF EventLog.EventDate EQ ldaLastDumpDate AND
      EventLog.EventTime < lcLastDumpTime THEN NEXT.

   liCustNum = int(ENTRY(2,EventLog.Key,CHR(255))) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN NEXT.

   FOR EACH CustContact NO-LOCK WHERE
            CustContact.Brand = gcBrand AND
            CustContact.CustNum = liCustNum AND
            CustContact.CustType = 5:
      fCollect().
   END.
END.

IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE.

/****  MAIN end ********/
