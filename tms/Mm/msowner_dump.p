/* ----------------------------------------------------------------------
  MODULE .......: msowner_dump.p 
  TASK .........: Dumps Msowner events. YTS-10088
                  previous implementation without logic module turned to be
                  too slow.
  APPLICATION ..: TMS
  AUTHOR .......: Janne Tourunen
  CREATED ......: 19.01.2017
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}

DEF INPUT-OUTPUT PARAMETER TABLE-HANDLE ihTempTable.
DEF INPUT  PARAMETER idLastDump       AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icModifiedFields AS CHAR NO-UNDO.

DEF VAR lhTable         AS HANDLE NO-UNDO.
DEF VAR lhCollect       AS HANDLE NO-UNDO.
DEF VAR liPicked        AS INT    NO-UNDO.
DEF VAR lcTableName     AS CHAR   NO-UNDO.
DEF VAR lhCLIField      AS HANDLE NO-UNDO.
DEF VAR lhCLIEventField AS HANDLE NO-UNDO.
DEF VAR lhTsEndField    AS HANDLE NO-UNDO.
DEF VAR lhTsBeginField  AS HANDLE NO-UNDO.
DEF VAR ldtTsBeginDate  AS DATE   NO-UNDO.
DEF VAR liTsBeginDate   AS INT    NO-UNDO.

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD CLI AS CHAR
   FIELD TsEnd AS DEC
   INDEX CLI CLI TsEnd.

FORM
    lcTableName AT 2  LABEL "Table  " FORMAT "X(15)"    SKIP
    liPicked    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY 
    TITLE " Collecting " FRAME fColl.

FUNCTION fCollect RETURNS LOGIC:

   ASSIGN lhCLIField      = lhTable:BUFFER-FIELD("CLI")
          lhCLIEventField = lhTable:BUFFER-FIELD("CliEvent")
          lhTsBeginField  = lhTable:BUFFER-FIELD("TsBegin")
          lhTsEndField    = lhTable:BUFFER-FIELD("TsEnd").

   fSplitTS(lhTsBeginField:BUFFER-VALUE,
            ldtTsBeginDate,
            liTsBeginDate).

   /* YTS-10342 Avoid sending MsOwner records which are been delayed more than
      one day (with row CLI_EVENT = 'C') */
   If lhCLIEventField:BUFFER-VALUE EQ "C" AND
      ldtTsBeginDate < (TODAY - 2)        THEN NEXT.

   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.CLI   = lhCLIField:BUFFER-VALUE AND
                     ttPicked.TsEnd = lhTsEndField:BUFFER-VALUE)
   THEN RETURN FALSE.
 
   CREATE ttPicked.
          ttPicked.CLI   = lhCLIField:BUFFER-VALUE.
          ttPicked.TsEnd = lhTsEndField:BUFFER-VALUE.  

   lhCollect:BUFFER-CREATE.
   lhCollect:BUFFER-COPY(lhTable).

   liPicked = liPicked + 1.
   IF NOT SESSION:BATCH AND liPicked MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP lcTableName liPicked WITH FRAME fColl.
   END.

END FUNCTION.

/***** MAIN START *******/

ASSIGN
   lhCollect   = ihTempTable:DEFAULT-BUFFER-HANDLE
   lhTable     = BUFFER msowner:HANDLE
   lcTableName = lhTable:NAME.

DEF VAR ldaEventDate  AS DATE NO-UNDO. 
DEF VAR liEventTime   AS INT  NO-UNDO. 
DEF VAR ldeTodayStamp AS DEC  NO-UNDO.

ASSIGN ldeTodayStamp = YEAR(TODAY) * 10000 +
                       MONTH(TODAY) * 100  +
                       DAY(TODAY).

fSplitTS(idLastDump,
         OUTPUT ldaEventDate,
         OUTPUT liEventTime).

FOR EACH Eventlog NO-LOCK WHERE
         Eventlog.Eventdate >= ldaEventDate AND
         EventLog.EventDate < TODAY         AND
         Eventlog.tablename = "MsOwner" USE-INDEX EventDate:
   
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.Brand = gcBrand                              AND
              MsOwner.CLI   = ENTRY(2,EventLog.Key,CHR(255))       AND
              MsOWner.TsEnd = DEC(ENTRY(3, EventLog.Key,CHR(255))) AND
              MsOwner.TsBeg < ldeTodayStamp NO-ERROR.
   IF AVAIL MsOwner THEN fCollect().
      
END. 

IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE.

/***** MAIN END *******/
