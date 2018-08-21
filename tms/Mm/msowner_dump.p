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
{Func/profunc.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump       AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric    AS CHAR     NO-UNDO.
DEF VAR lcDelimiter  AS CHAR     NO-UNDO.
DEF VAR lcModFields  AS CHAR     NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR liCnt        AS INT      NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lhField      AS HANDLE   NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO.

DEF VAR lhTable      AS HANDLE NO-UNDO.
DEF VAR lhCollect    AS HANDLE NO-UNDO.
DEF VAR lcTableName  AS CHAR   NO-UNDO.
DEF VAR lhCLIField   AS HANDLE NO-UNDO. 
DEF VAR lhTsEndField AS HANDLE NO-UNDO. 

DEF TEMP-TABLE ttMsOwner NO-UNDO LIKE MsOwner.

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD CLI AS CHAR
   FIELD TsEnd AS DEC
   INDEX CLI CLI TsEnd.

FUNCTION fCollect RETURNS LOGIC:

   lhCLIField = lhTable:BUFFER-FIELD("CLI").
   lhTsEndField = lhTable:BUFFER-FIELD("TsEnd").

   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.CLI = lhCLIField:BUFFER-VALUE AND
                     ttPicked.TsEnd =  lhTsEndField:BUFFER-VALUE)
   THEN RETURN FALSE.
 
   CREATE ttPicked.
   ttPicked.CLI = lhCLIField:BUFFER-VALUE.
   ttPicked.TsEnd = lhTsEndField:BUFFER-VALUE.  

   lhCollect:BUFFER-CREATE.
   lhCollect:BUFFER-COPY(lhTable).

END FUNCTION.


/***** MAIN START *******/

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

ASSIGN
   lhCollect   = BUFFER ttMSOwner:HANDLE
   lhTable     = BUFFER msowner:HANDLE
   lcTableName = lhTable:NAME.

OUTPUT STREAM sFile TO VALUE(icFile).

DEF VAR ldaEventDate AS DATE NO-UNDO. 
DEF VAR liEventTime AS INT NO-UNDO. 
DEF VAR ldeToday AS DEC NO-UNDO.
DEF VAR ldeLastDump AS DEC NO-UNDO.

ldeToday = Func.Common:mHMS2TS(TODAY, "00:00:00").

Func.Common:mSplitTS(idLastDump,
         OUTPUT ldaEventDate,
         OUTPUT liEventTime).

ldeLastDump = Func.Common:mHMS2TS(ldaEventDate, "00:00:00").

IF icDumpMode = "Full" THEN DO:
   FOR EACH MsOwner NO-LOCK WHERE
            MsOwner.Brand = Syst.Var:gcBrand:
      lhCollect:BUFFER-CREATE.
      lhCollect:BUFFER-COPY(lhTable).
   END.
END.
ELSE DO:
   FOR EACH Eventlog NO-LOCK WHERE
            Eventlog.Eventdate >= ldaEventDate AND
            EventLog.EventDate < TODAY AND
            Eventlog.tablename = "MsOwner" USE-INDEX EventDate:
   
      FIND FIRST MsOwner NO-LOCK WHERE
                 MsOwner.Brand = Syst.Var:gcBrand AND
                 MsOwner.CLI = ENTRY(2,EventLog.Key,CHR(255)) AND
                 MsOWner.TsEnd = DEC(ENTRY(3, EventLog.Key,CHR(255))) NO-ERROR.

      IF AVAIL MsOwner THEN DO:
         IF EventLog.Action = "Create" THEN DO:
         /* Always dump the latest ongoing msowner with create. YTS-11639 */
         fCollect().
            /* IF create-date is not yesterday, other MSOwner can be found. YTS-10342 */
            IF MsOwner.TSBegin <  ldeLastDump OR 
               MsOwner.TSBegin >= ldeToday THEN DO: 
               /* This begin date for create is not yesterday, so continue to search a new one */
               RELEASE MSOwner.
               FIND FIRST MsOwner NO-LOCK WHERE
                          MsOwner.Brand = Syst.Var:gcBrand AND
                          MsOwner.CLI = ENTRY(2,EventLog.Key,CHR(255)) AND
                          MsOwner.TSBegin >= ldeLastDump AND  
                          MSOwner.TSBegin < ldeToday NO-ERROR.
            END.
         END.
         IF AVAIL MsOwner THEN DO: /* now we got a correct MSOwner */
            fCollect().
         END.
      END.
      ELSE DO: /* with first search MSOwner not available */
         /* YTS-10342 New MSOwner-search to be able to find MSOwners 
            whose TSEnd is changed after midnight before dump start. */
         FIND FIRST MsOwner NO-LOCK WHERE
                    MsOwner.Brand = Syst.Var:gcBrand AND
                    MsOwner.CLI = ENTRY(2,EventLog.Key,CHR(255)) AND
                    MsOwner.TSBegin >= ldeLastDump NO-ERROR.
         IF AVAIL MsOwner THEN fCollect().
      END.
   END. 
END.

RUN pWriteFile.
IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE. 
OUTPUT STREAM sFile CLOSE.
SESSION:NUMERIC-FORMAT = lcNumeric.
EMPTY TEMP-TABLE ttMsOwner NO-ERROR.

/***** MAIN END *******/


PROCEDURE pWriteFile:

FOR EACH ttMsOwner NO-LOCK:

   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      lcField = ENTRY(liCnt,lcDumpFields).
      IF lcField BEGINS "#" THEN DO:
         CASE lcField:
            WHEN "#Segment" THEN DO:
               lcValue = fGetSegment(ttMSOwner.CustNum,0).
            END.
            /* NRTR-13 BI(TRACK) changes */
            WHEN "#NWProfile" THEN DO:
               FIND FIRST Customer NO-LOCK WHERE
                  Customer.CustNum = ttMSOwner.Custnum NO-ERROR.
               IF AVAIL Customer THEN
                 lcValue = STRING(Customer.NWProfile).
               ELSE
                 lcValue = "".
            END.
            OTHERWISE lcValue = "".
         END CASE.
      END.

      ELSE DO:
         lhField = lhCollect:BUFFER-FIELD(lcField).
         lcValue = lhField:BUFFER-VALUE.
      END.

      PUT STREAM sFile UNFORMATTED lcValue.

      IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
         PUT STREAM sFile UNFORMATTED lcDelimiter.

   END.

   PUT STREAM sFile UNFORMATTED SKIP.

   oiEvents = oiEvents + 1.

   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "MsOwner" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fColl.
   END.

END.

END PROCEDURE.
