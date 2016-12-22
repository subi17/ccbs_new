/* ----------------------------------------------------------------------
  MODULE .......: singlefeelogdump.p 
  TASK .........: Dumps SingleFee events. TRACK: Fees datamart. YDR-538
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 03.09.12
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
/*
YTS-9314: Dump logic was changed.
Daily we do two files:
1. file: eventlog dump with contain eventlog table information of singlefees.
2. file: non-billed singlefees

Once in a month 2nd day:
1. file: Non-billed singlefees and last invoiced singlefees
*/

{commali.i}
{timestamp.i}
{cparam2.i}
{tmsconst.i}
{dumpfile_run.i}

DEF INPUT PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcsinfeeFile AS CHAR NO-UNDO.
DEF STREAM sout.
OUTPUT STREAM sout TO VALUE(icFile).
DEF STREAM sinfee.

ASSIGN
   lcSinFeeFile = "singlefee_#MODE_#DATE_#TIME.txt"
   lcSinFeeFile = REPLACE(lcSinFeeFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                               STRING(MONTH(TODAY),"99") +
                                               STRING(DAY(TODAY),"99"))
    lcSinFeeFile = REPLACE(lcSinFeeFile,"#TIME",
                     REPLACE(STRING(TIME,"hh:mm:ss"),":",""))
    lcSinFeeFile = REPLACE(lcSinFeeFile,"#MODE",icDumpMode).

OUTPUT STREAM sinfee TO VALUE(lcSinFeeFile).

DEFINE VARIABLE liKey AS INTEGER NO-UNDO.
DEFINE VARIABLE lcChanges AS CHARACTER NO-UNDO.
DEFINE VARIABLE liEntries AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE ldaInvDate AS DATE NO-UNDO. 

/* This is needed daily and also once in a month for full dump */
FUNCTION fNonbilledSinFee_dump RETURNS LOG:
   /* YTS-9314: Non-billed fulldump
       done daily modified and full dump */
   FOR EACH SingleFee NO-LOCK WHERE
            SingleFee.InvNum = 0:
      PUT STREAM sinFee UNFORMATTED
         SingleFee.FMItemID "|"
         SingleFee.KeyValue "|"
         SingleFee.CustNum "|"
         SingleFee.BillPeriod "|"
         SingleFee.BillCode "|"
         SingleFee.Amt "|"
         SingleFee.InvNum SKIP.

      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 1.
         DISP oiEvents WITH FRAME fColl.
      END.
   END.
END.

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
 TITLE " Collecting " FRAME fColl.

IF icDumpMode = "Modified" THEN DO:
   /* Eventlog search */
   FOR EACH eventlog NO-LOCK WHERE
            eventlog.eventdate = TODAY - 1 AND
            eventlog.tablename = "SingleFee" USE-INDEX EventDate:

      liKey = INT(ENTRY(4,eventlog.key,CHR(255))) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.

      PUT STREAM sout UNFORMATTED
         eventlog.action "|"
         eventlog.eventdate " " eventlog.eventtime "|"
         eventlog.memo "|"
         eventlog.usercode "|"
         liKey "|".

      IF eventlog.action NE "Delete" THEN DO:

         lcChanges = "".

         IF eventlog.action EQ "Modify" THEN DO:

            ASSIGN
               lcChanges = ""
               liEntries  = num-entries(Eventlog.DataValues, CHR(255)) / 3.

            DO i = 0 TO liEntries  - 1 :
               lcChanges = lcChanges + ";" +
                  entry(3 * i + 1,Eventlog.DataValues,CHR(255)) + ":" +
                  entry(3 * i + 2,Eventlog.DataValues,CHR(255)) + ":" +
                  entry(3 * i + 3,Eventlog.DataValues,CHR(255)).
            END.

            IF lcChanges > "" THEN lcChanges = SUBSTRING(lcChanges,2).
         END.
         PUT STREAM sout UNFORMATTED
            lcChanges SKIP.
      END. /* IF eventlog.action NE "Delete" THEN DO: */

      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents WITH FRAME fColl.
      END.
   END.
   /* Run daily Non-billed singlefee dump */
   fNonbilledSinFee_dump().
END.
ELSE DO:
   /* Non-billed singlefee counter */
   fNonbilledSinFee_dump().
   ASSIGN
      ldaInvDate = ADD-INTERVAL(TODAY, -1, "months").
      ldainvdate = DATE(MONTH(ldaInvDate),1,YEAR(ldaInvDate)).
   /* YTS-9314: billed on last month */
   FOR EACH SingleFee NO-LOCK WHERE
            SingleFee.Invnum > 0,
      FIRST Invoice NO-LOCK WHERE
            Invoice.Brand = "1" AND
            Invoice.InvNum = SingleFee.InvNum AND
            Invoice.InvDate >= ldainvdate:
      PUT STREAM sinFee UNFORMATTED
         SingleFee.FMItemID "|"
         SingleFee.KeyValue "|"
         SingleFee.CustNum "|"
         SingleFee.BillPeriod "|"
         SingleFee.BillCode "|"
         SingleFee.Amt "|"
         SingleFee.InvNum SKIP.

      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 1.
         DISP oiEvents WITH FRAME fColl.
      END.
   END.
END.

OUTPUT STREAM sout CLOSE.
OUTPUT STREAM sinfee CLOSE.
IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE.
