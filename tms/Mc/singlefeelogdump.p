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
We do create two files every day:
1. file: eventlog dump with contain eventlog table information of singlefees.
2. file: non-billed singlefees

Once in a month, 2nd day, the 2.file contain also last 2 billed singlefees:
2. file: Non-billed singlefees and last 2 invoiced singlefees

At TRACK side data between eventlog dump and singlefee dump is merged to be used in TRACK reports. Since all events are not logged, singlefee dump data represents latest state of actual singlefee.
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
DEF VAR lcSpoolDir AS CHAR NO-UNDO.
DEF VAR lcTransDir AS CHAR NO-UNDO. 

FIND FIRST Dumpfile NO-LOCK WHERE Dumpfile.DumpId = icDumpID NO-ERROR.
IF AVAIL DumpFile THEN 
   ASSIGN
      lcSpoolDir = DumpFile.SpoolDir + "/"
      lcTransDir = Dumpfile.TransDir.

/* STREAM sout is for eventlog file */
DEF STREAM sout.
OUTPUT STREAM sout TO VALUE(icFile).
/* STREAM sinfee is for singlefee file */
DEF STREAM sinfee.

ASSIGN
   lcSinFeeFile = "singlefee_#MODE_#DATE_#TIME.txt"
   lcSinFeeFile = REPLACE(lcSinFeeFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                               STRING(MONTH(TODAY),"99") +
                                               STRING(DAY(TODAY),"99"))
    lcSinFeeFile = REPLACE(lcSinFeeFile,"#TIME",
                     REPLACE(STRING(TIME,"hh:mm:ss"),":",""))
    lcSinFeeFile = REPLACE(lcSinFeeFile,"#MODE",icDumpMode).

OUTPUT STREAM sinfee TO VALUE(lcSpoolDir + lcSinFeeFile).

DEFINE VARIABLE liKey AS INTEGER NO-UNDO.
DEFINE VARIABLE lcChanges AS CHARACTER NO-UNDO.
DEFINE VARIABLE liEntries AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE ldaInvDate AS DATE NO-UNDO. 
DEFINE VARIABLE liBilled AS INTEGER NO-UNDO.
DEFINE VARIABLE lisinfees AS INTEGER NO-UNDO. 
      
FUNCTION fNonbilledSinFee_dump RETURNS LOG:
   /* YTS-9314: Non-billed singlefees */
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

      liSinFees= lisinFees + 1.
      IF NOT SESSION:BATCH AND liSinFees MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP liSinFees WITH FRAME fsinfee.
      END.
   END.
   oiEvents = oiEvents + lisinFees.
END.

/* Eventlog search is done every day */
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
   END. 
   PUT STREAM sout UNFORMATTED lcChanges SKIP.

   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents WITH FRAME fColl.
   END.
END.
/* Run daily Non-billed singlefee dump */
fNonbilledSinFee_dump().

IF icDumpMode = "Full" THEN DO:
   ASSIGN
      ldaInvDate = ADD-INTERVAL(TODAY, -1, "months").
      ldainvdate = DATE(MONTH(ldaInvDate),1,YEAR(ldaInvDate)).
   /* YTS-9314: billed singlefees from last 2 months */
   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand = "1" AND
            Invoice.InvDate >= ldainvdate,
       EACH SingleFee NO-LOCK WHERE
            SingleFee.Invnum = Invoice.InvNum:
      PUT STREAM sinFee UNFORMATTED
         SingleFee.FMItemID "|"
         SingleFee.KeyValue "|"
         SingleFee.CustNum "|"
         SingleFee.BillPeriod "|"
         SingleFee.BillCode "|"
         SingleFee.Amt "|"
         SingleFee.InvNum SKIP.

      liBilled = liBilled + 1.
      IF NOT SESSION:BATCH AND liBilled MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP liBilled WITH FRAME fbil.
      END.
   END.
   oiEvents = oiEvents + libilled.
END.
OUTPUT STREAM sout CLOSE.
OUTPUT STREAM sinfee CLOSE.

UNIX SILENT VALUE("mv " + lcSpooldir + lcSinFeeFile + " " + lcTransDir).

IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE.
