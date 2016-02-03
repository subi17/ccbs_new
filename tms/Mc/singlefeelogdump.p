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
1-Action Executed (String = Create/Modify/Delete)
2-TimeStamp when it was executed (Datetime)
3-Process that generate this action (String)
4-User that execute the action (String= TMS user identifier)
5-Fee Unique Identifier (in case of FFItem it is FFItemNum+FFNum, in case of SingleFee it is FMItemId)
6-MsSeq (Integer)
7-CustNum (Integer)
8-BillPeriod (when it should be billed, Integer 999999)
9-Billing Item (knows in TMS as BillCode, String )
10-Fee Amount (float)
11-Changes
*/

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Syst/dumpfile_run.i}

DEF INPUT PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

def stream sout.
output stream sout to value(icFile).

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

DEFINE VARIABLE liKey AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcChanges AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liEntries AS INTEGER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcKey AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liOrderID AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMsseq AS INTEGER NO-UNDO. 

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
   
      FIND SingleFee NO-LOCK WHERE
           SingleFee.Brand = gcBrand AND
           SingleFee.FMItemID = liKey NO-ERROR.

      /* should not happen, but just in case */
      IF NOT AVAIL SingleFee THEN DO:
         PUT STREAM sout UNFORMATTED  "||||||" SKIP.
         NEXT.
      END.


      CASE SingleFee.HostTable:
         WHEN "MobSub" THEN liMsSeq = INT(SingleFee.KeyValue) NO-ERROR.
         WHEN "Order" THEN DO:
             liOrderID = INT(SingleFee.KeyValue) NO-ERROR.
             IF NOT ERROR-STATUS:ERROR THEN DO:
                FIND Order NO-LOCK WHERE
                     Order.Brand = gcBrand AND
                     Order.OrderId = liOrderID NO-ERROR.
                IF AVAIL Order THEN liMsSeq = Order.MsSeq.
             END.
         END.
         OTHERWISE liMsSeq = INT(SingleFee.KeyValue) NO-ERROR.
      END.

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
         liMsSeq "|"
         SingleFee.custnum "|"
         SingleFee.billperiod "|"
         SingleFee.BillCode "|"
         SingleFee.Amt "|"
         SingleFee.Invnum "|"
         lcChanges SKIP.
   END.
   ELSE PUT STREAM sout UNFORMATTED  "||||||" SKIP.
   
   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents WITH FRAME fColl.
   END.

END. 

OUTPUT STREAM sout CLOSE.
IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE.
