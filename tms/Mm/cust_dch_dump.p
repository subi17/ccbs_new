/* ----------------------------------------------------------------------
  MODULE .......: cust_DCH_dump.p
  TASK .........: [DCH] TMS/DWH - Customer Data History Dump created daily with old customer data
  APPLICATION ..: TMS
  AUTHOR .......: Hugo Alberto Lujan Chavez hugo.lujan
  CREATED ......: 20.07.2015 YPR-1952
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}

DEFINE INPUT  PARAMETER icDumpID      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER icFile        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icDumpMode    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER idLastDump    AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER icEventSource AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icEventFields AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oiEvents      AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER olInterrupted AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lcNumeric    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcModFields  AS CHARACTER NO-UNDO.
DEFINE VARIABLE liAmtMod     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcModified   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lc255        AS CHARACTER NO-UNDO. /* List separator */
DEFINE VARIABLE lcCustNum    AS CHARACTER NO-UNDO.

DEFINE STREAM sFile.

FORM 
   oiEvents AT 2 LABEL "Collecting" FORMAT ">>>>>>>>9"
   WITH OVERLAY ROW 10 CENTERED TITLE " Customer's events " 
        FRAME fPassTime.

lc255 = CHR(255).

FUNCTION fCollectEvent RETURNS LOGICAL
   (INPUT icCustNum AS CHARACTER):

   PUT STREAM sFile UNFORMATTED 
      Eventlog.UserCode               + lcDel +
      Eventlog.Action                 + lcDel +
      ENTRY(2,EventLog.Key,lc255)     + lcDel +
      ENTRY(3,EventLog.Key,lc255)     + lcDel +
      STRING(Eventlog.TimingTS)       + lcDel +
      icCustNum                       + lcDel +
      Eventlog.ModifiedFields         + lcDel +
      EventLog.DataValues               SKIP.

   oiEvents = oiEvents + 1.
   PAUSE 0.
   DISPLAY oiEvents WITH FRAME fPassTime. 
   RETURN TRUE.
END FUNCTION.    

/* Main block */
lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN
DO:
   ASSIGN lcDel        = fInitDelimiter(DumpFile.DumpDelimiter)
          lcModFields  = DumpFile.EventLogFields.
   
   IF DumpFile.DecimalPoint = "." THEN
      SESSION:NUMERIC-FORMAT = "AMERICAN":U.
   ELSE
      SESSION:NUMERIC-FORMAT = "EUROPEAN":U.
END.
ELSE 
DO:
   ASSIGN 
      lcDel = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN":U.
END.
OUTPUT STREAM sFile TO VALUE(icFile).

IF icDumpMode = "Full" 
THEN FOR EACH Eventlog NO-LOCK WHERE 
              EventLog.EventDate >= TODAY - 90 AND
              EventLog.TableName  = "Customer"
              USE-INDEX EventDate:
      
      IF LOOKUP(EventLog.Action, "Order,ACC,STC") = 0 THEN NEXT.
      lcCustNum = ENTRY(1,EventLog.Key,CHR(255)). 
      IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
   END.
ELSE FOR EACH Eventlog NO-LOCK WHERE 
              EventLog.EventDate = TODAY - 1 AND
              EventLog.TableName = "Customer"
              USE-INDEX EventDate:

      IF LOOKUP(EventLog.Action, "Order,ACC,STC") = 0 THEN NEXT.
      lcCustNum = ENTRY(1,EventLog.Key,CHR(255)). 
      IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
      
   END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.
SESSION:NUMERIC-FORMAT = lcNumeric.