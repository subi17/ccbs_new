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
{timestamp.i}

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
DEFINE VARIABLE liCount      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcModValues  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lc255        AS CHARACTER NO-UNDO. /* List separator */
DEFINE VARIABLE lcCustNum    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRow        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOrderId    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSalesMan   AS CHARACTER NO-UNDO.


DEF VAR ldaLastDumpDate AS DATE    NO-UNDO.
DEF VAR liLastDumpTime  AS INT     NO-UNDO.
DEF VAR lcLastDumpTime  AS CHAR    NO-UNDO.
DEF VAR ldEventTS       AS DECIMAL NO-UNDO.

DEFINE STREAM sFile.

FORM 
   oiEvents AT 2 LABEL "Collecting" FORMAT ">>>>>>>>9"
   WITH OVERLAY ROW 10 CENTERED TITLE " Customer's events " 
        FRAME fPassTime.

lc255 = CHR(255).

FUNCTION fCollectEvent RETURNS LOGICAL
   (INPUT icCustNum    AS CHAR,
    INPUT icEventTable AS CHAR):

   ASSIGN
      lcModFields = ""
      lcModValues = ""
      lcRow       = ""
      lcOrderId   = ""
      lcSalesMan  = ""
      ldEventTS   = fHMS2TS(EventLog.EventDate, EventLog.EventTime)

   IF EventLog.Memo <> "" THEN 
      ASSIGN lcRow      = ENTRY(1,EventLog.Memo,lc255) 
             lcOrderId  = ENTRY(3,EventLog.Memo,lc255) 
             lcSalesMan = ENTRY(4,EventLog.Memo,lc255). 

   IF icEventTable = "CustContact" THEN
      DO liCount = 1 TO NUM-ENTRIES(Eventlog.ModifiedFields):
         lcModFields = lcModFields + "CustContact_" + ENTRY(liCount,EventLog.ModifiedFields).
      END.
   ELSE lcModFields = Eventlog.ModifiedFields.

   DO liAmtMod = 1 TO NUM-ENTRIES(EventLog.DataValues,CHR(255)) BY 3:
      IF liAmtMod = 1 THEN
         ASSIGN lcModValues = ENTRY(liAmtMod + 1,EventLog.DataValues,CHR(255)).
      ELSE lcModValues = lcModValues + "," + ENTRY(liAmtMod + 1,EventLog.DataValues,CHR(255)).
   END.

   PUT STREAM sFile UNFORMATTED 
      Eventlog.UserCode               + lcDel +
      lcRow                           + lcDel +
      lcOrderId                       + lcDel +
      lcSalesMan                      + lcDel +
      STRING(ldEventTS)               + lcDel +
      icCustNum                       + lcDel +
      lcModFields                     + lcDel +
      lcModValues                        SKIP.

   oiEvents = oiEvents + 1.
   PAUSE 0.
   DISPLAY oiEvents WITH FRAME fPassTime. 
   RETURN TRUE.
END FUNCTION.    

/* Main block */
lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcDel = fInitDelimiter(DumpFile.DumpDelimiter).

   IF DumpFile.DecimalPoint = "." THEN
      SESSION:NUMERIC-FORMAT = "AMERICAN":U.
   ELSE
      SESSION:NUMERIC-FORMAT = "EUROPEAN":U.
END.
ELSE ASSIGN lcDel = CHR(9)
            SESSION:NUMERIC-FORMAT = "AMERICAN":U.

OUTPUT STREAM sFile TO VALUE(icFile).

fSplitTs(idLastDump, OUTPUT ldaLastDumpDate, OUTPUT liLastDumpTime).

lcLastDumpTime = STRING(liLastDumpTime,"hh:mm:ss").

IF icDumpMode = "Full" THEN DO:
   FOR EACH Eventlog NO-LOCK WHERE 
            EventLog.TableName = "Customer" OR
            EventLog.TableName = "CustContact":
      
      lcCustNum = "".
      
      CASE EventLog.TableName:
         WHEN "Customer" THEN DO:
            IF NOT (EventLog.Memo BEGINS "Order" OR
                    EventLog.Memo BEGINS "ACC"   OR
                    EventLog.Memo BEGINS "STC") THEN NEXT.
            lcCustNum = EventLog.Key. 
            IF lcCustNum NE "" THEN fCollectEvent(lcCustNum,"Customer").
         END.
         WHEN "CustContact" THEN DO:
            IF EventLog.Action = "Create" THEN NEXT.
            lcCustNum = ENTRY(2,EventLog.Key,CHR(255)). 
            IF lcCustNum NE "" THEN fCollectEvent(lcCustNum,"CustContact").
         END.
      END CASE.
   END.
END.
ELSE DO: /* Modified */
   FOR EACH Eventlog NO-LOCK WHERE 
            EventLog.EventDate >= ldaLastDumpDate:

      IF EventLog.EventDate EQ ldaLastDumpDate AND
         EventLog.EventTime < lcLastDumpTime THEN NEXT.

      lcCustNum = "".
      
      CASE EventLog.TableName:
         WHEN "Customer" THEN DO:
            IF NOT (EventLog.Memo BEGINS "Order" OR
                    EventLog.Memo BEGINS "ACC"   OR
                    EventLog.Memo BEGINS "STC") THEN NEXT.
            lcCustNum = EventLog.Key. 
            IF lcCustNum NE "" THEN fCollectEvent(lcCustNum,"Customer").
         END.
         WHEN "CustContact" THEN DO:
            IF EventLog.Action = "Create" THEN NEXT.
            lcCustNum = ENTRY(2,EventLog.Key,CHR(255)). 
            IF lcCustNum NE "" THEN fCollectEvent(lcCustNum,"CustContact").
         END.
      END CASE.
   END.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.
SESSION:NUMERIC-FORMAT = lcNumeric.
