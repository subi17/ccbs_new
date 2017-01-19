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

{commali.i}
{dumpfile_run.i}
{create_eventlog.i}
{timestamp.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.


DEF STREAM sout.
OUTPUT STREAM sout TO VALUE(icFile).

FOR EACH Eventlog NO-LOCK WHERE
         Eventlog.Eventdate = TODAY - 1 AND
         Eventlog.tablename = "MsOwner" USE-INDEX EventDate:
   
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.Brand = "1" AND
              MsOwner.CLI = ENTRY(2,EventLog.Key,CHR(255)) AND
              MsOWner.TsEnd = DEC(ENTRY(3, EventLog.Key,CHR(255))) NO-ERROR.
   IF AVAIL MsOwner THEN DO:
      PUT STREAM sout UNFORMATTED
         MsOwner.CLI "|" 
         MsOwner.CustNum "|" 
         MsOwner.BillTarget "|"
         MsOwner.TsBegin "|" 
         MsOwner.TsEnd "|"
         MsOwner.MsSeq "|"
         MsOwner.IMSI "|"
         MsOwner.CliEvent "|"
         MsOwner.Clitype "|"
         MsOwner.Brand "|"
         MsOwner.Contract "|"
         MsOwner.RepCodes "|"
         MsOwner.InPortOper "|"
         MsOwner.OutPortOper "|"
         MsOwner.AgrCust "|"
         MsOwner.InvCust "|"
         MsOwner.PayType "|"
         MsOwner.MandateId "|"
         MsOwner.MandateDate "|"
         MsOwner.TariffBundle "|"
         MsOwner.FixedNumber SKIP.

      
      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         DISP oiEvents WITH FRAME fColl.
         PAUSE 0.
      END.
   END.
END. 
OUTPUT STREAM sout CLOSE.
IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE.
