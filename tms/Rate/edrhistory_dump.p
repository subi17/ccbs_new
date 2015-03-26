/* ----------------------------------------------------------------------
  MODULE .......: edrhistory_dump.p
  TASK .........: Dumps rerated call changes to Track. YOT-2127 
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 07.11.12
  Version ......: Yoigo
----------------------------------------------------------------------- */
{commali.i}
{cparam2.i}
{dumpfile_run.i}
{timestamp.i}
{tmsconst.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF BUFFER bEDRHistory FOR EDRHistory.

DEF VAR lcSep AS CHAR NO-UNDO INIT "|".
DEF STREAM sout.

OUTPUT STREAM sout TO VALUE(icFile).

FIND FIRST DumpFile WHERE
           DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile AND
             DumpFile.DumpDelimiter > "" THEN
   lcSep = fInitDelimiter(DumpFile.DumpDelimiter).

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

FOR EACH EDRHistory NO-LOCK WHERE
         EDRHistory.Brand = gcBrand AND
         EDRHistory.UpdateDate = TODAY - 1:

   /* only the latest rerate should be picked */
   FIND FIRST bEDRHistory NO-LOCK WHERE
              bEDRHistory.MsSeq = EDRHistory.MsSeq AND
              bEDRHistory.DateSt = EDRHistory.DateST AND
              bEDRHistory.TimeStart = EDRHistory.TimeStart AND
              bEDRHistory.DTLSeq = EDRHistory.DTLSeq AND
              bEDRHistory.UpdateDate >= EDRHistory.UpDateDate AND
              ROWID(bEDRHistory) NE ROWID(EDRHistory) NO-ERROR.
   IF AVAIL bEDRHistory AND
      (bEDRHistory.UpdateDate > EDRHistory.UpdateDate OR
       bEDRHistory.UpdateTime > EDRHistory.UpdateTime) THEN NEXT.

   FIND FIRST MobCDR NO-LOCK WHERE
              MobCDR.CLI = EDRHistory.CLI AND
              MobCDR.DateSt = EDRHistory.DateST AND
              MobCDR.TimeSt = EDRHistory.TimeStart AND
              MobCDR.DtlSeq = EDRHistory.DtlSeq NO-ERROR.
   
   IF NOT AVAIL MobCDR THEN DO:
      RUN pDumpRow((BUFFER EDRHistory:HANDLE),"BEFORE_ORPHAN"). 
      oiEvents = oiEvents + 1.
      NEXT.
   END.

   /* skip if the original call is not yet dumped */
   IF MobCDR.ReadDate >= EDRHistory.UpdateDate THEN NEXT.
   
   RUN pDumpRow((BUFFER EDRHistory:HANDLE),"BEFORE"). 
   RUN pDumpRow((BUFFER MobCDR:HANDLE),"AFTER"). 
      
   oiEvents = oiEvents + 2.
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents WITH FRAME fColl.
   END.
END.
         
PROCEDURE pDumpRow:

   DEF INPUT PARAM ihCDR AS HANDLE NO-UNDO.
   DEF INPUT PARAM icLineType AS CHAR NO-UNDO.

   PUT STREAM sout UNFORMATTED
      ihCDR::CLIType lcSep 
      STRING(ihCDR::DateSt,"99.99.9999") lcSep
      ihCDR::BillCode lcSep
      ihCDR::CCN lcSep
      ihCDR::MsSeq lcSep
      ihCDR::CLI lcSep
      (IF AVAIL MobCDR THEN STRING(MobCDR.DataIn + MobCDR.DataOut)
                       ELSE "") lcSep
      ROUND(ihCDR::amount,2) lcSep  
      (IF AVAIL MobCDR THEN STRING(MobCDR.BillDur) ELSE "") lcSep
      ihCDR::ErrorCode lcSep
      (IF AVAIL MobCDR THEN fTS2HMS(MobCDR.ReadinTS) ELSE "") lcSep
      icLineType
   SKIP.

END PROCEDURE. 
         
FINALLY:
   IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE.
   OUTPUT STREAM sout CLOSE.
END.
