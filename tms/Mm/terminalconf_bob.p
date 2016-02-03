/* ----------------------------------------------------------------------
  module .......: Mm/terminalconf_bob.p
  task .........: Create/Update/Delete TerminalConf from Backdoor tool
  application ..: tms
  author .......: vikas
  created ......: 23.06.14
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/lib/eventlog.i}
{Syst/eventval.i}

/* files and dirs */
DEF VAR lcLine           AS CHAR NO-UNDO.
DEF VAR lcLogFile        AS CHAR NO-UNDO. 
DEF VAR lcFileName       AS CHAR NO-UNDO. 
DEF VAR lcIncDir         AS CHAR NO-UNDO. 
DEF VAR lcInputFile      AS CHAR NO-UNDO. 
DEF VAR lcProcDir        AS CHAR NO-UNDO. 
DEF VAR lcProcessedFile  AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir       AS CHAR NO-UNDO. 
DEF VAR lcReportFileOut  AS CHAR NO-UNDO. 
DEF VAR lcOutDir         AS CHAR NO-UNDO. 
DEF VAR lcToday          AS CHAR NO-UNDO.
DEF VAR lcTime           AS CHAR NO-UNDO.
DEF VAR lcSep            AS CHAR NO-UNDO INIT ";".
DEF VAR liRead           AS INT NO-UNDO. 
DEF VAR liErrors         AS INT NO-UNDO. 

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhTerminalConf AS HANDLE NO-UNDO.
   lhTerminalConf = BUFFER TerminalConf:HANDLE.
   RUN StarEventInitialize(lhTerminalConf).

END.

ASSIGN
   lcIncDir    = fCParam("TerminalBackTool","IncDir") 
   lcProcDir   = fCParam("TerminalBackTool","IncProcDir")
   lcSpoolDir  = fCParam("TerminalBackTool","OutSpoolDir")
   lcOutDir    = fCParam("TerminalBackTool","OutDir")
   lcToday     = STRING(YEAR(TODAY),"9999") + 
                 STRING(MONTH(TODAY),"99")  +
                 STRING(DAY(TODAY),"99")
   lcTime      = REPLACE(STRING(TIME,"hh:mm:ss"),":","").

FUNCTION fLogLine RETURNS LOG(icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine lcSep 
      icMessage SKIP.

END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName. 
   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
   
      /* To prevent duplicate file handling */
      IF CAN-FIND (FIRST ActionLog NO-LOCK WHERE
                         ActionLog.Brand = gcBrand AND
                         ActionLog.TableName = "Cron" AND
                         ActionLog.KeyValue = lcFileName AND
                         ActionLog.ActionID = "TerminalConfBOB" AND
                         ActionLog.ActionStatus = 0) THEN NEXT.

      DO TRANS:
         CREATE ActionLog.
         ASSIGN 
            ActionLog.Brand        = gcBrand   
            ActionLog.TableName    = "Cron"  
            ActionLog.KeyValue     = lcFileName
            ActionLog.ActionID     = "TerminalConfBOB"
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                     MONTH(TODAY)
            ActionLog.ActionStatus = 0
            ActionLog.ActionTS     = fMakeTS().
      END.

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcLogFile = lcSpoolDir + "terminalconf_bob_" +
               lcToday + "_" + lcTime + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile).
   fBatchLog("START", lcLogFile).

   LINE_LOOP:
   REPEAT:
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine = "" OR lcLine = ? THEN NEXT LINE_LOOP.

      RUN pHandleRecord(INPUT lcLine).
   
      IF RETURN-VALUE BEGINS "ERROR" THEN liErrors = liErrors + 1.
      liRead = liRead + 1.
      
      fLogLine(RETURN-VALUE).

   END. /* REPEAT: LINE_LOOP: */
  
   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.
   
   DO TRANS:
      ASSIGN 
         ActionLog.ActionDec    = liRead
         ActionLog.ActionChar   = "Read: " + STRING(liRead) + 
                                  " Errors: " + STRING(liErrors) + 
                                  " Succesful: " + STRING(liRead - liErrors) + 
                                  CHR(10) + "Finished: " + fTS2HMS(fMakeTS())
         ActionLog.ActionStatus = 3.
   END.
   
END. /* REPEAT: */

INPUT STREAM sFile CLOSE.


PROCEDURE pHandleRecord: 

   DEF INPUT PARAMETER pcLine AS CHAR NO-UNDO. 

   /* local variables */
   DEF VAR lcTerminalCode     AS CHAR NO-UNDO.
   DEF VAR ldeDextraPrice     AS DEC  NO-UNDO.
   DEF VAR lcInputAction      AS CHAR NO-UNDO.
   DEF VAR ldaValidFrom       AS DATE NO-UNDO.
   DEF VAR ldaValidTo         AS DATE NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.

   IF NUM-ENTRIES(pcLine,lcSep) <> 5 THEN
      RETURN "ERROR:Wrong file format".

   ASSIGN
      lcTerminalCode = ENTRY(1,pcLine,lcSep)
      ldeDextraPrice = DECIMAL(ENTRY(2,pcLine,lcSep))
      lcInputAction  = ENTRY(3,pcLine,lcSep)
      ldaValidFrom   = DATE(ENTRY(4,pcLine,lcSep))
      ldaValidTo     = DATE(ENTRY(5,pcLine,lcSep)) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN
      RETURN "ERROR:Invalid value".

   IF lcTerminalCode = "" OR lcTerminalCode = ? THEN
      RETURN "ERROR:Terminal Code is blank or unknown".

   CASE lcInputAction:
      WHEN "CREATE" THEN DO:
         IF ldaValidFrom = ? THEN
            RETURN "ERROR:Invalid ValidFrom date".
         IF ldaValidTo = ? OR ldaValidTo < TODAY OR ldaValidTo < ldaValidFrom THEN
            RETURN "ERROR:Invalid ValidTo date".
         IF ldeDextraPrice = 0 OR ldeDextraPrice = ? THEN
            RETURN "ERROR:Invalid Dextra Price".
         FIND FIRST TerminalConf WHERE
                    TerminalConf.TerminalCode = lcTerminalCode AND
                    TerminalConf.ValidTo >= ldaValidFrom NO-LOCK NO-ERROR.
         IF AVAIL TerminalConf THEN
            RETURN "ERROR:Active TerminalConf record exists".

         CREATE TerminalConf.
         ASSIGN TerminalConf.TerminalCode = lcTerminalCode
                TerminalConf.DextraPrice  = ldeDextraPrice
                TerminalConf.ValidFrom    = ldaValidFrom
                TerminalConf.ValidTo      = ldaValidTo.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTerminalConf).
         FIND CURRENT TerminalConf NO-LOCK NO-ERROR.
      END.
      WHEN "UPDATE" THEN DO:
         IF ldeDextraPrice > 0 OR ldaValidFrom <> ? THEN DO:
            IF ldaValidTo <> ? THEN
               RETURN "ERROR:ValidTo date can't be updated along with either dextra price or validfrom".
         END.

         IF ldaValidFrom <> ? AND ldaValidFrom <= TODAY THEN
            RETURN "ERROR:ValidFrom can't be past or current date".

         IF ldeDextraPrice > 0 OR ldaValidFrom <> ? THEN DO:
            FIND FIRST TerminalConf WHERE
                       TerminalConf.TerminalCode = lcTerminalCode AND
                       TerminalConf.ValidFrom > TODAY
                 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAIL TerminalConf THEN
               RETURN "ERROR:Future TerminalConf record not found or nothing to update".
            ELSE DO:
               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTerminalConf).
               IF ldeDextraPrice > 0 THEN
                  TerminalConf.DextraPrice = ldeDextraPrice.
               IF ldaValidFrom > TODAY THEN
                  TerminalConf.ValidFrom = ldaValidFrom.
               IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTerminalConf).
            END.
            FIND CURRENT TerminalConf NO-LOCK NO-ERROR.
         END.
         ELSE IF ldaValidTo <> ? THEN DO:
            IF ldaValidTo < TODAY THEN
               RETURN "ERROR:ValidTo can not be past date".
            FIND FIRST TerminalConf WHERE
                       TerminalConf.TerminalCode = lcTerminalCode AND
                       TerminalConf.ValidTo >= TODAY
                 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAIL TerminalConf THEN
               RETURN "ERROR:Active TerminalConf record not found or nothing to update".
            ELSE DO:
               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTerminalConf).
               TerminalConf.ValidTo = ldaValidTo.
               IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTerminalConf).
            END.
            FIND CURRENT TerminalConf NO-LOCK NO-ERROR.
         END.
         ELSE RETURN "ERROR:Nothing to update".
      END.
      WHEN "DELETE" THEN DO:
         FIND FIRST TerminalConf WHERE
                    TerminalConf.TerminalCode = lcTerminalCode AND
                    TerminalConf.ValidFrom > TODAY
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF NOT AVAIL TerminalConf THEN
            RETURN "ERROR:TerminalConf record not found or nothing to delete".

         IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTerminalConf).
         DELETE TerminalConf.
      END.
      OTHERWISE RETURN "ERROR:Invalid action value".
   END.

   RETURN "OK".

END PROCEDURE.

