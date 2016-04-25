/* ----------------------------------------------------------------------
  MODULE .......: gb_refund_handler.p
  TASK .........: Handle Google Billing refund file
  APPLICATION ..: tms
  AUTHOR .......: ilsavola & kariaika
  CREATED ......: 19.4.2016
  Version ......: yoigo
---------------------------------------------------------------------- */
{commpaa.i}
gcbrand = "1".
{tmsconst.i}
{q25functions.i}
{ftransdir.i}
{eventlog.i}
{gbilling.i}

DEF VAR lcProcessedFile AS CHAR NO-UNDO.
DEF VAR lcRootDir AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcLogOutDir AS CHAR NO-UNDO.
DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR lcActionID AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR lcErrorLog AS CHAR NO-UNDO.

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

ASSIGN 
   lcTableName = {&GB_ACTION_GROUP_NAME}
   lcActionID = {&GB_REFUND_HANDLER}
   ldCurrentTimeTS = fMakeTS().

fInitGBParameters().

DO TRANS:

   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      RETURN. /*No reporting in first time.*/
   END.
END.

/*File handling logic starts*/
/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcGBInDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile = lcGBInDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:

      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.

      /*Accept only activation files*/
      IF NOT lcFileName BEGINS "XXXX" THEN NEXT.

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcErrorLog = lcGBSpoolDir + lcFileName + ".LOG".

   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile).

   OUTPUT STREAM sLog TO VALUE(lcErrorLog) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.

   RUN pReadFileData.

   OUTPUT STREAM sLog CLOSE.
   fMove2TransDir(lcErrorLog, "", lcGBLogDir).
   fMove2TransDir(lcResponseFile, "", lcGBOutDir).
   fMove2TransDir(lcInputFile, "", lcGBProcessedDir).
   IF SESSION:BATCH AND lcInputFile NE "" THEN
      fBatchLog("FINISH", lcInputFile).
END.

DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.

INPUT STREAM sin CLOSE.


PROCEDURE pReadFileData:

   DEF VAR liLineNum AS INT.
   DEF VAR lcLine AS CHAR NO-UNDO.
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcCorrId AS CHAR NO-UNDO.
   DEF VAR ldtDateTime AS DATETIME NO-UNDO.
   DEF VAR ldeAmount AS DECIMAL NO-UNDO. 

   FILE_LINE:
   REPEAT TRANS:

      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.
      liLineNum = liLineNum + 1.

      IF NOT SESSION:BATCH AND liLineNum MOD 10 = 0 THEN DO:
         disp "Reading data. " lcFilename liLineNum with frame a.
         pause 0.
      END.

      assign
         lcMSISDN = entry(1,lcline,";")
         lcCorrId = entry(2,lcline,";")
         ldtDateTime = DATETIME(entry(3,lcline,";"))
         ldeAmount = DECIMAL(entry(2,lcline,";")).
      


      lcErr = fProcessGBEntry(lcMSISDN,
                              lcCorrId,
                              ldtDateTime,
                              ldeAmount).
      lcOutLine = lcLine + ";" + lcErr.
      

   END.

END PROCEDURE.
