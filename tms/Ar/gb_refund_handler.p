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

DEF VAR lcResponseFile AS CHAR NO-UNDO.
DEF VAR lcRootDir AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcLogOutDir AS CHAR NO-UNDO.
DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR lcActionID AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR lcErrorLog AS CHAR NO-UNDO.
DEF VAR lcCurrentPeriod AS CHAR NO-UNDO. /*period when program is started*/

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.
DEF STREAM sResponse.




FUNCTION fGenerateFileName RETURNS CHAR
   (INPUT icInFN AS CHAR,
    INPUT icType AS CHAR):
   DEF VAR liFileNumberStart AS INT NO-UNDO.
   DEF VAR lcTemp AS CHAR NO-UNDO.
   liFileNumberStart = R-INDEX(icInFN, "_") + 1.
   lcTemp = SUBSTRING(icInFN, liFileNumberStart).

   RETURN REPLACE(icInFN, lcTemp, icType + ".csv").
END.

ASSIGN 
   lcCurrentPeriod = SUBSTRING(STRING(fMakeTS()),1,6)
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
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
   END.
   ELSE  ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}.

   RELEASE ActionLog.
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
      IF NOT lcFileName BEGINS "es_yoigo-" THEN NEXT.

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcErrorLog = lcGBSpoolDir + fGenerateFileName(lcFileName, "log").
   lcResponseFile = lcGBSpoolDir + fGenerateFileName(lcFileName, "result").

   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile).

   OUTPUT STREAM sLog TO VALUE(lcErrorLog) append.
   OUTPUT STREAM sResponse TO VALUE(lcResponseFile) append.

   RUN pReadFileData.

   OUTPUT STREAM sLog CLOSE.
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
   DEF VAR lcOutLine AS CHAR NO-UNDO.
   DEF VAR lcLogLine AS CHAR NO-UNDO.
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcCorrId AS CHAR NO-UNDO.
   DEF VAR lcTimeInfo AS CHAR NO-UNDO.
   DEF VAR lcAmount AS CHAR NO-UNDO.
   DEF VAR ldeAmount AS DECIMAL NO-UNDO. 
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcErrInfo AS CHAR NO-UNDO.
   DEF VAR lcOrigNumericFormat AS CHAR NO-UNDO.
   DEF VAR llgPayType AS LOGICAL.
   DEF VAR lcRefId AS CHAR NO-UNDO.

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
         lcMSISDN = entry(5,lcline,";")
         lcCorrId = entry(3,lcline,";")
         lcTimeInfo = entry(2,lcline,";")
         lcAmount = entry(8,lcline,";").
         IF entry(6,lcLine,";") EQ "POSTPAID" THEN
            llgPayType = FALSE.
         ELSE
            llgPayType = TRUE.
         lcRefId = entry(9,lcLine,";").   
      
      /* Check used numeric format at first row amount value and
         store original value for changing it back after handling 
         even thought it would not be nessessary because session 
         will end after this refund run. */
      IF lcOrigNumericFormat EQ "" THEN
         lcOrigNumericFormat = SESSION:NUMERIC-FORMAT.

      /* Check amount numeric format each time to ensure corrent
         handling of decimal value. (it was not sure which is
         reveived value numeric format in different cases prepaid /
         postpaid. If we can count on that numeric format remains 
         same for whole file, this could be also added under storing
         original value if. */
      IF INDEX(lcAmount, ".") > 0 THEN
         SESSION:NUMERIC-FORMAT = "AMERICAN".
      ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".        

      ldeAmount = DECIMAL(lcAmount).

      lcErr = fProcessGBEntry(lcMSISDN,
                              lcCorrId,
                              lcTimeInfo,
                              lcCurrentPeriod,
                              ldeAmount,
                              llgPayType,
                              lcRefId,
                              lcErrInfo).
      lcOutLine = lcLine + ";" + lcErr.
      lcLogLine = lcFilename + ";" + lcOutLine + ";" + lcErrInfo. 
      PUT STREAM sLog UNFORMATTED  lcLogLine SKIP.
      PUT STREAM sResponse UNFORMATTED  lcOutLine SKIP.
      

   END.
   SESSION:NUMERIC-FORMAT = lcOrigNumericFormat.

END PROCEDURE.
