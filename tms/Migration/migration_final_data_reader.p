/* ----------------------------------------------------------------------
  MODULE .......: migration_final_data_reader.p
  TASK .........: Program reads Migration Finalization Data Files.
                  It sets services, barrings, bundles, upsells and sets
                  given data amount to network.
                  Program procides information about success in JSON format 
                  to Migration Tool (WEB).
                  Format for input file:
|MM internal ID|MSISDN|BARRINGS|SERVICES|BONO|UPSELL|USED_DATA_AMOUNT|
|MM internal ID|MSISDN|barring_1;barring_2|service_1;service_2;service_3||upsell_1|34000| 
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 30.1.2017
  Version ......: yoigo
---------------------------------------------------------------------- */
{commpaa.i}
{timestamp.i}
{cparam2.i}
{tmsconst.i}
{barrfunc.i}
DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.
DEF VAR lcSpoolDir AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR lcActionID AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR lcInDir AS CHAR NO-UNDO.
DEF VAR ldaReadDate AS DATETIME.
DEF VAR lcTimePart AS CHAR. /*For log file name*/
DEF VAR lcLogFile AS CHAR NO-UNDO.
DEF VAR lcRowStatus AS CHAR NO-UNDO.

ASSIGN
   lcTableName = "MB_Migration"
   lcActionID = "migration_final_data__reader"
   ldCurrentTimeTS = fMakeTS()
   lcLogDir = fCParam("MB_Migration", "MigrationLogDir")
   lcInDir = fCParam("MB_Migration", "MigrationInDir").

IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".
IF lcInDir EQ "" OR lcInDir EQ ? THEN lcInDir = "/tmp/".

/*Set output and log files*/
ldaReadDate = TODAY.
lcTimePart = STRING(YEAR(ldaReadDate)) +
             STRING(MONTH(ldaReadDate),"99") +
             STRING(DAY(ldaReadDate),"99") +
             REPLACE(STRING(TIME,"HH:MM:SS"),":","").
lcLogFile = lcLogDir + "MM_MIGRATION_FINAL_DATA__" + lcTimePart + ".log".

OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
   "Migration filefinal data reading starts " + fTS2HMS(fMakeTS()) SKIP.

/*Ensure that multiple instances of the program are not running*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      PUT STREAM sLog UNFORMATTED
         "File processing alrady ongoing " + fTS2HMS(fMakeTS()) SKIP.
      OUTPUT STREAM sLog CLOSE.
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
      QUIT. /*No reporting in first time.*/
   END.
END.

/*Execution part*/
RUN pHandleInputFile. 
RUN pSendResults.

/*Release ActionLog lock*/
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

PUT STREAM sLog UNFORMATTED
   "Migration finalization file handling done " + fTS2HMS(fMakeTS()) SKIP.
OUTPUT STREAM sLog CLOSE.

/*Functions and procedures*/

/*Function generates a barring command from separated list format:
  barring1,barring2,barring3 ->
  barring1=1,barring2=1,barring3=1
  The program clears list so that already active barrings are not set to
  list to be activated again. (Idea: reduce errors by incoorect input)
  List handling creates log for problem solving purposes.
  The program does not make validations for barring command compability etc.
  The checks are done when calling actual barring functions.  
  */
FUNCTION fMMInputToBarrCmd RETURNS CHAR
   (iiMsSeq AS INT,
    icInput AS CHAR,
    icInputSeparator AS CHAR):
   DEF VAR i AS INT NO-UNDO.
   DEF VAR lcCommand AS CHAR NO-UNDO.

   PUT STREAM sLog UNFORMATTED
      "Generating Barring Command, input: "
      STRING(iiMsSeq) + " " +
      icInput "." SKIP.

   IF icInput EQ "" THEN DO:
      PUT STREAM sLog UNFORMATTED
         "Empty barring list " SKIP.
      RETURN icInput.
   END.
   DO i = 1 TO NUM-ENTRIES(icInput,icInputSeparator):
      IF fGetBarringStatus(ENTRY(i,icInput,icInputSeparator), iiMsSeq) EQ
         {&BARR_STATUS_ACTIVE} THEN DO:
         PUT STREAM sLog UNFORMATTED
            "Already active, not added to command: "
            ENTRY(i,icInput,icInputSeparator) " " SKIP.
      END.
      ELSE DO:
         lcCommand = lcCommand + ENTRY(i,icInput,icInputSeparator) + "=1".
         IF i NE NUM-ENTRIES(icInput,icInputSeparator) THEN
            lcCommand = lcCommand + ",".
      END.
   END.
   PUT STREAM sLog UNFORMATTED
      "Generated Barring Command: "
       STRING(iiMsSeq) + " " +
       lcCommand "." SKIP.

   RETURN lcCommand.
END.

/*Function validates barring request.
Codes:
   EMPTY                                 :  The command is OK
   Validation Error + description        :  Errorneous request
   Ongoing network command               :  Barring command was ongoing  
   ERROR: MobSub not found.              :  MobSub not found
   ERROR: Mobile line provisioning is not complete : Mobsub, wrong status
   */
FUNCTION fValidateMMBarringCommand RETURNS CHAR
   (iiMsSeq AS INT,
    icInput AS CHAR): /*In processed format b1=1,B2=1...*/
    
   DEF VAR i AS INT NO-UNDO.
   DEF VAR lcValStatus AS CHAR NO-UNDO.

   FIND MobSub NO-LOCK WHERE Mobsub.MsSeq = iiMsSeq NO-ERROR.

   IF NOT AVAIL MobSub THEN RETURN "ERROR: MobSub not found.".

   IF MobSub.MsStatus EQ {&MSSTATUS_FIXED_PROV_ONG} /*16*/ THEN
      RETURN "ERROR: Mobile line provisioning is not complete".

   IF icInput EQ "" THEN RETURN "".
   lcValStatus = fValidateBarrRequest(iiMsSeq, icInput).
   IF lcValStatus EQ "ONC" THEN RETURN "Ongoing network command".
   ELSE IF lcValStatus NE "" THEN RETURN "Validation Error: " + lcValStatus.
   
   RETURN lcValStatus.

END.



/*File reading and data processing for each filed.  
  MSISDN (Single value)
  Barrings (list):
     List of barrings will be set in a command.
     Barring list handling filters out already active barrings and
     makes also other validations.
  Services (list)
     Services are set one by one.
  Bono (single value)     
  Upsell (Single value)
  Data (Single value)
  */
PROCEDURE pHandleInputFile:
   DEF VAR lcLine AS CHAR NO-UNDO.
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcFileName AS CHAR NO-UNDO.
   DEF VAR liLineNumber AS INT NO-UNDO.
   DEF VAR lcMMOrderID AS CHAR NO-UNDO. 
   DEF VAR lcBarringList AS CHAR NO-UNDO.
   DEF VAR lcServiceList AS CHAR NO-UNDO.
   DEF VAR lcBono AS CHAR NO-UNDO.
   DEF VAR lcUpsell AS CHAR NO-UNDO.
   DEF VAR liDataAmount AS INT NO-UNDO.
   PUT STREAM sLog UNFORMATTED
      "List collection starts " + fTS2HMS(fMakeTS()) SKIP.
   FILE_LINE:
   REPEAT TRANS:

      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.
      liLineNumber = liLineNumber + 1.

      IF NOT SESSION:BATCH AND liLineNumber MOD 10 = 0 THEN DO:
         disp "Reading data: " lcFilename liLineNumber with frame a.
         pause 0.
      END.
      IF NUM-ENTRIES (lcLine) NE 3 THEN DO:
         PUT STREAM sLog UNFORMATTED
         lcLine + ";" + "ERROR:Incorrect input format"  SKIP.
         NEXT.

      END.
      assign
         lcMMOrderID = STRING(ENTRY(1,lcline,";"))
         lcMSISDN = STRING(ENTRY(2,lcline,";"))
         lcBarringList = STRING(ENTRY(3,lcline,";"))
         lcServiceList = STRING(ENTRY(4,lcline,";"))
         lcBono = STRING(ENTRY(5,lcline,";"))
         lcUpsell = STRING(ENTRY(6,lcline,";"))
         liDataAmount = INT(ENTRY(7,lcline,";")).

   END. /* Line handling END */

   PUT STREAM sLog UNFORMATTED
      "Read " + STRING(liLineNumber) + " lines. " 
      "Collection done " + fTS2HMS(fMakeTS()) SKIP.
END.

/*Procedure sends results to Migration Tool and writes logs*/
PROCEDURE pSendResults:

   PUT STREAM sLog UNFORMATTED
      "Result reporting starts " + fTS2HMS(fMakeTS()) SKIP.

   PUT STREAM sLog UNFORMATTED
      "Result reporting done " + fTS2HMS(fMakeTS()) SKIP.
END.
