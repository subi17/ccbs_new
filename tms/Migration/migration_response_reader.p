/* ----------------------------------------------------------------------
  MODULE .......: migration_response_reader.p
  TASK .........: Program reads Migration response files.
                  It also sets corresponding order status according to
                  returned status and notifies WEB MigrationTool about 
                  changed status.
                  Timing plan:
                  The program runs periodically some time after TMS has
                  sent migration request to NC.
                  This program has locking functionality that prevents
                  running multiple instances.
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 25.1.2017
  Version ......: yoigo
---------------------------------------------------------------------- */
{commpaa.i}
{timestamp.i}
{cparam2.i}
{tmsconst.i}
{migrationfunc.i}

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
DEF VAR lcInputFile AS CHAR No-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcMQMessage AS CHAR NO-UNDO.

DEF VAR lcErr AS CHAR NO-UNDO.

ASSIGN
   lcTableName = "MB_Migration"
   lcActionID = "migration_response_reader"
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
lcLogFile = lcLogDir + "MM_MIGRATION_RESPONSE_" + lcTimePart + ".log".

OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
   "Migration file reading starts " + fTS2HMS(fMakeTS()) SKIP.

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
lcErr = fInitMigrationMQ("response").
   IF lcErr NE "" THEN DO:
   PUT STREAM sLog UNFORMATTED
      "Migration file will be skipped, " + lcErr +
      fTS2HMS(fMakeTS()) SKIP.

END.
ELSE DO:
   /*MQ ready, it is possible to handle data*/
   INPUT STREAM sFile THROUGH VALUE("ls -ltr " + lcInDir + "/").
   REPEAT:
      IMPORT STREAM sFile UNFORMATTED lcFileName.
      lcInputFile = lcInDir + lcFileName.
    
      RUN pReadFile.
   END.
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
END.

PUT STREAM sLog UNFORMATTED
   "Migration file handling done " + fTS2HMS(fMakeTS()) SKIP.
OUTPUT STREAM sLog CLOSE.


/*File handling logic*/
/*Program separates succesful and unsuccesful rows to different lists.
  Data is provided to Migration Tool in a single function call accoring to
  temp tables that this procedure has collected*/
PROCEDURE pReadFile:
   DEF VAR lcLine AS CHAR NO-UNDO.
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcNCStatus AS CHAR NO-UNDO.
   DEF VAR lcNCComment AS CHAR NO-UNDO.
   DEF VAR lcFileName AS CHAR NO-UNDO.
   DEF VAR liLineNumber AS INT NO-UNDO.
   DEF VAR liOrderID AS INT NO-UNDO.

   PUT STREAM sLog UNFORMATTED
      "List collection starts " + fTS2HMS(fMakeTS()) SKIP.
   
   FILE_LINE:
   REPEAT TRANS:

      IMPORT STREAM sIn UNFORMATTED lcLine.
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
         lcMSISDN = STRING(ENTRY(1,lcline,";"))
         lcNCStatus = STRING(ENTRY(2,lcline,";"))
         lcNCComment = STRING(ENTRY(3,lcline,";")) 
         liOrderID = 0.

      FIND FIRST Order NO-LOCK /*EL?)*/ WHERE
                 Order.brand EQ gcBrand AND
                 Order.CLI EQ lcMSISDN AND
                 Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING}.
      IF AVAIL Order THEN liOrderID = Order.OrderID.

      /*Nodo data contains a number for Order that is
        already handled
        in incorrect status
        does not exist at all
        ->TMS writes own error notification list for this*/
      IF liOrderID EQ 0 THEN DO:
         lcRowStatus = "TMS Error".
      END.
      /*NC response is OK and TMS has order for the operation*/
      ELSE IF lcNCStatus EQ "0000 00000" THEN DO:
         /*Succesful case handling*/
         /*Set Order status*/
         /*Add entry to Migration Tool response list (ok)*/
         /*Send SMS*/
         /*It is important to check that we do not create double requests*/
         lcRowStatus = fCreateMigrationSub(liOrderID).
         
         IF lcRowStatus EQ "" THEN lcRowStatus = "OK".       

      END.
      ELSE DO:
         /*Error case handling*/
         /*Set Order Status*/
         /*Add entry to Migration tool response list (failed)*/
         lcRowStatus = "NC Failure".                                      
      END.
      /*Send message to WEB*/
      lcMQMessage = fGenerateNCResponseInfo(liOrderID,
                                            lcMSISDN,
                                            lcNCStatus,
                                            lcNCComment).
      IF lcMQMessage EQ "" THEN 
         lcRowStatus = lcRowStatus + ";Message creation failed". 
      ELSE DO:
        lcRowStatus = ";" + fWriteToMQ(lcMQMessage).
      END.

      PUT STREAM sLog UNFORMATTED
         lcLine + ";" + lcRowStatus SKIP.
                  
   END. /* Line handling loop END */

   PUT STREAM sLog UNFORMATTED
      "Read " + STRING(liLineNumber) + " lines. " 
      "List collection done " + fTS2HMS(fMakeTS()) SKIP.
END.

