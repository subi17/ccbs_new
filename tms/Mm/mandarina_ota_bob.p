/* ----------------------------------------------------------------------
  MODULE .......: mandarina_ota_bob.p
  TASK .........: BOB / Read input file; Create MEMO for each MSISDN in input file 
                  Input files must begin "OTA_OK" or "OTA_KO".
  APPLICATION ..: tms
  AUTHOR .......: jotorres & ilsavola
  CREATED ......: 09/2017
  Version ......: yoigo
---------------------------------------------------------------------- */

/* includes */
{Syst/commpaa.i}
gcbrand = "1".

{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/ftransdir.i}

/* Directories */
DEF VAR lcSpoolDirectory     AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/spool/     */
DEF VAR lcIncomingDirectory  AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/incoming/  */  
DEF VAR lcOutgoingDirectory  AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/outgoing/  */
DEF VAR lcProcessedDirectory AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/processed/ */
DEF VAR lcLogsDirectory      AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/logs/      */

/* Input file fields */
DEF VAR lcMSISDN AS CHAR NO-UNDO. /* MSISDN */

/* mandarina_ota_bob status */ 
DEF VAR lcTableName     AS CHAR NO-UNDO.
DEF VAR lcActionID      AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.

/* Streams */
DEF STREAM sFilesInDir.  /* Files in directory */ 
DEF STREAM sCurrentFile. /* Current processing file */
DEF STREAM sCurrentLog.  /* Log file for current processing file */
DEF STREAM sMandaLog.    /* Log file for mandarina_ota_bob.p executions */

DEF VAR lcFileName      AS CHAR NO-UNDO. /* Files in directory */
DEF VAR lcCurrentFile   AS CHAR NO-UNDO. /* Current processing file */
DEF VAR lcCurrentLog    AS CHAR NO-UNDO. /* log for current processing file */
DEF VAR lcLine          AS CHAR NO-UNDO. /* Read line of the current file. */
DEF VAR lcMandOtaBobLog AS CHAR NO-UNDO. /* Log file for Mandarina OTA Bob Tool executions */

/* Memo info */
DEF VAR lcMemoTitle AS CHAR NO-UNDO.
DEF VAR lcMemoText  AS CHAR NO-UNDO.

/* Getting directories from CParams */
ASSIGN
   lcSpoolDirectory     = fCParamC("MandarinaSpoolDir")
   lcIncomingDirectory  = fCParamC("MandarinaIncomingDir")
   lcOutgoingDirectory  = fCParamC("MandarinaOutgoingDir")
   lcProcessedDirectory = fCParamC("MandarinaProcessedDir")
   lcLogsDirectory      = fCParamC("MandarinaLogsDir") NO-ERROR.

/* Log file for mandarina executions */
lcMandOtaBobLog = lcLogsDirectory + 
                  STRING(YEAR(TODAY), "9999") + 
                  STRING(MONTH(TODAY), "99" ) +
                  STRING(DAY(TODAY), "99") + 
                  "_mandarina_OTA_bob.log".                     

OUTPUT STREAM sMandaLog TO VALUE(lcMandOtaBobLog) APPEND.
PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_OTA_bob_starts" SKIP.
  
/* Check if other mandarina_OTA_bob is running */
ASSIGN 
   lcTableName = "MANDARINA"
   lcActionID  = "file_reading_OTA"
   ldCurrentTimeTS = Func.Common:mMakeTS(). 
 
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";another_mandarina_OTA_bob_running" SKIP.
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_OTA_bob_finishing" SKIP.
      OUTPUT STREAM sMandaLog CLOSE.
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
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_OTA_bob_first_run" SKIP.
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_OTA_bob_finishes" SKIP.      
      OUTPUT STREAM sMandaLog CLOSE.
      QUIT. /*No reporting in first time.*/
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE Actionlog.
   END.
END.

/* Processing files in incoming directory */
INPUT STREAM sFilesInDir THROUGH VALUE("ls -1tr " + lcInComingDirectory).
REPEAT:
   IMPORT STREAM sFilesInDir UNFORMATTED lcFileName.
   /* Only process the correct files */
   IF (NOT (lcFileName BEGINS "OTA_OK")) AND (NOT (lcFileName BEGINS "OTA_KO")) THEN
     NEXT.
   lcCurrentFile = lcInComingDirectory + lcFileName.
   lcCurrentLog = lcLogsDirectory + lcFileName + ".log". 
   IF SEARCH(lcCurrentFile) NE ? THEN DO:
      INPUT  STREAM sCurrentFile FROM VALUE(lcCurrentFile).
      OUTPUT STREAM sCurrentLog TO VALUE(lcCurrentLog).
   END.
   ELSE 
      NEXT.

ASSIGN
  lcMemoTitle = (IF lcFileName BEGINS "OTA_OK" 
                 THEN "OTA OK" 
                 ELSE "OTA KO")
  lcMemoText = (IF lcFileName BEGINS "OTA_OK" 
                THEN "Migración red - OTA OK"
                ELSE "Migración red - OTA KO").

   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";" + lcCurrentFile + ";start_processing_file" SKIP.
   REPEAT:
      IMPORT STREAM sCurrentFile UNFORMATTED lcLine.
      lcMSISDN = TRIM(ENTRY(1, lcLine)).

      /* Removing prefix */
      IF LENGTH(lcMSISDN) EQ 11 THEN
         lcMSISDN = SUBSTRING(lcMSISDN, 3, 9).
    
      /* Check subscription */     
      FIND FIRST mobsub WHERE
                 mobsub.Brand EQ gcBrand AND
                 mobsub.CLI   EQ lcMSISDN 
            USE-INDEX CLI NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mobsub THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:MSISDN_not_found" SKIP.
         NEXT.
      END. 
   
      Func.Common:mWriteMemoWithType("Mobsub",
                        mobsub.MsSeq,
                        mobsub.CustNum,
                        lcMemoTitle,
                        lcMemoText,
                        "Service",      /* memo type */
                        "Sistema").     /* creator */
      PUT STREAM sCurrentLog UNFORMATTED
         lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";OK" SKIP.
   
   END.

   INPUT STREAM sCurrentFile CLOSE.
   OUTPUT STREAM sCurrentLog CLOSE.
   fMove2TransDir(lcCurrentFile, "", lcProcessedDirectory).
   fMove2TransDir(lcCurrentLog, "", lcOutgoingDirectory).
   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";" + lcCurrentFile + ";finish_processing_file" SKIP.

END. 
INPUT STREAM sFilesInDir CLOSE.

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

PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_OTA_bob_finishes" SKIP.
PUT STREAM sMandaLog UNFORMATTED "-------------------------------" SKIP.
OUTPUT STREAM sMandaLog CLOSE.
