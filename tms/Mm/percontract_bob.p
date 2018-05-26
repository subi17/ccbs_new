/*---------------------------------------------------------------
File /tms/Mm/percontract_bob.p 
YCO-469. Bob tool to assign periodical contract to a subscription
Solution Page:
https://arda.qvantel.com/pages/viewpage.action?pageId=30738422
05/2018
jotorres.
---------------------------------------------------------------*/

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".

{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/ftransdir.i}

/* Directories */
DEF VAR lcSpoolDirectory     AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/spool/". 
DEF VAR lcIncomingDirectory  AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/incoming/".  
DEF VAR lcOutgoingDirectory  AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/outgoing/".
DEF VAR lcProcessedDirectory AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/processed/".
DEF VAR lcLogsDirectory      AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/logs/".      

/* Input file fields */
DEF VAR lcMSISDN      AS CHAR NO-UNDO. /* MSISDN */
DEF VAR lcPerContract AS CHAR NO-UNDO. /* DayCampaign.DCEvent */

/* percontract_bob status */ 
DEF VAR lcTableName     AS CHAR NO-UNDO.
DEF VAR lcActionID      AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.

/* Streams */
DEF STREAM sFilesInDir.  /* Files in directory */ 
DEF STREAM sCurrentFile. /* Current processing file */
DEF STREAM sCurrentLog.  /* Log file for current processing file */
DEF STREAM sPerContrLog. /* Log file for percontract_bob.p executions */

DEF VAR lcFileName          AS CHAR NO-UNDO. /* Files in directory */
DEF VAR lcCurrentFile       AS CHAR NO-UNDO. /* Current processing file */
DEF VAR lcCurrentLog        AS CHAR NO-UNDO. /* log for current processing file */
DEF VAR lcLine              AS CHAR NO-UNDO. /* Read line of the current file. */
DEF VAR lcPerContractBobLog AS CHAR NO-UNDO. /* Log file for Periodical Contract Bob Tool executions */

/* Getting directories from CParams */
ASSIGN
   lcSpoolDirectory     = fCParamC("PerContractBobSpoolDir")
   lcIncomingDirectory  = fCParamC("PerContractBobIncomingDir")
   lcOutgoingDirectory  = fCParamC("PerContractBobOutgoingDir")
   lcProcessedDirectory = fCParamC("PerContractBobProcessedDir")
   lcLogsDirectory      = fCParamC("PerContractBobLogsDir") NO-ERROR.

/* Log file for PerContractBob executions */
lcPerContractBobLog = lcLogsDirectory + 
                      STRING(YEAR(TODAY), "9999") + 
                      STRING(MONTH(TODAY), "99" ) +
                      STRING(DAY(TODAY), "99") + 
                      "_percontract_bob.log".                     

OUTPUT STREAM sPercontrLog TO VALUE(lcPerContractBobLog) APPEND.
PUT STREAM sPercontrLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";percontract_bob_starts" SKIP.
  
/* Check if other percontract_bob is running */
ASSIGN 
   lcTableName = "PERCONTRACTBOB"
   lcActionID  = "file_reading"
   ldCurrentTimeTS = Func.Common:mMakeTS(). 
 
DO TRANSACTION:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand AND
              ActionLog.ActionID  EQ  lcActionID       AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      PUT STREAM sPercontrLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";another_percontract_bob_running" SKIP.
      PUT STREAM sPercontrLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";percontract_bob_ends" SKIP.
      OUTPUT STREAM sPercontrLog CLOSE.
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /* First execution stamp */
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = Syst.Var:gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      PUT STREAM sPercontrLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";percontract_bob_first_run" SKIP.
      PUT STREAM sPercontrLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";percontract_bob_ends" SKIP.      
      OUTPUT STREAM sPercontrLog CLOSE.
      QUIT. /* No reporting in first execution. */
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE Actionlog.
   END.
END.

/* Processing files in incoming directory */
INPUT STREAM sFilesInDir THROUGH VALUE("ls -1tr " + lcInComingDirectory).
REPEAT:
   IMPORT STREAM sFilesInDir UNFORMATTED lcFileName.
   /* Only process the correct files */
   IF (NOT (lcFileName BEGINS "PERCONTRACT")) THEN
     NEXT.     
   lcCurrentFile = lcInComingDirectory + lcFileName.
   lcCurrentLog = lcLogsDirectory + lcFileName + ".log". 
   IF SEARCH(lcCurrentFile) NE ? THEN DO:
      INPUT  STREAM sCurrentFile FROM VALUE(lcCurrentFile).
      OUTPUT STREAM sCurrentLog TO VALUE(lcCurrentLog).
   END.
   ELSE 
      NEXT.

   PUT STREAM sPercontrLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";" + lcCurrentFile + ";start_processing_file" SKIP.
   REPEAT:
      IMPORT STREAM sCurrentFile UNFORMATTED lcLine.
      lcLine = TRIM(lcLine).
      IF lcLine = "" THEN
         NEXT.
      IF NUM-ENTRIES(lcLine, ";") <> 2 THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";ERROR:incorrect_number_of_parameters" SKIP.            
      END.
      ASSIGN  
         lcMSISDN      = TRIM(ENTRY(1, lcLine, ";"))
         lcPerContract = TRIM(ENTRY(2, lcLine, ";")).
         
      /* Removing prefix (if needed) */
      IF LENGTH(lcMSISDN) EQ 11 THEN
         lcMSISDN = SUBSTRING(lcMSISDN, 3, 9).
    
      /* Check subscription */     
      FIND FIRST mobsub WHERE
                 mobsub.Brand EQ Syst.Var:gcBrand AND
                 mobsub.CLI   EQ lcMSISDN 
            USE-INDEX CLI NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mobsub THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";ERROR:MSISDN_not_found" SKIP.
         NEXT.
      END. 
      
      /* Check Periodical Contract */
      FIND FIRST DayCampaign NO-LOCK WHERE 
                 DayCampaign.Brand   EQ Syst.Var:gcBrand AND 
                 DayCampaign.DCEvent EQ lcPerContract
                 USE-INDEX DCEvent NO-ERROR.
      IF NOT AVAILABLE DayCampaign THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";ERROR:periodical_contract_not_found" SKIP.
         NEXT.         
      END.          
      
      
      /*---------------------*/
      /* CREATING PERMANENCY */
      /*---------------------*/
                           
      PUT STREAM sCurrentLog UNFORMATTED
         lcLine + ";OK" SKIP.
   
   END.

   INPUT STREAM sCurrentFile CLOSE.
   OUTPUT STREAM sCurrentLog CLOSE.
   fMove2TransDir(lcCurrentFile, "", lcProcessedDirectory).
   fMove2TransDir(lcCurrentLog, "", lcOutgoingDirectory).
   PUT STREAM sPercontrLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";" + lcCurrentFile + ";end_processing_file" SKIP.

END. 
INPUT STREAM sFilesInDir CLOSE.

DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.

PUT STREAM sPercontrLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";percontract_bob_end" SKIP.
PUT STREAM sPercontrLog UNFORMATTED "-------------------------------" SKIP.
OUTPUT STREAM sPercontrLog CLOSE.
