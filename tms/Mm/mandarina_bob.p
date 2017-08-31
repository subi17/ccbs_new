/* ----------------------------------------------------------------------
  MODULE .......: mandarina_bob.p
  TASK .........: BOB / Read input file; send commands to Procera to 
                  set or remove a redirection to LP. Create "log" file.
  APPLICATION ..: tms
  AUTHOR .......: jotorres & ilsavola
  CREATED ......: 08/2017
  Version ......: yoigo
---------------------------------------------------------------------- */

/*---------------------------------------------------------------------- 
https://kethor.qvantel.com/browse/MANDLP-8
2.1 Yoigo employee can upload a file to bob tool to set or 
    remove a redirection to a LP
---------------------------------------------------------------------- */

/* Parameters */
DEF INPUT PARAMETER pcProcessMode AS CHAR NO-UNDO. /* ["massive"|"priority"] */

/* Initial values */
DEF VAR lcMemoTitle AS CHAR INITIAL "Mandarina LP 2017". /* Mandarina LP 2017 campaign. */

/* includes */
{Syst/commpaa.i}
gcbrand = "1".

{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/lpfunctions.i}
{Func/ftransdir.i}

/* Directories */
DEF VAR lcBaseDirectory     AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/".
DEF VAR lcSpoolDirectory    AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/spool/". 
DEF VAR lcIncomingDirectory AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/incoming/". 
DEF VAR lcOutgoingDirectory AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/outgoing/". 
DEF VAR lcLogsDirectory     AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/logs/". 

/* Input file fields */
DEF VAR lcMSISDN AS CHAR NO-UNDO. /* MSISDN */
DEF VAR lcLP     AS CHAR NO-UNDO. /* ["Mandarina1"|"Mandarina2"] */
DEF VAR lcAction AS CHAR NO-UNDO. /* ["on"|"off"] */

/* mandarina_bob status */ 
DEF VAR lcTableName     AS CHAR NO-UNDO.
DEF VAR lcActionID      AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.
 
/* Streams */
DEF STREAM sFilesInDir.  /* Files in directory */ 
DEF STREAM sCurrentFile. /* Current processing file */
DEF STREAM sCurrentLog.  /* Log file for current processing file */
DEF STREAM sMandaLog.    /* Log file for mandarina_bob.p executions */

DEF VAR lcFileName    AS CHAR NO-UNDO. /* File in directory */
DEF VAR lcCurrentFile AS CHAR NO-UNDO. /* Current processing file */
DEF VAR lcLine        AS CHAR NO-UNDO. /* Read line of the current file. */

DEF VAR lcErr     AS CHAR    NO-UNDO.
DEF VAR llSuccess AS LOGICAL NO-UNDO.

/* Getting directories from CParams */
ASSIGN
   lcBaseDirectory     = fCParam("Mandarina", "MandarinaBaseDir")
   lcSpoolDirectory    = fCParam("Mandarina", "MandarinaSpoolDir")
   lcIncomingDirectory = fCParam("Mandarina", "MandarinaIncomingDir")
   lcOutgoingDirectory = fCParam("Mandarina", "MandarinaOutgoingDir")
   lcLogsDirectory     = fCParam("Mandarina", "MandarinaLogsDir").

/* Log file for mandarina executions */
OUTPUT STREAM sMandaLog TO VALUE(lcLogsDirectory + "mandarina_bob.log") APPEND.
PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_starting (" + pcProcessMode + ")------------------------------------------" SKIP.

/* Verify input parameter */
IF pcProcessMode <> "massive" AND pcProcessMode <> "priority" THEN DO:
   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";incorrect_input_parameter" SKIP.
   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_finishing" SKIP.
   OUTPUT STREAM sMandaLog CLOSE.
   QUIT.
END.
  
/* Check if other mandarina_bob is running */
ASSIGN 
   lcTableName = "MANDARINA"
   lcActionID  = "file_reading_" + pcProcessMode
   ldCurrentTimeTS = fMakeTS(). 
 
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";another_mandarina_bob_running" SKIP.
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_finishing" SKIP.
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
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_first_run" SKIP.
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_finishing" SKIP.      
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
   IF NOT (lcFileName BEGINS ("LP" + pcProcessMode)) THEN
     NEXT.
   lcCurrentFile = lcInComingDirectory + lcFileName.
   IF SEARCH(lcCurrentFile) NE ? THEN DO:
      INPUT STREAM sCurrentFile FROM VALUE(lcCurrentFile).
      OUTPUT STREAM sCurrentLog TO VALUE(lcLogsDirectory + lcFileName + ".log") APPEND. /* "append" to don't lose previous log if duplicated incommming file name */
   END.
   ELSE 
      NEXT. 

   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";" + lcCurrentFile + ";started" SKIP.
   REPEAT:
      IMPORT STREAM sCurrentFile UNFORMATTED lcLine.
      IF NUM-ENTRIES(lcLine, ";") <> 3 THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:bad_row" SKIP.
         NEXT.  
      END.
      ASSIGN
         lcMSISDN = ENTRY(1, lcLine, ";")
         lcLP     = ENTRY(2, lcLine, ";")
         lcAction = ENTRY(3, lcLine, ";"). 
     
      /* Checking values */
      IF LENGTH(lcMSISDN) <> 9
         OR
         (lcLP <> "Mandarina1" AND lcLP <> "Mandarina2")
         OR
         (lcAction <> "on" AND lcAction <> "off")
      THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:bad_field" SKIP.
         NEXT.
      END.

      /* Check subscription */     
      FIND FIRST mobsub WHERE
                 mobsub.Brand = gcBrand AND
                 mobsub.CLI   = lcMSISDN 
           USE-INDEX CLI NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mobsub THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:MSISDN_not_found" SKIP.
         NEXT.
      END.

      lcErr = "".
      IF lcAction = "on" THEN DO:
         /* No activate LP if ICC Changed */
         IF fICCDoneRecently(mobsub.MsSeq) THEN DO:
            PUT STREAM sCurrentLog UNFORMATTED
               lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ICC_DONE_RECENTLY" SKIP.
            NEXT.
         END.
         llSuccess = fMakeLPCommandRequest (INPUT mobsub.MsSeq,                     /*Subscription identifier*/
                                            INPUT (IF LcLP = "Mandarina1" 
                                                   THEN "REDIRECTION_OTAFAILED1" 
                                                   ELSE "REDIRECTION_OTAFAILED2"),  /*LP command to network*/ 
                                            INPUT mobsub.CustNum,                   /*Customer number for memo*/
                                            INPUT lcMemoTitle,                      /*Memo title. Empty -> no memo writing*/
                                            INPUT ("LP: " + lcLp + " " + lcAction), /*Memo text*/
                                            INPUT "LP_BOB",                         /*Creator tag for memo*/
                                            INPUT-OUTPUT lcErr).                    /*Request creation info*/
         IF (NOT llSuccess) THEN DO:
            PUT STREAM sCurrentLog UNFORMATTED
               lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:" + STRING(mobsub.MsSeq) + "_COMMAND_REQUEST_" + lcErr SKIP.
            NEXT.
         END.
      END.  
      ELSE DO: /* lcAction = "off" */
         llSuccess = fMakeLPCommandRequest (INPUT mobsub.MsSeq,                     /*Subscription identifier*/
                                            INPUT "REMOVE",                         /*LP command to network*/ 
                                            INPUT mobsub.CustNum,                   /*Customer number for memo*/
                                            INPUT lcMemoTitle,                      /*Memo title. Empty -> no memo writing*/
                                            INPUT ("LP: " + lcLp + " " + lcAction), /*Memo text*/
                                            INPUT "LP_BOB",                         /*Creator tag for memo*/
                                            INPUT-OUTPUT lcErr).                    /*Request creation info*/
         IF NOT llSuccess THEN DO:
            PUT STREAM sCurrentLog UNFORMATTED
               lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:" + STRING(mobsub.MsSeq) + "_COMMAND_REQUEST_" + lcErr SKIP.
            NEXT.
         END. 
      END.
      
      PUT STREAM sCurrentLog UNFORMATTED
        lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";OK" SKIP.
   END.
   INPUT STREAM sCurrentFile CLOSE.
   OUTPUT STREAM sCurrentLog CLOSE.
   fMove2TransDir(lcCurrentFile, "", lcOutgoingDirectory).
   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";" + lcCurrentFile + ";finished" SKIP.

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

PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_finishing" SKIP.
OUTPUT STREAM sMandaLog CLOSE.
