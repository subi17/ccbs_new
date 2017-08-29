/* ----------------------------------------------------------------------
  MODULE .......: mandarina_bob.p
  TASK .........: BOB / Read input file; send commands to Procera to 
                  set or remove a redirection to LP. Create "log" file.
  APPLICATION ..: tms
  AUTHOR .......: jotorres
  CREATED ......: 08/2017
  Version ......: yoigo
---------------------------------------------------------------------- */

/*---------------------------------------------------------------------- 
https://kethor.qvantel.com/browse/MANDLP-8
2.1 Yoigo employee can upload a file to bob tool to set or remove 
     a redirection to a LP
---------------------------------------------------------------------- */

/* includes */
{Syst/commpaa.i}
gcbrand = "1".
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/cparam2.i}

/* Directories */
DEF VAR lcBaseDirectory     AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/".
DEF VAR lcSpoolDirectory    AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/spool/". 
DEF VAR lcIncomingDirectory AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/incoming/". 
DEF VAR lcOutgoingDirectory AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/outgoing/". 
DEF VAR lcLogsDirectory     AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/logs/". 

/* Input file fields */
DEF VAR lcMSISDN AS CHAR NO-UNDO. /* MSISDN */
DEF VAR lcLP     AS CHAR NO-UNDO. /* [Mandarina1|Mandarina2] */
DEF VAR lcAction AS CHAR NO-UNDO. /* [ON|OFF] */

/* Mandarina is processing. */
DEF VAR lcTableName     AS CHAR NO-UNDO.
DEF VAR lcActionID      AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.
 
/* Streams */
DEF STREAM sFile.
DEF STREAM sInputFile.
DEF STREAM sLog.

DEF VAR lcFileName  AS CHAR NO-UNDO. /* Current file */
DEF VAR lcInputFile AS CHAR NO-UNDO. 
DEF VAR lcLine      AS CHAR NO-UNDO. /* Read line of the current file. */
DEF VAR lcCommand   AS CHAR NO-UNDO. /* Command to send to Procera */
 
/* Getting directories from CParams */
ASSIGN
   lcBaseDirectory     = fCParam("Mandarina", "MandarinaBaseDir")
   lcSpoolDirectory    = fCParam("Mandarina", "MandarinaSpoolDir")
   lcIncomingDirectory = fCParam("Mandarina", "MandarinaIncomingDir")
   lcOutgoingDirectory = fCParam("Mandarina", "MandarinaOutgoingDir")
   lcLogsDirectory     = fCParam("Mandarina", "MandarinaLogsDir").
 
/* Check if process ir running */
ASSIGN 
   lcTableName = "MANDARINA"
   lcActionID  = "Mandarina file reading"
   ldCurrentTimeTS = fMakeTS(). 
 
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
      QUIT. /*No reporting in first time.*/
   END.
   ELSE /*IF (liHRLPTestLevel EQ {&Q25_HRLP_NO_TEST}) THEN */ DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE Actionlog.
   END.
END.

INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcInComingDirectory).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
   lcInputFile = lcInComingDirectory + lcFileName.
   IF SEARCH(lcInputFile) NE ? THEN DO:
      INPUT STREAM sInputFile FROM VALUE(lcInputFile).
      OUTPUT STREAM sLog TO VALUE(lcLogsDirectory + "LP_" + lcFileName + ".log").
   END.
   ELSE 
      NEXT. 

   REPEAT:
     IMPORT STREAM sInputFile UNFORMATTED lcLine.
     IF NUM-ENTRIES(lcLine, ";") <> 3 THEN
        NEXT.
      ASSIGN
         lcMSISDN = ENTRY(1, lcLine, ";")
         lcLP     = ENTRY(2, lcLine, ";")
         lcAction = ENTRY(3, lcLine, ";"). 
      IF lcAction = "ON" THEN DO:
         /* send Procera */
         lcCommand = "ET:XFPL:TRANSID,431012496:MSISDN," + lcMSISDN + ":" + 
                     "LP," + (IF lcLP = "Mandarina1" THEN "REDIRECTION_OTAFAILED1" ELSE  "REDIRECTION_OTAFAILED2").
      END.  
      ELSE DO: /* OFF */
         /* send Procera */
         lcCommand = "SET:XFPL:TRANSID,431012536:MSISDN," + lcMSISDN + ":LP,remove;".
      END.
      
      PUT STREAM sLog UNFORMATTED
        lcLine + ";" + STRING(TIME,"hh:mm:ss") SKIP.
   END.
   OUTPUT STREAM sLog CLOSE.
   INPUT STREAM sInputFile CLOSE.
   /* Pending of moving processed file. I'm sure there's already a function for this. */

END. 
INPUT STREAM sFile CLOSE.

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














