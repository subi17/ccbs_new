/* ----------------------------------------------------------------------
  MODULE .......: mandarina_bob.p
  TASK .........: BOB / Read input file; send commands to Procera to 
                  set or remove a redirection to a LP.
  APPLICATION ..: tms
  AUTHOR .......: jotorres
  CREATED ......: 08/2017
  Version ......: yoigo
---------------------------------------------------------------------- */

---------------------------------------------------------------------- 
https://kethor.qvantel.com/browse/MANDLP-8
2.1 Yoigo employee can upload a file to bob tool to set or remove 
     a redirection to a LP
---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcbrand = "1".

/* Directories */
DEF VAR lcBaseDirectory     AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/".
DEF VAR lcSpoolDirectory    AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/spool/". 
DEF VAR lcIncomingDirectory AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/incoming/". 
DEF VAR lcOutgoingDirectory AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/outgoing/". 
DEF VAR lcLogsDirectory     AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/mandarina/logs/". 

/* Input file content */
DEF VAR lcMSISDN AS CHAR NO-UNDO.  /* MSISDN */
DEF VAR lcLP     AS CHAR NO-UNDO. /* [Mandarina1|Mandarina2] */
DEF VAR lcAction AS CHAR NO-UNDO. /* [ON|OFF] */

/* Actual file. */
DEF VAR lcTableName     AS CHAR NO-UNDO.
DEF VAR lcActionID      AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.
 
/* Files */
DEFINE STREAM sInputFile.
DEFINE STREAM sLog.


/* Getting directories from CParams */
ASSIGN
   lcBaseDirectory     = fCParam("Mandarina", "MandarinaBaseDir")
   lcSpoolDirectory    = fCParam("Mandarina", "MandarinaSpoolDir")
   lcIncomingDirectory = fCParam("Mandarina", "MandarinaIncomingDir")
   lcOutgoingDirectory = fCParam("Mandarina", "MandarinaOutgoingDir")
   lcLogsDirectory     = fCParam("Mandarina", "MandarinaLogsDir").

/* Check is process ir running */
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

INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcSpoolDirectory).
REPEAT:
   IMPORT STREAM sFile UNFORMATTED lcFileName.
   lcInputFile = lcSpoolDirectory + lcFileName.
   IF SEARCH(lcInputFile) NE ? THEN DO:
      INPUT STREAM sFile FROM VALUE (lcInputFile).
      OUTPUT STREM sLog TO VALUE(lcLogsDirectory + "LP_" + lcFileName + ".log") 
   END.
   ELSE NEXT. 

   REPEAT:
     INPUT STREAM sFile FROM VALUE(lcInputFile).
     IMPORT STREAM sFile UNFORMATTED cLine.
     IF NUM-ENTRIES(cLine, ";") <> 3 THEN
        NEXT.
      ASSIGN
         lcMSISDN = ENTRY(1, cLine, ";")
         lcLP     = ENTRY(2, cLine, ";")
         lcAction = ENTRY(3, cLine, ";"). 
      IF lcAction = "ON" THEN DO:
         /* send Procera */
         lcCommand = "ET:XFPL:TRANSID,431012496:MSISDN," + lcMSISDN + ":" + 
                     "LP," + (IF lcLP = "Mandarina1" THEN "REDIRECTION_OTAFAILED1" ELSE  "REDIRECTION_OTAFAILED2").
      END.  
      ELSE DO: /* OFF */
         lcCommand = "SET:XFPL:TRANSID,431012536:MSISDN," + lcMSISDN + ":LP,remove;".
      END.
      
      PUT STREAM sLog UNFORMATTED
        cLine + ";" + STRING(TIME,"hh:mm:ss") SKIP.
   END.
   OUTPUT STREAM sLog CLOSE.
   INPUT STREAM sFile CLOSE.
   /* Pending move processed file */
END.
















