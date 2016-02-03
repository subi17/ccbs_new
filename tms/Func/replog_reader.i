&IF "{&REPLOG_READER_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE REPLOG_READER_I YES
/* ----------------------------------------------------------------------
  Module .......: Func/replog_reader.i
  Task .........: Replog / ActiveMQ Reader Include File
  Application ..: TMS
  Author .......: Vikas
  Created ......: 03.07.13
  Version ......: Yoigo
---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/date.i}
{Func/log.i}
{Func/ftransdir.i}
{Func/cparam2.i}

DEFINE VARIABLE liLoop            AS INTEGER       NO-UNDO.
DEFINE VARIABLE ldToday           AS DATE          NO-UNDO.
DEFINE VARIABLE ldDate            AS DATE          NO-UNDO.
DEFINE VARIABLE ldLogDate         AS DATE          NO-UNDO.
DEFINE VARIABLE lcTime            AS CHARACTER     NO-UNDO.
DEFINE VARIABLE ldeTimeStamp      AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lcDel             AS CHARACTER     NO-UNDO INIT "|".
DEFINE VARIABLE liAmount          AS INTEGER       NO-UNDO.
DEFINE VARIABLE liRepLogs         AS INTEGER       NO-UNDO.
DEFINE VARIABLE lcHostName        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcConfFile        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcReadLine        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcHost            AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcDumpSpool       AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcDumpOut         AS CHARACTER     NO-UNDO.
DEFINE VARIABLE liDumpFreq        AS INTEGER       NO-UNDO.
DEFINE VARIABLE lcDumpFile        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcLogFile         AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcLogFileStat     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE llLogFileAct      AS LOGICAL       NO-UNDO INIT ?.
DEFINE VARIABLE liPort            AS INTEGER       NO-UNDO.
DEFINE VARIABLE liTimeOut         AS INTEGER       NO-UNDO.
DEFINE VARIABLE liLogLevel        AS INTEGER       NO-UNDO.
DEFINE VARIABLE lcUserName        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcPassword        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcOperator        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcServer          AS CHARACTER     NO-UNDO.
DEFINE VARIABLE liOffSet          AS INTEGER       NO-UNDO.
DEFINE VARIABLE lcNagiosURL       AS CHARACTER     NO-UNDO.

/* SER-8026 change */
DEF VAR lcMonitorMethod AS CHAR NO-UNDO.
DEF VAR lcMonitorDir    AS CHAR NO-UNDO.
DEF STREAM sNagios.

DEFINE VARIABLE lcStatLogFile     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcCdrSpool        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcCdrOut          AS CHARACTER     NO-UNDO.
DEFINE VARIABLE liCdrFreq         AS INTEGER       NO-UNDO.
DEFINE VARIABLE ldaCounterDate    AS DATE          NO-UNDO.

/* amq_reader_* - common variables */
DEFINE VARIABLE ldaReadDate       AS DATE          NO-UNDO.
DEFINE VARIABLE ldeReadInTS       AS DECIMAL       NO-UNDO.
DEFINE VARIABLE ldeCDRStamp       AS DECIMAL       NO-UNDO.
DEFINE VARIABLE ldeCurrStamp      AS DECIMAL       NO-UNDO.
DEFINE VARIABLE liReadTime        AS INTEGER       NO-UNDO.
DEFINE VARIABLE ldaConnectDate    AS DATE          NO-UNDO.
DEFINE VARIABLE liCdrCount        AS INTEGER       NO-UNDO.
DEFINE VARIABLE lcDel2            AS CHARACTER     NO-UNDO. 
DEFINE VARIABLE lcMessage         AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcKeyValue        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE llOk              AS LOGICAL       NO-UNDO.
DEFINE VARIABLE llStart           AS LOGICAL       NO-UNDO INIT TRUE.

DEFINE VARIABLE lMsgPublisher     AS CLASS Gwy.MqPublisher NO-UNDO.

DEFINE STREAM sDump.
DEFINE STREAM sLogStat.

DEFINE TEMP-TABLE ttNagios NO-UNDO
   FIELD tcCommand AS CHARACTER
   FIELD tdeTS1    AS DECIMAL
   FIELD tdeTS2    AS DECIMAL.

DEFINE TEMP-TABLE ttStat NO-UNDO
   FIELD ttDatabase  AS CHAR
   FIELD ttTable     AS CHAR
   FIELD ttEventType AS CHAR
   FIELD ttEvents    AS INT
   FIELD ttDate      AS DATE
   FIELD ttHour      AS INT
   INDEX Date   ttDate
   INDEX TypeH  ttDatabase ttTable ttEventType ttDate ttHour.

ASSIGN
   lcOperator = fCparam("NAGIOS","Operator")
   liOffSet   = INT(fCParam("NAGIOS","TimeOffSet"))
   lcMonitorMethod = fCParam("NAGIOS","MonitorMethod").
IF lcMonitorMethod = ? THEN lcMonitorMethod = "File".

IF lcMonitorMethod = "File" THEN DO:
   lcMonitorDir = fCParam("NAGIOS","MonitorDir").
   IF lcMonitorDir = ? THEN lcMonitorDir = "/tmp".
END.   

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

FUNCTION fDateToString RETURNS CHAR (INPUT idaDate AS DATE):

   IF idaDate EQ ? THEN RETURN "".
   
   RETURN STRING(idaDate).

END.

FUNCTION fSocWrite RETURNS LOGICAL
  (INPUT pcMessage        AS CHARACTER,
   INPUT pcServerAndPort  AS CHARACTER):
   
   DEFINE VARIABLE lhSocket AS HANDLE  NO-UNDO.
   DEFINE VARIABLE lmMemPtr AS MEMPTR  NO-UNDO.
   DEFINE VARIABLE llReturn AS LOGICAL NO-UNDO.
   
   CREATE SOCKET lhSocket.
   lhSocket:SET-SOCKET-OPTION("SO-RCVTIMEO","10").

   lhSocket:CONNECT(pcServerAndPort) NO-ERROR.

 
   IF NOT lhSocket:CONNECTED() THEN llReturn = FALSE.
   ELSE DO:
   
      SET-SIZE(lmMemPtr) = LENGTH(pcMessage) + 1.

      PUT-STRING(lmMemPtr,1,LENGTH(pcMessage)) = pcMessage.

      lhSocket:WRITE(lmMemPtr,1,GET-SIZE(lmMemPtr)) NO-ERROR.
   
      SET-SIZE(lmMemPtr) = 0.
   
   END.
   
   lhSocket:DISCONNECT() NO-ERROR.

   DELETE OBJECT lhSocket.

   RETURN llReturn.

END FUNCTION.

/* SER-8026 change */
FUNCTION fFileWrite RETURNS LOGIC
   (icMessage AS CHAR,
    icFile AS CHAR):
    
   OUTPUT STREAM sNagios TO VALUE(icFile) APPEND.
   PUT STREAM sNagios UNFORMATTED icMessage SKIP.
   OUTPUT STREAM sNagios CLOSE.
       
END FUNCTION.

FUNCTION fNagios RETURNS LOGICAL
  (INPUT pcCommand AS CHARACTER,
   INPUT pcURL     AS CHARACTER):

   DEFINE VARIABLE lcNagios      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcTimeStamp   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCommand     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcDescription AS CHARACTER NO-UNDO.
      
   /* nagios URL */
   IF pcURL = "" THEN RETURN FALSE.
   FIND FIRST TMSParam WHERE
              TMSParam.Brand      = gcBrand  AND
              TMSParam.ParamGroup = "NAGIOS" AND
              TMSParam.ParamCode  = "URL"
   NO-LOCK NO-ERROR.
   IF AVAIL TMSParam AND TRIM(TMSParam.CharVal) NE "" 
      THEN lcNagiosURL = TMSParam.CharVal.
   ELSE RETURN FALSE.

   ASSIGN
      lcCommand     = ENTRY(1,pcCommand,":")
      lcDescription = ENTRY(2,pcCommand,":") WHEN
                      NUM-ENTRIES(pcCommand,":") > 1
      lcTimeStamp   = fTS2HMS(fOffSetTS(liOffSet))
      lcTimeStamp   = SUBSTR(lcTimeStamp,7,4)  +
                      SUBSTR(lcTimeStamp,4,2)  +
                      SUBSTR(lcTimeStamp,1,2)  +
                      SUBSTR(lcTimeStamp,12,2) +
                      SUBSTR(lcTimeStamp,15,2) +
                      SUBSTR(lcTimeStamp,18,2)
      lcNagios      = "[" + lcTimeStamp + "]".
      IF lcMonitorMethod = "File" THEN 
         lcNagios = lcNagios + ";" + lcServer + ";" +
                    lcOperator + ";" + lcCommand.
      ELSE lcNagios = lcNagios +
                      "PROCESS_SERVICE_CHECK_RESULT;" +
                      lcServer + ";" +
                      "Screen_" + lcOperator + "_" + 
                      UPPER(lcCommand) +
                      ";0;Screen Ok - " + lcOperator + " ".
   
   lcNagios = lcNagios + lcDescription.

   /* SER-8026 change */
   IF lcMonitorMethod = "File" THEN DO:
      fFileWrite(lcNagios,lcMonitorDir + "/" + UPPER(lcCommand) + ".txt").
   END.   
   ELSE DO:
      IF lcNagiosURL = ? OR lcNagiosURL = "" THEN RETURN FALSE.
      fSocWrite(lcNagios + CHR(10),lcNagiosUrl).
   END.
      
   RETURN TRUE.
END.

FUNCTION fKeepAlive RETURNS INTEGER
  (INPUT pcCommand AS CHARACTER,
   INPUT pcURL     AS CHARACTER):

   DEFINE VARIABLE ldeAlarmLimit AS DECIMAL   NO-UNDO INITIAL 0.00600.
   DEFINE VARIABLE lcCommand     AS CHARACTER NO-UNDO.

   lcCommand = ENTRY(1,pcCommand,":").

   FIND FIRST ttNagios WHERE
              ttNagios.tcCommand = lcCommand 
   EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL ttNagios THEN DO:

      CREATE ttNagios.
      ASSIGN
         ttNagios.tcCommand = lcCommand
         ttNagios.tdeTS1    = fOffSetTS(1).

      fNagios(pcCommand,pcURL).

   END.
   
   ttNagios.tdeTS2 = fOffSetTS(liOffSet).

   IF ttNagios.tdeTS2 - ttNagios.tdeTS1 > ldeAlarmLimit THEN DO:
   
      fNagios(pcCommand,pcURL).

      ASSIGN
         ttNagios.tdeTS1 = fOffSetTS(1)
         ttNagios.tdeTS2 = ttNagios.tdeTS1.

   END.

   RETURN
      INTEGER((ldeAlarmLimit - (ttNagios.tdeTS2 - ttNagios.tdeTS1)) * 100000).

END.

PROCEDURE pInitialize:

   DEF INPUT PARAMETER icModule AS CHAR NO-UNDO.

   DEF VAR liLogTreshold  AS INT  NO-UNDO.

   /* get hostname */
   INPUT THROUGH uname -n.
   IMPORT lcHostName.
   INPUT CLOSE.

   CASE lcHostName:
      WHEN "Sadira" THEN
         lcConfFile = "Mailconf/replog_reader_conf.dev".
      WHEN "Merak" OR WHEN "Botein" THEN
         lcConfFile = "Mailconf/replog_reader_conf.staging".
      WHEN "Merga" THEN
         lcConfFile = "Mailconf/replog_reader_conf.merga".
      WHEN "Alpheratz" THEN DO:
         IF icModule EQ "revolver" THEN 
            lcConfFile = "Mailconf/revolver.alpheratz".
         ELSE IF icModule EQ "dms" THEN 
            lcConfFile = "Mailconf/dms_messaging_conf.alpheratz".         
         ELSE     
            lcConfFile = "Mailconf/replog_reader_conf.alpheratz".
      END.
      WHEN "Angetenar" THEN
         lcConfFile = "Mailconf/replog_reader_conf.angetenar".         
      WHEN "Hebe"  OR WHEN "Flora" OR WHEN "Pallas" THEN DO:
         IF icModule BEGINS "amq_" 
         THEN lcConfFile = "Mailconf/replog_reader_conf_amq.prod".
         ELSE IF icModule EQ "dms" THEN 
            lcConfFile = "Mailconf/dms_messaging_conf.prod". 
         ELSE lcConfFile = "Mailconf/replog_reader_conf.prod".
      END.
      OTHERWISE lcConfFile = "".
   END CASE.

   IF SEARCH(lcConfFile) = ? THEN
      RETURN "Configuration file " + lcConfFile + " is missing".

   INPUT FROM VALUE(SEARCH(lcConfFile)) NO-ECHO.
   REPEAT:
      IMPORT UNFORMATTED lcReadLine.

      IF lcReadLine BEGINS "-" THEN DO:
         CASE SUBSTRING(lcReadLine,1,2):
            /* Host */
            WHEN "-H" THEN lcHost     = SUBSTR(lcReadLine,4).
            /* Port */
            WHEN "-P" THEN liPort     = INT(SUBSTR(lcReadLine,4)) NO-ERROR.
            /* TimeOut */
            WHEN "-T" THEN liTimeOut  = INT(SUBSTR(lcReadLine,4)) NO-ERROR.
         END CASE.
      END.
      ELSE IF lcReadLine BEGINS "#USER:" THEN
         lcUserName = SUBSTR(lcReadLine,7).
      ELSE IF lcReadLine BEGINS "#PASS:" THEN
         lcPassword = SUBSTR(lcReadLine,7).
      ELSE IF lcReadLine BEGINS "#DUMP_SPOOL:" THEN
         lcDumpSpool = SUBSTR(lcReadLine,13).
      ELSE IF lcReadLine BEGINS "#DUMP_OUTGOING:" THEN
         lcDumpOut = SUBSTR(lcReadLine,16).
      ELSE IF lcReadLine BEGINS "#DUMP_FREQ:" THEN
         liDumpFreq = INT(SUBSTR(lcReadLine,12)) NO-ERROR.
      ELSE IF lcReadLine BEGINS "#LOG_MANAGER_FILE:" THEN
         lcLogFile  = SUBSTR(lcReadLine,19).
      ELSE IF lcReadLine BEGINS "#LOG_LEVEL:" THEN
         liLogLevel = INT(SUBSTR(lcReadLine,12)) NO-ERROR.
      ELSE IF lcReadLine BEGINS "#NAGIOS:" THEN
         lcNagiosURL = SUBSTR(lcReadLine,9).
      ELSE IF lcReadLine BEGINS "#LOG_STAT:" THEN
         lcLogFileStat = SUBSTR(lcReadLine,11).
      ELSE IF lcReadLine BEGINS "#CDR_SPOOL:" THEN
         lcCdrSpool = SUBSTR(lcReadLine,12).
      ELSE IF lcReadLine BEGINS "#CDR_OUTGOING:" THEN
         lcCdrOut = SUBSTR(lcReadLine,15).
      ELSE IF lcReadLine BEGINS "#CDR_FREQ:" THEN
         liCdrFreq = INT(SUBSTR(lcReadLine,11)).
   END.
   INPUT CLOSE.

   IF lcHost = "" THEN RETURN "Host is missing".
   IF liPort = 0 OR liPort = ? THEN RETURN "Port is missing".
   /*
   IF lcUserName = "" THEN RETURN "Username is missing".
   IF lcPassword = "" THEN RETURN "Password is missing".
   */

   IF liLogLevel = 0 OR liLogLevel = ? THEN
      liLogLevel = 2. /* default */

   IF liDumpFreq = 0 OR liDumpFreq = ? THEN
      liDumpFreq = 600. /* 10 mins delay */

   IF lcLogFile > "" THEN DO:
      liLogTreshold = 500000.
      lcLogFile = lcLogFile + "_" + icModule + "_" +
                  STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") + "_" +
                  REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

      fSetLogFileName(lcLogFile).
      fSetGlobalLoggingLevel(liLogLevel).
      fSetLogTreshold(liLogTreshold).
   END. /* IF lcLogFile > "" THEN DO: */

   IF lcCdrSpool > "" AND lcCdrOut = "" THEN RETURN "Outgoing path is missing".

   IF liCdrFreq = 0 OR liCdrFreq = ? THEN liCdrFreq = 120.

   ldaCounterDate = TODAY.

END PROCEDURE.

PROCEDURE pFinalize:

   DEF INPUT PARAMETER icModule AS CHAR NO-UNDO.

   IF llLogFileAct = FALSE THEN DO:
      OUTPUT STREAM sDump CLOSE.

      UNIX SILENT VALUE("gzip " + lcDumpFile).
      lcDumpFile = lcDumpFile + ".gz".

      /* Move the report to Transfer directory */
      IF lcDumpOut > "" THEN
         fMove2TransDir(lcDumpFile, ".txt", lcDumpOut).
   END. /* IF llLogFileAct = FALSE THEN DO: */

   IF lcLogFile > "" THEN fCloseLog().

   /* Replog handling statistics file */
   IF lcLogFileStat > "" AND ldLogDate NE ? THEN DO:

      lcStatLogFile = lcLogFileStat + "_" + icModule + "_" +
                      STRING(YEAR(ldLogDate)) + STRING(MONTH(ldLogDate),"99") +
                      STRING(DAY(ldLogDate),"99") + ".txt".

      OUTPUT STREAM sLogStat TO VALUE(lcStatLogFile).

      FOR EACH ttStat WHERE
               ttStat.ttDate = ldLogDate NO-LOCK:
         PUT STREAM sLogStat UNFORMATTED
             STRING(ttStat.ttDate) "|"
             STRING(ttStat.ttHour) "|"
             ttStat.ttDatabase     "|"
             ttStat.ttTable        "|"
             ttStat.ttEventType    "|"
             ttStat.ttEvents       SKIP.
      END.
      OUTPUT STREAM sLogStat CLOSE.
   END. /* IF lcLogFileStat > "" THEN DO: */

   EMPTY TEMP-TABLE ttStat NO-ERROR.

   IF VALID-OBJECT(lMsgPublisher) THEN DELETE OBJECT lMsgPublisher.

END PROCEDURE.

PROCEDURE pDumpFileRotation:

   DEF INPUT PARAMETER icModule AS CHAR NO-UNDO.

   IF llLogFileAct = FALSE THEN DO:
      OUTPUT STREAM sDump CLOSE.

      UNIX SILENT VALUE("gzip " + lcDumpFile).
      lcDumpFile = lcDumpFile + ".gz".

      /* Move the report to Transfer directory */
      IF lcDumpOut > "" THEN
         fMove2TransDir(lcDumpFile, ".txt", lcDumpOut).
   END. /* IF llLogFileAct = FALSE THEN DO: */

   ASSIGN ldeTimeStamp = fSecOffSet(fMakeTS(),liDumpFreq)
          ldDate       = TODAY
          llLogFileAct = TRUE
          lcDumpFile   = lcDumpSpool + "/replog_reader_" + icModule + "_" +
          STRING(YEAR(ldDate)) + STRING(MONTH(ldDate),"99") +
          STRING(DAY(ldDate),"99") + "_" +
          REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

END PROCEDURE.

FUNCTION fWriteMessage RETURNS LOG (INPUT icMessage AS CHAR):

   IF icMessage = ? THEN icMessage = "".

   /* Write message to dump file if log file is active */
   IF llLogFileAct = FALSE THEN
      PUT STREAM sDump UNFORMATTED icMessage SKIP.

   RETURN TRUE.

END. /* FUNCTION fWriteMessage RETURNS LOG */

&ENDIF
