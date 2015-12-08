&IF "{&AMQ_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE AMQ_I YES
/* ----------------------------------------------------------------------
  Module .......: Func/amq.i
  Task .........: General ActiveMQ Handler Include File
  Application ..: TMS
  Author .......: ilsavola
  Created ......: 27.11.2015
  Version ......: Yoigo
---------------------------------------------------------------------- */
{commali.i}
{date.i}
{log.i}
{ftransdir.i}

DEFINE VARIABLE lcHostName        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcConfFile        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcReadLine        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcHost            AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcDumpFile        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcLogFile         AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcLogFileStat     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE liPort            AS INTEGER       NO-UNDO.
DEFINE VARIABLE liTimeOut         AS INTEGER       NO-UNDO.
DEFINE VARIABLE liLogLevel        AS INTEGER       NO-UNDO.
DEFINE VARIABLE lcUserName        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcPassword        AS CHARACTER     NO-UNDO.


DEFINE VARIABLE lMsgPublisher     AS CLASS Gwy.MqPublisher NO-UNDO.



FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

FUNCTION fDateToString RETURNS CHAR (INPUT idaDate AS DATE):

   IF idaDate EQ ? THEN RETURN "".
   
   RETURN STRING(idaDate).

END.

PROCEDURE pInitialize:

   DEF INPUT PARAMETER icModule AS CHAR NO-UNDO.

   DEF VAR liLogTreshold  AS INT  NO-UNDO.

   /* get hostname */
   INPUT THROUGH uname -n.
   IMPORT lcHostName.
   INPUT CLOSE.

   CASE lcHostName:
      WHEN "Alpheratz" THEN DO:
         IF icModule EQ "dms" THEN 
            lcConfFile = "Mailconf/dms_messaging_conf.alpheratz".         
      END.
      WHEN "Pallas" THEN DO:
         IF icModule EQ "dms" THEN 
            lcConfFile = "Mailconf/dms_messaging_conf.prod". 
      END.
      OTHERWISE DO:
         lcConfFile = "".
         RETURN "Unknown configuration".
      END.   
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
      ELSE IF lcReadLine BEGINS "#LOG_MANAGER_FILE:" THEN
         lcLogFile  = SUBSTR(lcReadLine,19).
      ELSE IF lcReadLine BEGINS "#LOG_LEVEL:" THEN
         liLogLevel = INT(SUBSTR(lcReadLine,12)) NO-ERROR.
      ELSE IF lcReadLine BEGINS "#LOG_STAT:" THEN
         lcLogFileStat = SUBSTR(lcReadLine,11).
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
   RETURN "".

END PROCEDURE.


FUNCTION fSendToMQ RETURNS CHAR
   (icMsg AS CHAR,
    icInitKey AS CHAR,
    icMQ AS CHAR):
   DEF VAR lcRet AS CHAR NO-UNDO.

   RUN pInitialize(INPUT "dms").

   IF RETURN-VALUE > "" THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").

         RETURN RETURN-VALUE.
   END.

   lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                       liTimeOut,icMQ,
                                       lcUserName,lcPassword).

   IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN DO:
            LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found",
                                    "ERROR").
            lcRet = "ActiveMQ Publisher handle not found".
      END.
   END.
   ELSE IF NOT lMsgPublisher:send_message(icMsg) THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN DO:
         LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
         lcRet = "ActiveMQ message sending failed".
      END.
   END.
   RUN pFinalize(INPUT "").
   RETURN lcRet.

END.


PROCEDURE pFinalize:

   DEF INPUT PARAMETER icModule AS CHAR NO-UNDO.

   IF lcLogFile > "" THEN fCloseLog().



   IF VALID-OBJECT(lMsgPublisher) THEN DELETE OBJECT lMsgPublisher.

END PROCEDURE.
&ENDIF
