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
{tmsconst.i}

DEFINE VARIABLE lcHostName        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcReadLine        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lcHost            AS CHARACTER     NO-UNDO.
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

   DEF INPUT PARAMETER icConfFile AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icModule AS CHAR NO-UNDO. /*Additional caller info*/

   DEF VAR liLogTreshold  AS INT  NO-UNDO.

   IF SEARCH(icConfFile) = ? THEN
      RETURN "Configuration file " + icConfFile + " is missing".

   INPUT FROM VALUE(SEARCH(icConfFile)) NO-ECHO.
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
      /*Actually this is directory for building file name*/
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
   
   IF lcUserName = "" THEN RETURN "Username is missing".
   IF lcPassword = "" THEN RETURN "Password is missing".

   IF liLogLevel = 0 OR liLogLevel = ? THEN
      liLogLevel = 2. /* default */

   IF lcLogFile > "" THEN DO:
      liLogTreshold = 500000.
      lcLogFile = lcLogFile + "_" + icModule + "_" +
                  STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") + ".txt".

      fSetLogFileName(lcLogFile).
      fSetGlobalLoggingLevel(liLogLevel).
      fSetLogTreshold(liLogTreshold).
   END. /* IF lcLogFile > "" THEN DO: */
   RETURN "".

END PROCEDURE.

/*Function stores message for resending / further usage*/
/*Resendinf counter will be increased by resending cron*/
FUNCTION fStoreMsg RETURNS CHAR
   (icConfig AS CHAR,
    icMQ     AS CHAR,
    icMsg    AS CHAR,
    icStatus AS CHAR,
    icUsage  AS CHAR):

CREATE AMQMsg.
   ASSIGN AMQMsg.ConfFile = icConfig
          AMQMsg.InsertTS = fMakeTS()
          AMQMsg.MQName = icMQ
          AMQMsg.MsgContent = icMsg
          AMQMsg.StatusCode = icStatus
          AMQMsg.Usage = icUsage.
          AmQMsg.ResendCount = 0.

RETURN "".

END.    



/*Function sends message with parametersw that are defined in icConfFile*/
FUNCTION fSendToMQ RETURNS CHAR
   (icMsg AS CHAR, /*message contents*/
    icMQ AS CHAR, /*message queue*/
    icConfFile AS CHAR, /*configuration file*/
    icModule AS CHAR, /*for identifying log files*/
    lgResending AS LOGICAL): /*flag for creating new entry to MESSAGE db*/
   DEF VAR lcRet AS CHAR NO-UNDO.
   DEF VAR lcDBStatus  AS CHAR NO-UNDO.

   RUN pInitialize(INPUT icConfFile, INPUT icModule).

   icMsg = fNotNull(icMsg).
   IF icMsg EQ "" THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE("", "AMQ: Message is empty").
      RETURN "AMQ: Message is empty".
   END.

   IF RETURN-VALUE > "" THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR in init").
         IF lgResending EQ FALSE THEN DO:
            /*This is the 1st sending, create new entry*/
            fStoreMsg( icConfFile, icMQ, icMsg, {&AMQ_MSG_INIT_FAILED}, "dms").
         END.

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
   RUN pFinalize.

   IF lgResending EQ FALSE THEN DO:
      /*This is the 1st sending, create new entry*/
      lcDBStatus = "".
      IF lcRet NE "" THEN lcDBStatus = lcRet.
      ELSE lcDbStatus = {&AMQ_MSG_SENT}.

      fStoreMsg( icConfFile, icMQ, icMsg, lcDBStatus, "dms").
   END.

   RETURN lcRet.

END.


PROCEDURE pFinalize:

   IF lcLogFile > "" THEN fCloseLog().

   IF VALID-OBJECT(lMsgPublisher) THEN DELETE OBJECT lMsgPublisher.

END PROCEDURE.
&ENDIF
