/* ----------------------------------------------------------------------
  MODULE .......: migration_notification.p 
  TASK .........: Program sends migration status information to WEB by using
                  message queue.
  APPLICATION ..: TMS
  AUTHOR .......: ilsavola 
  CREATED ......: 20.2.2017 
  CHANGE .......: 
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commali.i}
{tmsconst.i}
{cparam2.i}
{log.i}

DEF INPUT PARAMETER icMessage AS CHAR NO-UNDO.

DEF VAR lcLoginMq          AS CHAR NO-UNDO.
DEF VAR lcPassMq           AS CHAR NO-UNDO.
DEF VAR liPortMq           AS INTEGER   NO-UNDO.
DEF VAR lcServerMq         AS CHAR NO-UNDO.
DEF VAR liTimeOutMq        AS INTEGER   NO-UNDO.
DEF VAR lcQueueMq          AS CHAR NO-UNDO.
DEF VAR liLogLevel         AS INT NO-UNDO.
DEF VAR lcLogManagerFile   AS CHAR NO-UNDO.
DEF VAR lcMigrationLogDir  AS CHAR NO-UNDO.
DEF VAR liLogTreshold      AS IN  NO-UNDO.

DEF VAR lMsgPublisher AS CLASS Gwy.MqPublisher NO-UNDO.

ASSIGN 
   lcLoginMq        = fCParamC("MigrationMqLogin")
   lcPassMq         = fCParamC("MigrationMqPassCode")
   liPortMq         = fCParamI("MigrationMqPort")
   lcServerMq       = fCParamC("MigrationMqServer")
   liTimeOutMq      = fCParamI("MigrationMqTimeOut")
   liLogTreshold    = fCParamI("InvPushLogTreshold")
   lcQueueMq        = fCParamC("MigrationToQueue").

   lcLogManagerFile = lcMigrationLogDir + "Migration_LogManager_" +
                                               STRING(YEAR(TODAY),"9999") +
                                               STRING(MONTH(TODAY),"99")  +
                                               STRING(DAY(TODAY),"99")    +
                                               ".log".


IF liLogLevel = 0 OR liLogLevel = ? THEN
   liLogLevel = 2. /* default */
IF lcLogManagerFile > "" THEN DO:
   fSetLogFileName(lcLogManagerFile).
   fSetGlobalLoggingLevel(liLogLevel).
   fSetLogTreshold(liLogTreshold).
END.

lMsgPublisher = NEW Gwy.MqPublisher(lcServerMq,liPortMq,
                                    liTimeOutMq,lcQueueMq,
                                    lcLoginMq,lcPassMq).

IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE("RabbitMQ Publisher handle not found","ERROR").
END.

IF NOT lMsgPublisher:send_message(icMessage) THEN DO:
   IF LOG-MANAGER:LOGFILE-NAME <> ? AND
   LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
   LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
END.

IF VALID-OBJECT(lMsgPublisher) THEN DELETE OBJECT(lMsgPublisher).


