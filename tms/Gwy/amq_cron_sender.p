/*Batch job that makes active message queue sending*/
/*Message is sent if status is notsent*/
/*Implemetned to be run as cron job*/
/*Future plans:
   TODO: Move the job to run in screen as replog_reader.
*/


{commpaa.i}
gcBrand = "1".
Katun = "Cron".
{tmsconst.i}
{timestamp.i}
{cparam2.i}
{date.i}
{amq.i}

DEF VAR lcActionID        AS CHAR NO-UNDO.
DEF VAR lcTableName       AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS   AS DEC  NO-UNDO.
DEF VAR lcSendStatus      AS CHAR NO-UNDO.

lcTableName = "DMS".
lcActionID = {&AMQ_RESENDER}.
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
      RETURN. /*No reporting in first time.*/
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE Actionlog.
   END.
END.



FOR EACH AmqMsg WHERE 
         AmqMsg.StatusCode NE {&AMQ_MSG_SENT}:

   AmqMsg.ResendCount = AmqMsg.ResendCount + 1.           
   lcSendStatus = fSendToMQ(AMQMsg.MsgContent,
                            AMQMsg.MQName, 
                            AMQMsg.ConfFile, 
                            "AMQ_resender", 
                            TRUE).
   IF lcSendStatus EQ "" THEN DO:
      AMQMsg.StatusCode = {&AMQ_MSG_SENT}.
   END.
   ELSE DO:
      AMQMsg.StatusCode = lcSendStatus.
      AMQMsg.InsertTS = fMakeTS().
   END.
END.



DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.

