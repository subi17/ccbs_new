/* ----------------------------------------------------------------------------
  MODULE .......: request_daemon.p
  FUNCTION .....: process funcrun batches 
  APPLICATION ..: TMS
  Author........: Subhash Sanjeevi
  CREATED ......: 13.01.16/aam 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "req_daemon".
   
{heartbeat.i}
{timestamp.i}
{log.i}
{cparam2.i}
{eventlog.i}
{host.i}
{tmsconst.i}

DEF VAR liLoop               AS INT  NO-UNDO.
DEF VAR lcLockFile           AS CHAR NO-UNDO.
DEF VAR llCurrentLog         AS LOG  NO-UNDO.
DEF VAR liInterval           AS INT  NO-UNDO.
DEF VAR liParamCheckInterval AS INT  NO-UNDO.
DEF VAR ldtStarted           AS DATETIME NO-UNDO.
DEF VAR ldtLastParamCheck    AS DATETIME NO-UNDO.
DEF VAR ldtLastMonitoring    AS DATETIME NO-UNDO.
DEF VAR ldaLastLoggingDay    AS DATE NO-UNDO.
DEF VAR lcHost               AS CHAR NO-UNDO.
DEF VAR llReplica            AS LOG  NO-UNDO INIT FALSE.
/******** Main start *********/

ASSIGN
   lcLockFile           = fCParamC("ReqDaemonLockFile") /* This cparam has to be created */
   liParamCheckInterval = 5
   llCurrentLog         = FALSE 
   ldtLastParamCheck    = ADD-INTERVAL(NOW,       
                                       -10,
                                       "minutes")
   ldtLastMonitoring    = NOW                                    
   ldaLastLoggingDay    = TODAY
   ldtStarted           = NOW.

IF lcLockFile = ? OR lcLockFile = "" THEN
   lcLockFile = "/tmp/req_daemon.lock".
   
UNIX SILENT VALUE("touch " + lcLockFile).   

RUN pHostName(OUTPUT lcHost).
llReplica = fIsThisReplica().
IF llReplica THEN 
   THIS-PROCEDURE:PRIVATE-DATA = "Host:" + lcHost. 

fELog("REQUEST_DAEMON_" + lcHost,"started").

DO WHILE TRUE 
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN LEAVE. 
 
   /* should I be running */
   FILE-INFO:FILE-NAME = lcLockFile.
   IF FILE-INFO:FILE-TYPE = ? THEN LEAVE.         
   
   /* get the latest configuration */ 
   IF INTERVAL(NOW,ldtLastParamCheck,"minutes") > liParamCheckInterval THEN DO:
      liInterval = fCParamI("ReqDaemonInterval").
      
      IF liInterval = 0 OR liInterval = ? THEN liInterval = 60. 
      
      liParamCheckInterval = MIN(30,INTEGER(liInterval / 60) * 10).
      
      ldtLastParamCheck = NOW.
   END.

   /* write a log once a day */
   IF TODAY > ldaLastLoggingDay THEN DO:
      RUN pWriteLog(ldtStarted,
                    liLoop).
      ldaLastLoggingDay = TODAY.
   END.
   
   IF NOT llReplica THEN  
      RUN pExecPublishingRequest.

   liLoop = liLoop + 1.

   /* monitoring */   
   IF INTERVAL(NOW,ldtLastMonitoring,"minutes") > 10 THEN DO:
      fKeepAlive("REQUEST:Daemon").
      ldtLastMonitoring = NOW.
   END.
   
   PAUSE liInterval NO-MESSAGE.
    
END.

IF llCurrentLog THEN DO:
   fCloseLog().
END.

fELog("REQUEST_DAEMON_" + lcHost,"stopped").

QUIT.

/********** Main end **********/

PROCEDURE pExecPublishingRequest:
DEF VAR lcCommand AS CHAR NO-UNDO. 

   DO ON QUIT UNDO, RETRY:
   
      IF RETRY THEN LEAVE.

      FOR EACH RequestType WHERE 
               RequestType.Brand = gcBrand AND 
               RequestType.Mode  = "Batch" AND
               RequestType.InUse:
         
         /* logging on type level */
         IF RequestType.LogOn THEN DO:

            IF RequestType.LogFile > "" AND RequestType.LogEntry > "" THEN DO:
               fSetLogFileName(RequestType.LogFile).
               fSetLogEntryTypes(RequestType.LogEntry).
               fSetLogTreshold(INTEGER(RequestType.LogThreshold)).      
            
               IF RequestType.LogClear THEN DO:
                  fClearLog().
               END.
            END.
         END. 
         
         FOR EACH RequestStatus OF RequestType NO-LOCK WHERE
                  RequestStatus.InUse:

             /* logging on status level */
            IF RequestStatus.LogOn THEN DO:

               IF RequestStatus.LogFile > "" AND RequestStatus.LogEntry > "" 
               THEN DO:
                  fSetLogFileName(RequestStatus.LogFile).
                  fSetLogEntryTypes(RequestStatus.LogEntry).
                  fSetLogTreshold(INTEGER(RequestStatus.LogThreshold)).      
            
                  IF RequestStatus.LogClear THEN DO:
                     fClearLog().
                  END.
               END.
            END.

            FIND MsRequest NO-LOCK WHERE 
                 MsRequest.Brand     EQ gcBrand             AND
                 MsRequest.ReqType   EQ RequestType.ReqType AND 
                 MsRequest.ReqStatus EQ RequestType.ReqStat NO-ERROR.                 
            IF AVAIL MsRequest THEN DO:

               IF MsRequest.ReqType NE ({&REQTYPE_PUBLISH_IFS}) THEN 
                  RUN publish_invoice.p (MsRequest.MsRequest).
               ELSE IF MsRequest.ReqType NE ({&REQTYPE_PUBLISH_IFS}) THEN 
                  RUN publish_ifs.p (MsRequest.MsRequest).
 
               LEAVE.

            END.                           
         END.
      END.         
   
   END.

END PROCEDURE.

PROCEDURE pWriteLog:

   DEF INPUT PARAMETER idtStarted AS DATETIME NO-UNDO.
   DEF INPUT PARAMETER iiLoops    AS INT  NO-UNDO.
   
   DO TRANS:

      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "RequestType"  
         ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99")  +
                                  STRING(DAY(TODAY),"99")
         ActionLog.ActionID     = "REQUESTDAEMON" + 
                                  (IF llReplica 
                                   THEN "_RPL" 
                                   ELSE "")
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
         ActionLog.ActionStatus = 3
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = fMakeTS()
         ActionLog.ActionChar   = "Started " + 
                                  SUBSTRING(ISO-DATE(idtStarted),1,19) + 
                                  ", " + STRING(iiLoops) + " loops".
   END.

END PROCEDURE.







