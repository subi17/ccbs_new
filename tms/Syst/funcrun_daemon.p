/* ----------------------------------------------------------------------------
  MODULE .......: funcrun_daemon.p
  FUNCTION .....: process funcrun batches 
  APPLICATION ..: TMS
  CREATED ......: 12.05.10/aam 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "fr_daemon".
   
{Func/heartbeat.i}
{Func/timestamp.i}
{Func/log.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Syst/host.i}

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
   lcLockFile           = fCParamC("FRDaemonLockFile")
   liParamCheckInterval = 5
   llCurrentLog         = FALSE 
   ldtLastParamCheck    = ADD-INTERVAL(NOW,       
                                       -10,
                                       "minutes")
   ldtLastMonitoring    = NOW                                    
   ldaLastLoggingDay    = TODAY
   ldtStarted           = NOW.

IF lcLockFile = ? OR lcLockFile = "" THEN
   lcLockFile = "/tmp/funcrun_daemon.lock".
   
UNIX SILENT VALUE("touch " + lcLockFile).   

RUN pHostName(OUTPUT lcHost).
llReplica = fIsThisReplica().
IF llReplica THEN 
   THIS-PROCEDURE:PRIVATE-DATA = "Host:" + lcHost. 

fELog("FUNCRUN_DAEMON_" + lcHost,"started").

DO WHILE TRUE 
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN LEAVE. 
 
   /* should I be running */
   FILE-INFO:FILE-NAME = lcLockFile.
   IF FILE-INFO:FILE-TYPE = ? THEN LEAVE.         
   
   /* get the latest configuration */ 
   IF INTERVAL(NOW,ldtLastParamCheck,"minutes") > liParamCheckInterval THEN DO:
      RUN pGetConfiguration(INPUT-OUTPUT llCurrentLog,
                            OUTPUT liInterval,
                            OUTPUT liParamCheckInterval). 
      
      ldtLastParamCheck = NOW.
   END.

   /* write a log once a day */
   IF TODAY > ldaLastLoggingDay THEN DO:
      RUN pWriteLog(ldtStarted,
                    liLoop).
      ldaLastLoggingDay = TODAY.
   END.
   
   IF NOT llReplica THEN
      RUN pRunQueue.
   
   RUN pRunExecution.
   
   liLoop = liLoop + 1.

   /* monitoring */   
   IF INTERVAL(NOW,ldtLastMonitoring,"minutes") > 10 THEN DO:
      fKeepAlive("FUNCRUN:Daemon").
      ldtLastMonitoring = NOW.
   END.
   
   PAUSE liInterval NO-MESSAGE.
    
END.

IF llCurrentLog THEN DO:
   fCloseLog().
END.

fELog("FUNCRUN_DAEMON_" + lcHost,"stopped").

QUIT.

/********** Main end **********/


PROCEDURE pGetConfiguration:

   DEF INPUT-OUTPUT PARAMETER olCurrentLog         AS LOG  NO-UNDO.
   DEF OUTPUT PARAMETER       oiInterval           AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER       oiParamCheckInterval AS INT  NO-UNDO.
      
   DEF VAR lcLogFile     AS CHAR NO-UNDO.
   DEF VAR liLogTreshold AS INT  NO-UNDO.
   DEF VAR lcLogEntry    AS CHAR NO-UNDO.
   DEF VAR llLog         AS LOG  NO-UNDO.
    
   ASSIGN 
      oiInterval = fCParamI("FRDaemonInterval")
      llLog      = (fCParamI("FRDaemonLogOn") = 1 AND NOT llReplica).

   IF oiInterval = 0 OR oiInterval = ? THEN oiInterval = 60. 
      
   oiParamCheckInterval = MIN(30,INTEGER(oiInterval / 60) * 10).
         
   IF llLog THEN DO:
      ASSIGN 
         lcLogFile     = fCParamC("FRDaemonLogFile")
         liLogTreshold = fCParamI("FRDaemonLogTreshold")
         lcLogEntry    = fCParamC("FRDaemonLogEntry").
      IF liLogTreshold = 0 OR liLogTreshold = ? THEN 
         liLogTreshold = 500000. 
      IF lcLogEntry = "" OR lcLogEntry = ? THEN 
         lcLogEntry = "4GLTrace:4".
   END.
   
   /* logging wanted */
   IF llLog NE olCurrentLog THEN DO:

      IF NOT llLog THEN DO:
         fCloseLog().
      END.
      ELSE IF lcLogFile > "" THEN DO:
         fSetLogFileName(lcLogFile).
         fSetLogEntryTypes(lcLogEntry).
         fSetLogTreshold(liLogTreshold).      
      END.
      
      olCurrentLog = llLog.    
   END.

END PROCEDURE.

PROCEDURE pRunQueue:

   DO ON QUIT UNDO, RETRY:
   
      IF RETRY THEN LEAVE.
      
      RUN Syst/funcrunqueue_batch.p.
   END.

END PROCEDURE.

PROCEDURE pRunExecution:

   DO ON QUIT UNDO, RETRY:
   
      IF RETRY THEN LEAVE.
      
      RUN Syst/funcrunexec_batch.p.
   END.

END PROCEDURE.

PROCEDURE pWriteLog:

   DEF INPUT PARAMETER idtStarted AS DATETIME NO-UNDO.
   DEF INPUT PARAMETER iiLoops    AS INT  NO-UNDO.
   
   DO TRANS:

      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "FuncRun"  
         ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99")  +
                                  STRING(DAY(TODAY),"99")
         ActionLog.ActionID     = "FRDAEMON" + 
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







