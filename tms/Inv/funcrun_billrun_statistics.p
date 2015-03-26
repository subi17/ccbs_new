/* ----------------------------------------------------------------------
  MODULE .......: funcrun_billrun_statistics.p
  TASK .........: Print a billrun performance statistics (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 13.08.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{cparam2.i}
{files.i}
{timestamp.i}
{funcrunprocess_run.i}
{funcrunprocess_update.i}

DEF VAR liLoop            AS INT  NO-UNDO.
DEF VAR liFRProcessID     AS INT  NO-UNDO.
DEF VAR liFRExecID        AS INT  NO-UNDO.
DEF VAR lcRunMode         AS CHAR NO-UNDO.
DEF VAR liUpdateInterval  AS INT  NO-UNDO.
DEF VAR ldaInvDate        AS DATE NO-UNDO.
DEF VAR liInvType         AS INT  NO-UNDO.
DEF VAR liInterval        AS INT  NO-UNDO.
DEF VAR ldtLastRun        AS DATETIME NO-UNDO.
DEF VAR llStop            AS LOG  NO-UNDO.
DEF VAR liFeedFromExecSeq AS INT  NO-UNDO.
DEF VAR liProcessedEvents AS INT  NO-UNDO.
DEF VAR liInvQty          AS INT  NO-UNDO.

DEF BUFFER bFollowExec FOR FuncRunExec.
DEF BUFFER bFollowFuncRunProcess FOR FuncRunProcess.


/****** Main start ********/

RUN pInitializeFuncRunProcess(OUTPUT liFRProcessID,
                              OUTPUT liFRExecID,    
                              OUTPUT lcRunMode,
                              OUTPUT liUpdateInterval).
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pGetFuncRunProcessParameters(liFRProcessID).

ASSIGN 
   ldaInvDate = fSetFuncRunDateParameter(1)
   liInvType  = fSetFuncRunIntParameter(2)
   liInterval = fSetFuncRunIntParameter(3).
   
IF ldaInvDate = ? OR liInvType = 0 OR liInterval = 0 THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

ASSIGN 
   liInterval = MAX(liInterval,15)
   ldtLastRun = NOW.

FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = liFRExecID NO-LOCK.
liFeedFromExecSeq = FuncRunExec.FeedFromExecSeq.

DO WHILE TRUE:

   /* check that billing run is on */
   ASSIGN llStop = TRUE
          liProcessedEvents = 0.

   IF liFeedFromExecSeq > 0 THEN DO:
      FIND FIRST bFollowExec WHERE 
                 bFollowExec.FRExecID = liFeedFromExecSeq 
      NO-LOCK NO-ERROR.
      IF AVAILABLE bFollowExec THEN DO:
         IF LOOKUP(bFollowExec.RunState,"Initialized,Running") > 0 THEN
            llStop = FALSE.
         ELSE DO:
            FOR EACH bFollowFuncRunProcess NO-LOCK WHERE
                     bFollowFuncRunProcess.FRExecId = bFollowExec.FRExecID:
                liProcessedEvents = liProcessedEvents +
                                    bFollowFuncRunProcess.Processed.
            END.
         END.
      END.
   END.

   IF llStop AND liProcessedEvents > 0 AND
      liProcessedEvents = liInvQty THEN LEAVE.
   
   /* generate the actual statistic in defined intervals (minutes), and the
      last version when billing run has ended */
   IF INTERVAL(NOW,ldtLastRun,"minutes") >= liInterval OR llStop THEN DO:
   
      /* mark already here so that the time that the statistics generation 
         takes doesn't affect this timing */
      ldtLastRun = NOW.  
      
      RUN billrun_statistics.p (INPUT ldaInvDate,
                                INPUT liInvType,
                                INPUT lcRunMode,
                                OUTPUT liInvQty).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
         QUIT.
      END.   
   
      liLoop = liLoop + 1.
      IF liUpdateInterval > 0 AND liLoop MOD liUpdateInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(liFRProcessID,liLoop) THEN DO:
            RUN pCancelFuncRunProcess(liFRProcessID,"Cancelled").
            QUIT.
         END.
      END.   
      
   END.

   IF llStop THEN LEAVE.
    
   PAUSE 60 NO-MESSAGE. 
END.

RUN pFinalizeFuncRunProcess(liFRProcessID,liLoop).

QUIT.

/******** Main end *******/


