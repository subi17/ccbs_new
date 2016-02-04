/* ----------------------------------------------------------------------
  MODULE .......: funcrun_chk_invrowcounter.p
  TASK .........: Check unbilled invoice row counters against cdrs
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 21.02.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{Func/cparam2.i}
{Func/files.i}
{Func/timestamp.i}
{Syst/funcrunprocess_run.i}
{Func/date.i}
{Inv/chk_cdr_invrowcounter.i}

DEF VAR liCounterCnt  AS INT  NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR ldaPeriodEnd  AS DATE NO-UNDO.


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

FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = liFRProcessID
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE FuncRunProcess THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"Process not available").
   QUIT.
END.   

ldaPeriodEnd = fLastDayOfMonth(TODAY).

RUN pGetFeeds.

RUN Inv/chk_cdr_invrowcounter.p(INPUT TABLE ttSubs BY-REFERENCE,
                            FuncRunProcess.ProcSeq,    
                            ldaPeriodEnd,
                            liFRProcessID,
                            liUpdateInterval,
                            OUTPUT liCounterCnt).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.   


RUN pFinalizeFuncRunProcess(liFRProcessID,liCounterCnt).

QUIT.

/******** Main end *******/


PROCEDURE pGetFeeds:

   FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = liFRProcessID 
      NO-LOCK.
   FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = liFRExecID 
      NO-LOCK.
 
   FOR EACH FuncRunResult NO-LOCK WHERE
            FuncRunResult.FRExecID    = FuncRunExec.FeedFromExecSeq AND
            FuncRunResult.FRResultSeq = FuncRunProcess.ProcSeq:
      CREATE ttSubs.
      ASSIGN
         ttSubs.MsSeq   = FuncRunResult.IntParam
         ttSubs.InvCust = FuncRunResult.DecParam
         ttSubs.Order   = FuncRunResult.ResultOrder.
   END. 

END PROCEDURE.


