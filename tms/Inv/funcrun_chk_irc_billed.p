/* ----------------------------------------------------------------------
  MODULE .......: funcrun_chk_irc_billed.p
  TASK .........: Check invoice row counters of invoices against cdrs
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
{Inv/chk_billed_invrowcounter.i}

DEF VAR liCounterCnt  AS INT  NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.


/****** Main start ********/

RUN pInitializeFuncRunProcess(OUTPUT liFRProcessID,
                               OUTPUT liFRExecID,    
                               OUTPUT lcRunMode,
                               OUTPUT liUpdateInterval).
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = liFRProcessID
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE FuncRunProcess THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"Process not available").
   QUIT.
END.   

RUN pGetFeeds.

RUN Inv/chk_cdr_invrowcounter_billed.p(INPUT TABLE ttInvoice BY-REFERENCE,
                                   FuncRunProcess.ProcSeq,
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
      CREATE ttInvoice.
      ASSIGN
         ttInvoice.InvNum  = FuncRunResult.IntParam
         ttInvoice.InvCust = FuncRunResult.DecParam
         ttInvoice.Order   = FuncRunResult.ResultOrder.
   END. 

END PROCEDURE.



