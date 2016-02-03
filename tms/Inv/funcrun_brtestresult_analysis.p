/* ----------------------------------------------------------------------
  MODULE .......: funcrun_brtestresult_analysis
  TASK .........: Analyset test results of a billrun (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 14.03.12
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

DEF VAR liBRTestQueueID  AS INT  NO-UNDO.
DEF VAR ldaInvDate       AS DATE NO-UNDO.
DEF VAR liInvType        AS INT  NO-UNDO.
DEF VAR liCaseCnt        AS INT  NO-UNDO.
DEF VAR liFRProcessID    AS INT  NO-UNDO.
DEF VAR liFRExecID       AS INT  NO-UNDO.
DEF VAR lcRunMode        AS CHAR NO-UNDO.
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

RUN pGetFuncRunProcessParameters(liFRProcessID).

ASSIGN 
   liBRTestQueueID = fSetFuncRunIntParameter(1)
   ldaInvDate      = fSetFuncRunDateParameter(2)
   liInvType       = fSetFuncRunIntParameter(3).

IF liBRTestQueueID = 0 THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

RUN brtestresult_analysis.p(liBRTestQueueID,
                            ldaInvDate,
                            liInvType,
                            liFRProcessID,
                            liFRExecID,
                            liUpdateInterval,
                            OUTPUT liCaseCnt).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.   

RUN pFinalizeFuncRunProcess(liFRProcessID,liCaseCnt).

QUIT.

/******** Main end *******/


