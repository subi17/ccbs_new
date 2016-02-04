/* ----------------------------------------------------------------------
  MODULE .......: funcrun_triggerrate
  TASK .........: RUN Rate/rerate from FuncRun
  AUTHOR .......: 14.02.12/aam 
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{Func/cparam2.i}
{Func/files.i}
{Func/timestamp.i}
{Syst/funcrunprocess_run.i}

DEF VAR liItems       AS INT  NO-UNDO.
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

RUN pGetFuncRunProcessParameters(liFRProcessID).

RUN Rate/triggerrate.p (liFRProcessID,
                   liUpdateInterval,
                   OUTPUT liItems).
                   
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.   

RUN pFinalizeFuncRunProcess(liFRProcessID,liItems).

QUIT.

/******** Main end *******/


