/* ----------------------------------------------------------------------
  MODULE .......: funcrun_combine_doc1.p
  TASK .........: Combine doc1 files into the final one (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 07.07.10
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

DEF VAR liFRProcessID    AS INT  NO-UNDO.
DEF VAR liFRExecID       AS INT  NO-UNDO.
DEF VAR lcRunMode        AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT  NO-UNDO.
DEF VAR ldaInvDate       AS DATE NO-UNDO.
DEF VAR liFileCnt        AS INT  NO-UNDO.

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

ldaInvDate = fSetFuncRunDateParameter(1).
   
IF ldaInvDate = ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

RUN combine_doc1_files.p (ldaInvDate,
                          liFRProcessID,
                          liUpdateInterval,
                          lcRunMode,
                          OUTPUT liFileCnt).
                
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.   

RUN pFinalizeFuncRunProcess(liFRProcessID,liFileCnt).

QUIT.

/******** Main end *******/


