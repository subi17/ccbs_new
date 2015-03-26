/* ----------------------------------------------------------------------
  MODULE .......: funcrun_invrowcounter_unbilled_split.p
  TASK .........: Split subscriptions for unbilled invrowcounter check
  CREATED ......: 08.11.12/aam 
  ---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{funcrunprocess_run.i}

DEF VAR liRunAmt      AS INT  NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR liSubsCnt      AS INT  NO-UNDO.
DEF VAR liUseReplica   AS INT  NO-UNDO.

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
   liRunAmt     = fSetFuncRunIntParameter(1)
   liUseReplica = fSetFuncRunIntParameter(2).

IF liRunAmt = ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   
IF liUseReplica = ? THEN liUseReplica = 0.


RUN invrowcounter_unbilled_split.p(liRunAmt,
                                   liFRProcessID,
                                   liFRExecID,
                                   liUpdateInterval,
                                   liUseReplica,
                                   OUTPUT liSubsCnt).

IF RETURN-VALUE > "" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pFinalizeFuncRunProcess(liFRProcessID,liSubsCnt).

QUIT.

/******** Main end *******/


