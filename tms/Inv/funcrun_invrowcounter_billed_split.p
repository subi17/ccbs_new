/* ----------------------------------------------------------------------
  MODULE .......: funcrun_invrowcounter_billed_split.p
  TASK .........: Split subscriptions for billed invrowcounter check
  CREATED ......: 19.12.12/aam 
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{Syst/funcrunprocess_run.i}

DEF VAR ldaInvDate    AS DATE NO-UNDO.
DEF VAR liInvType     AS INT  NO-UNDO.
DEF VAR liRunAmt      AS INT  NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR liInvCnt      AS INT  NO-UNDO.
DEF VAR liUseReplica  AS INT  NO-UNDO.

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
   ldaInvDate   = fSetFuncRunDateParameter(1)
   liInvType    = fSetFuncRunIntParameter(2)
   liRunAmt     = fSetFuncRunIntParameter(3)
   liUseReplica = fSetFuncRunIntParameter(4).

IF liRunAmt = ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   
IF liUseReplica = ? THEN liUseReplica = 0.


RUN invrowcounter_billed_split.p(ldaInvDate,
                                 liInvType,
                                 liRunAmt,
                                 liFRProcessID,
                                 liFRExecID,
                                 liUpdateInterval,
                                 liUseReplica,
                                 OUTPUT liInvCnt).

IF RETURN-VALUE > "" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pFinalizeFuncRunProcess(liFRProcessID,liInvCnt).

QUIT.

/******** Main end *******/


