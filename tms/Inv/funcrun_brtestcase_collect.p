/* ----------------------------------------------------------------------
  MODULE .......: funcrun_brtestcase_collect
  TASK .........: Collect test cases for billrun (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 12.03.12
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
{tmsconst.i}

DEF VAR liBRTestQueueID  AS INT  NO-UNDO.
DEF VAR ldaPeriodBeg     AS DATE NO-UNDO.
DEF VAR ldaPeriodEnd     AS DATE NO-UNDO.
DEF VAR ldaInvDate       AS DATE NO-UNDO.
DEF VAR ldaDueDate       AS DATE NO-UNDO.
DEF VAR liFeePeriod      AS INT  NO-UNDO.
DEF VAR liInvType        AS INT  NO-UNDO.
DEF VAR liRunQty         AS INT  NO-UNDO.
DEF VAR liCaseCnt        AS INT  NO-UNDO.
DEF VAR liFRProcessID    AS INT  NO-UNDO.
DEF VAR liFRExecID       AS INT  NO-UNDO.
DEF VAR lcRunMode        AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT  NO-UNDO.
DEF VAR llMergeAnalysis  AS LOG  NO-UNDO.

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
   ldaPeriodBeg    = fSetFuncRunDateParameter(2)
   ldaPeriodEnd    = fSetFuncRunDateParameter(3)
   liFeePeriod     = fSetFuncRunIntParameter(4)
   ldaInvDate      = fSetFuncRunDateParameter(5)
   ldaDueDate      = fSetFuncRunDateParameter(6)
   llMergeAnalysis = fSetFuncRunLogParameter(7)
   liInvType       = {&INV_TYPE_TEST}.

IF ldaDueDate NE ? THEN DO:
   IF ldaDueDate < ldaInvDate THEN 
      RETURN "ERROR:Due date cannot be earlier than invoice date".
            
   IF ldaDueDate > ldaInvDate + 60 THEN 
      RETURN "ERROR:Due date is too far in the future".
END.
 
IF liBRTestQueueID = 0 OR ldaPeriodBeg = ? OR ldaPeriodEnd = ? OR
   ldaInvDate = ? OR liFeePeriod = ? OR liInvType = ?
THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

RUN brtestcase_collect.p(liBRTestQueueID,
                         ldaPeriodBeg,
                         ldaPeriodEnd,
                         liFeePeriod,
                         ldaInvDate,
                         ldaDueDate,
                         liInvType,
                         liFRProcessID,
                         liFRExecID,
                         liUpdateInterval,
                         llMergeAnalysis,
                         OUTPUT liCaseCnt).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.   


RUN pFinalizeFuncRunProcess(liFRProcessID,liCaseCnt).

QUIT.

/******** Main end *******/


