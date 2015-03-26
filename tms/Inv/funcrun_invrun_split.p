/* ----------------------------------------------------------------------
  MODULE .......: FUNCRUN_INVRUN_SPLIT
  TASK .........: Creates customer split files (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 22.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{funcrunprocess_run.i}

DEF VAR ldaDateFrom   AS DATE NO-UNDO.
DEF VAR ldaDateTo     AS DATE NO-UNDO.
DEF VAR liFeePeriod   AS INT  NO-UNDO.
DEF VAR liRunAmt      AS INT  NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR liCustCnt     AS INT  NO-UNDO.
DEF VAR lcList        AS INT  NO-UNDO.
DEF VAR liCustomerQty AS INT  NO-UNDO.
DEF VAR liCustPickMode AS INT  NO-UNDO.
DEF VAR lcTestFile    AS CHAR NO-UNDO.

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
   ldaDateFrom = fSetFuncRunDateParameter(1)
   ldaDateTo   = fSetFuncRunDateParameter(2)
   liRunAmt    = fSetFuncRunIntParameter(3).
IF lcRunMode = "test" THEN ASSIGN
   liCustomerQty  = fSetFuncRunIntParameter(4)
   liCustPickMode = fSetFuncRunIntParameter(5).

lcTestFile     = fSetFuncRunCharParameter(6).

IF ldaDateFrom = ? OR ldaDateTo = ? OR liRunAmt = ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

liFeePeriod = YEAR(ldaDateTo) * 100 + MONTH(ldaDateTo).
   
RUN invrun_split.p (?,
                    ?,
                    ldaDateFrom,
                    ldaDateTo,
                    liFeePeriod,
                    ?,
                    liRunAmt,
                    liFRProcessID,
                    liFRExecID,
                    liUpdateInterval,
                    lcRunMode,
                    liCustomerQty,
                    liCustPickMode,
                    lcTestFile,
                    OUTPUT lcList,
                    OUTPUT liCustCnt).

RUN pFinalizeFuncRunProcess(liFRProcessID,liCustCnt).

QUIT.

/******** Main end *******/


