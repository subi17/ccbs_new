/* ----------------------------------------------------------------------
  MODULE .......: funcrun_delete_testinv.p
  TASK .........: Delete test invoices (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 22.06.10
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

DEF VAR lcMessage     AS CHAR NO-UNDO.
DEF VAR liInvCnt      AS INT  NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR liAction      AS INT  NO-UNDO.

/****** Main start ********/

RUN pInitializeFuncRunProcess(OUTPUT liFRProcessID,
                              OUTPUT liFRExecID,    
                              OUTPUT lcRunMode,
                              OUTPUT liUpdateInterval).
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN delete_test_invoice.p("",
                          "",
                          liFRProcessID,
                          liUpdateInterval,
                          lcRunMode,
                          OUTPUT liInvCnt).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.   

RUN pFinalizeFuncRunProcess(liFRProcessID,liInvCnt).

QUIT.

/******** Main end *******/


