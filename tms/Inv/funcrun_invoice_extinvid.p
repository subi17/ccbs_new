/* ----------------------------------------------------------------------
  MODULE .......: FR_INVOICE_EXTINVIND.P
  TASK .........: Invoice numbering (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 27.04.10
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

DEF VAR ldaInvDate    AS DATE NO-UNDO.
DEF VAR lcMessage     AS CHAR NO-UNDO.
DEF VAR liInvType     AS INT  NO-UNDO.
DEF VAR liInvCnt      AS INT  NO-UNDO.
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

ASSIGN 
   ldaInvDate = fSetFuncRunDateParameter(1)
   liInvType  = fSetFuncRunIntParameter(2).

IF ldaInvDate = ? OR liInvType = ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

RUN invoice_extinvid.p(ldaInvDate,
                       liInvType,
                       2,   /* action */  
                       liFRProcessID,
                       liUpdateInterval,
                       OUTPUT liInvCnt).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.   


RUN pFinalizeFuncRunProcess(liFRProcessID,liInvCnt).

QUIT.

/******** Main end *******/


