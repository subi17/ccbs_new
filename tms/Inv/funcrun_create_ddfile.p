/* ----------------------------------------------------------------------
  MODULE .......: FR_CREATE_DDFILE
  TASK .........: Creates direct debit file CSB19 (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 29.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{cparam2.i}
{funcrunprocess_run.i}

DEF VAR ldaInvDate    AS DATE NO-UNDO.
DEF VAR liInvType     AS INT  NO-UNDO.
DEF VAR llSplitBank   AS LOG  NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR liInvCount    AS INT  NO-UNDO.
DEF VAR liFileCount   AS INT  NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO.
DEF VAR llCSBValidation AS LOG NO-UNDO. 
DEF VAR lcInputFileDir AS CHAR NO-UNDO. 

DEF STREAM sLog.


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
   ldaInvDate      = fSetFuncRunDateParameter(1)
   liInvType       = fSetFuncRunIntParameter(2)
   llSplitBank     = fSetFuncRunLogParameter(3)
   llCSBValidation = fSetFuncRunLogParameter(4)
   lcInputFileDir  = fSetFuncRunCharParameter(5).

IF ldaInvDate = ? OR liInvType = ? OR llSplitBank EQ ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

IF llCSBValidation EQ ? THEN llCSBValidation = FALSE.
IF lcInputFileDir  EQ ? THEN lcInputFileDir = "".

RUN ddoutfileco.p ("",
                   0,
                   99999999,
                   "",
                   "_",
                   ldaInvDate,
                   liInvType,
                   "",
                   0,             /* printstate from */
                   1,             /* printstate to */
                   "",
                   FALSE,         /* no empty file */
                   llSplitBank,
                   liFRProcessID,
                   liUpdateInterval,
                   lcRunMode,
                   llCSBValidation,
                   lcInputFileDir,
                   OUTPUT liInvCount,
                   OUTPUT liFileCount,
                   OUTPUT lcError).
 
IF RETURN-VALUE BEGINS "ERROR" OR lcError BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pFinalizeFuncRunProcess(liFRProcessID,liInvCount).

QUIT.

/******** Main end *******/


