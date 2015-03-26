/* ----------------------------------------------------------------------
  MODULE .......: FX_INVPRINT_SPLIT
  TASK .........: Creates customer split files (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 27.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{funcrunprocess_run.i}

DEF VAR ldaInvDate    AS DATE NO-UNDO.
DEF VAR liInvType     AS INT  NO-UNDO.
DEF VAR lcPrintHouse  AS CHAR NO-UNDO.
DEF VAR lcFileType    AS CHAR NO-UNDO.
DEF VAR liRunAmt      AS INT  NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR liInvCnt       AS INT  NO-UNDO.
DEF VAR lcList         AS INT  NO-UNDO.
DEF VAR llOnlyNew      AS LOG  NO-UNDO.
DEF VAR liUseReplica   AS INT  NO-UNDO.
DEF VAR llCheckNumbers AS LOG  NO-UNDO.

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
   lcPrintHouse = fSetFuncRunCharParameter(3)
   lcFileType   = fSetFuncRunCharParameter(4)
   liRunAmt     = fSetFuncRunIntParameter(5)
   llOnlyNew    = fSetFuncRunLogParameter(6)
   liUseReplica = fSetFuncRunIntParameter(7)
   llCheckNumbers = fSetFuncRunLogParameter(8).

IF ldaInvDate = ? OR liInvType = ? OR lcFileType = ? OR liRunAmt = ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   
IF llCheckNumbers = ? THEN llCheckNumbers = TRUE.
IF lcPrintHouse = ? THEN lcPrintHouse = "".


RUN printdoc1_split.p (ldaInvDate,
                       llOnlyNew,
                       (liInvType = 1),
                       lcPrintHouse,
                       liRunAmt,
                       lcFileType,
                       TRUE,
                       liFRProcessID,
                       liFRExecID,
                       liUpdateInterval,
                       liUseReplica,
                       llCheckNumbers,
                       OUTPUT lcList,
                       OUTPUT liInvCnt).

IF RETURN-VALUE > "" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pFinalizeFuncRunProcess(liFRProcessID,liInvCnt).

QUIT.

/******** Main end *******/


