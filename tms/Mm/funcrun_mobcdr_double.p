/* ----------------------------------------------------------------------
  MODULE .......: funcrun_mobcdr_double.p
  TASK .........: Check doubles from mobcdrs (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 04.01.12
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

DEF VAR lcMessage     AS CHAR NO-UNDO.
DEF VAR liInvCnt      AS INT  NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR ldaReadDate   AS DATE NO-UNDO.
DEF VAR lcLogFile     AS CHAR NO-UNDO. 
DEF VAR liMarked      AS INT  NO-UNDO.

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

ldaReadDate = fSetFuncRunDateParameter(1).
   
IF ldaReadDate = ? THEN ldaReadDate = TODAY. 

lcLogFile = fCParamC("DoubleCallLog").
IF lcLogFile = ? THEN lcLogFile = "".
lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                      STRING(MONTH(TODAY),"99") + 
                                      STRING(DAY(TODAY),"99")).

RUN Mm/mobcdr_double_check.p ("ReadDate",
                           ldaReadDate,
                           ldaReadDate,
                           "",
                           TRUE,
                           FALSE,
                           lcLogFile,
                           liFRProcessID,
                           liUpdateInterval,
                           lcRunMode,
                           OUTPUT liMarked).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.   

RUN pFinalizeFuncRunProcess(liFRProcessID,liMarked).

QUIT.

/******** Main end *******/


