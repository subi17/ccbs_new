/* ----------------------------------------------------------------------
  MODULE .......: funcrun_unbilled_calls_report.p
  TASK .........: Generate unbilled calls report (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: Vikas 
  CREATED ......: 11.09.11
  Modified .....: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN 
   Syst.CUICommon:gcBrand = "1"
   katun   = "Cron".

{Syst/funcrunprocess_run.i}
{Func/direct_dbconnect.i}
{Func/coinv.i}

DEF VAR liFRProcessID    AS INT  NO-UNDO.
DEF VAR liFRExecID       AS INT  NO-UNDO.
DEF VAR lcRunMode        AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT  NO-UNDO.
DEF VAR liCallCnt        AS INT  NO-UNDO.

DEF VAR liBillPeriod       AS INT  NO-UNDO.
DEF VAR ldaEndPeriod       AS DATE NO-UNDO.

DEF VAR ldaPrevDb          AS DATE NO-UNDO.

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

ASSIGN liBillPeriod  = fSetFuncRunIntParameter(1)
       ldaEndPeriod  = fInt2Date(liBillPeriod,1).

IF liBillPeriod = 0 OR ldaEndPeriod = ?
THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.

EMPTY TEMP-TABLE ttDB.

ASSIGN ldaPrevDb = ?.

/* Initialize current DB */
fInitializeConnectTables("MobCDR","").
RUN pGetCurrentDbtt(Syst.CUICommon:gcBrand,
                  "",
                  ldaEndPeriod,
                  ldaEndPeriod).
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

/* Connect the previous db */
FOR FIRST ttDB WHERE 
          ttDb.ConnName = "",
    FIRST DBConfig NO-LOCK WHERE
          DBConfig.DBConfigId = ttDb.DbConfigId:
   ldaPrevDb = DbConfig.FromDate - 1.
END.
   
IF ldaPrevDb NE ? THEN DO:
   fInitializeConnectTables("MobCDR","old").
   RUN pDirectConnect2Dbs(Syst.CUICommon:gcBrand,
                          "old", 
                          ldaPrevDb,
                          ldaPrevDb).
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
      QUIT.
   END.
END.

RUN Mm/unbilled_calls.p(liFRProcessID,
                     liUpdateInterval,
                     INPUT liBillPeriod,
                     ldaPrevDb,
                     OUTPUT liCallCnt).

IF CONNECTED("oldmcdr") THEN 
      DISCONNECT oldmcdr NO-ERROR.

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pFinalizeFuncRunProcess(liFRProcessID,liCallCnt).

QUIT.

/******** Main end *******/


