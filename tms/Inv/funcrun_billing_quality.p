/* ----------------------------------------------------------------------
  MODULE .......: funcrun_billing_quality.p
  TASK .........: Print a billing quality report (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 06.07.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{Func/cparam2.i}
{Func/files.i}
{Func/coinv.i}
{Func/timestamp.i}
{Syst/funcrunprocess_run.i}
{Func/direct_dbconnect.i}

DEF VAR liInvCnt       AS INT  NO-UNDO.
DEF VAR liSubCnt       AS INT  NO-UNDO.
DEF VAR liFRProcessID  AS INT  NO-UNDO.
DEF VAR liFRExecID     AS INT  NO-UNDO.
DEF VAR lcRunMode      AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR ldaInvDate     AS DATE NO-UNDO.
DEF VAR liInvType      AS INT  NO-UNDO.
DEF VAR llBillDetails  AS LOG  NO-UNDO.
DEF VAR liPeriod       AS INT  NO-UNDO.
DEF VAR ldaFromDate    AS DATE NO-UNDO.
DEF VAR ldaToDate      AS DATE NO-UNDO.
DEF VAR llSubDetails   AS LOG  NO-UNDO.
DEF VAR lcFile         AS CHAR NO-UNDO.
DEF VAR lcTransDir     AS CHAR NO-UNDO.
DEF VAR llSubReport    AS LOG  NO-UNDO.
DEF VAR ldaEndPeriod   AS DATE NO-UNDO.

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
   ldaInvDate    = fSetFuncRunDateParameter(1)
   liInvType     = fSetFuncRunIntParameter(2)
   llBillDetails = fSetFuncRunLogParameter(3)

   liPeriod      = fSetFuncRunIntParameter(4)
   ldaFromDate   = fSetFuncRunDateParameter(5)
   ldaToDate     = fSetFuncRunDateParameter(6)
   llSubDetails  = fSetFuncRunLogParameter(7)
   llSubReport   = fSetFuncRunLogParameter(8).
   
IF ldaInvDate = ? OR liInvType = ? OR liPeriod = 0 OR 
   ldaFromDate = ? OR ldaToDate = ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

lcFile = fCParamC("BillQualityFileName").
IF lcRunMode = "test" THEN 
   lcTransDir = fCParamC("FRTestRunDir").
ELSE lcTransDir = fCParamC("BillQualityTransDir").

IF lcFile = ? OR lcFile = "" THEN DO:   
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid file name").
   QUIT.
END.   
IF lcTransDir = ? THEN lcTransDir = "".

/* format file name here, so that both reports get the same name */
ASSIGN   
   lcFile = REPLACE(lcFile,"#PERIOD",STRING(liPeriod,"999999"))
   lcFile = REPLACE(lcFile,"#INVDATE",STRING(ldaInvDate,"999999"))
   lcFile = REPLACE(lcFile,"#MODE",STRING(llBillDetails,"d/s") +
                                   (IF llSubReport 
                                    THEN STRING(llSubDetails,"d/s")
                                    ELSE "n")).

RUN billing_report.p (ldaInvDate,
                      liInvType,
                      (IF llSubReport 
                       THEN "no*no**" 
                       ELSE "no*trans*" + lcTransDir + "*") + lcFile,
                      llBillDetails,
                      liFRProcessID,
                      liUpdateInterval,
                      lcRunMode,
                      OUTPUT liInvCnt).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.   

IF llSubReport THEN DO:

   ldaEndPeriod = fInt2Date(liPeriod,2).
   
   /* connect to correct cdr dbs */
   fInitializeConnectTables("MobCDR","").
   RUN pDirectConnect2Dbs(gcBrand,
                          "",  
                          ldaEndPeriod,
                          ldaEndPeriod).
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
      QUIT.
   END.   

   RUN unbilled_subsqty.p (liPeriod,
                           ldaFromDate,
                           ldaToDate,
                           "append*trans*" + lcTransDir + "*" + lcFile,
                           llSubDetails,
                           liFRProcessID,
                           liUpdateInterval,
                           lcRunMode,
                           OUTPUT liSubCnt).
 
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
      QUIT.
   END.   
END.

RUN pFinalizeFuncRunProcess(liFRProcessID,liInvCnt).

QUIT.

/******** Main end *******/


