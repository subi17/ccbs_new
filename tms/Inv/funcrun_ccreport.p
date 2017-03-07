/* ----------------------------------------------------------------------
  MODULE .......: funcrun_ccreport.p
  TASK .........: Print a cc report (function execution)
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
{Func/timestamp.i}
{Syst/funcrunprocess_run.i}
{Func/direct_dbconnect.i}

DEF VAR liInvCnt       AS INT  NO-UNDO.
DEF VAR liFRProcessID  AS INT  NO-UNDO.
DEF VAR liFRExecID     AS INT  NO-UNDO.
DEF VAR lcRunMode      AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR ldaInvDate     AS DATE NO-UNDO.
DEF VAR liInvType      AS INT  NO-UNDO.

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

/* connect to correct cdr dbs */
FIND FIRST Invoice USE-INDEX InvDate WHERE
           Invoice.Brand = gcBrand AND
           Invoice.InvDate = ldaInvDate AND
           Invoice.InvType = liInvType NO-LOCK NO-ERROR.
IF AVAILABLE Invoice THEN DO:
   fInitializeConnectTables("MobCDR","").
   RUN pDirectConnect2Dbs(gcBrand,
                          "",  
                          Invoice.ToDate,
                          Invoice.ToDate).
   IF RETURN-VALUE BEGINS "ERROR" THEN DO: 
      RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
      QUIT.
   END.   
END.
 
RUN Mm/ccreport.p (ldaInvDate,
                ldaInvDate,
                liInvType,
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


