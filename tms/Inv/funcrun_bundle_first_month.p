/* ----------------------------------------------------------------------
  MODULE .......: funcrun_bundle_first_month.p 
  TASK .........: Calculate first month's fixed fee for bundles that
                  were terminated on the same month as activated (funcrun
                  execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 19.10.10
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

DEF VAR ldaFromDate   AS DATE NO-UNDO.
DEF VAR ldaToDate     AS DATE NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liFRExecID    AS INT  NO-UNDO.
DEF VAR lcRunMode     AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT NO-UNDO.
DEF VAR liAction      AS INT  NO-UNDO.
DEF VAR liSubsCount   AS INT  NO-UNDO. 
DEF VAR liTotalSubsCount AS INT  NO-UNDO.
DEF VAR lcTestFile    AS CHAR NO-UNDO.
DEF VAR llPickedCust  AS LOG  NO-UNDO.

DEF TEMP-TABLE ttCustomer NO-UNDO
    FIELD CustNum AS INT
    INDEX CustNum CustNum.
   
DEFINE STREAM sDump.

FUNCTION fCollectCustomer RETURNS LOG
   (iiCustNum AS INT):

   IF CAN-FIND(FIRST ttCustomer WHERE
      ttCustomer.CustNum = iiCustNum) THEN RETURN FALSE.

   FOR FIRST Customer NO-LOCK WHERE
             Customer.CustNum = iiCustNum:
      CREATE ttCustomer.
      ttCustomer.CustNum = iiCustNum.
   END.
    
   RETURN TRUE. 
END FUNCTION.


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
   ldaFromDate = fSetFuncRunDateParameter(1)
   ldaToDate   = fSetFuncRunDateParameter(2).
   lcTestFile = fSetFuncRunCharParameter(3).

IF ldaFromDate = ? OR ldaFromDate = ? OR ldaFromDate > ldaToDate THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   

llPickedCust = FALSE.

FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = liFRExecID NO-LOCK.
 
/* Calculate first month fee only for specified customers */
IF lcTestFile > "" THEN DO:
   RUN pCustomersFromFile.
   llPickedCust = TRUE.
END.
ELSE IF FuncRunExec.FeedFromExecSeq > 0 THEN DO:
   RUN pCustomersAsFeed.
   llPickedCust = TRUE.
END.

IF llPickedCust THEN DO:

   FOR EACH ttCustomer NO-LOCK:
      RUN Mm/bundle_first_month_fee.p(ldaFromDate,
                                   ldaToDate,
                                   ttCustomer.CustNum,
                                   liFRProcessID,
                                   liUpdateInterval,
                                   lcRunMode,
                                   OUTPUT liSubsCount).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         EMPTY TEMP-TABLE ttCustomer.
         RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
         QUIT.
      END. /* IF RETURN-VALUE BEGINS "ERROR" THEN DO: */

      /* If customer has DSS active then calculate Bundle fee */
      /* based on the DSS total consumption                   */
      RUN Mm/dss_bundle_first_month_fee.p(ldaFromDate,
                                       ldaToDate,
                                       ttCustomer.CustNum,
                                       liFRProcessID,
                                       liUpdateInterval,
                                       lcRunMode,
                                       OUTPUT liSubsCount).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         EMPTY TEMP-TABLE ttCustomer.
         RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
         QUIT.
      END. 

      liTotalSubsCount = liTotalSubsCount + liSubsCount.

   END. /* FOR EACH ttCustomer NO-LOCK: */
   
   EMPTY TEMP-TABLE ttCustomer.
END. 

ELSE DO:
   RUN Mm/bundle_first_month_fee.p(ldaFromDate,
                                ldaToDate,
                                0,
                                liFRProcessID,
                                liUpdateInterval,
                                lcRunMode,
                                OUTPUT liSubsCount).

   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
      QUIT.
   END. /* IF RETURN-VALUE BEGINS "ERROR" THEN DO: */

   liTotalSubsCount = liTotalSubsCount + liSubsCount.

   /* If customer has DSS active then calculate Bundle fee */
   /* based on the DSS total consumption                   */
   RUN Mm/dss_bundle_first_month_fee.p(ldaFromDate,
                                    ldaToDate,
                                    0,
                                    liFRProcessID,
                                    liUpdateInterval,
                                    lcRunMode,
                                    OUTPUT liSubsCount).

   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
      QUIT.
   END. /* IF RETURN-VALUE BEGINS "ERROR" THEN DO: */

   liTotalSubsCount = liTotalSubsCount + liSubsCount.

END. /* ELSE DO: */

RUN pFinalizeFuncRunProcess(liFRProcessID,liTotalSubsCount).

QUIT.

/******** Main end *******/


PROCEDURE pCustomersFromFile:

   DEF VAR lcCustNum   AS CHAR NO-UNDO.
   DEF VAR liCustNum   AS INT  NO-UNDO.
   
   FILE-INFO:FILE-NAME = lcTestFile.
   IF NOT FILE-INFO:FILE-TYPE BEGINS "F" THEN
      RETURN "ERROR:File containing test customer numbers not found".
      
   INPUT STREAM sDump FROM VALUE(lcTestFile).
   
   REPEAT:
   
      IMPORT STREAM sDump lcCustNum.
      IF lcCustNum = "" THEN NEXT.
      
      liCustNum = INT(lcCustNum) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.

      fCollectCustomer(liCustNum).
   END.

   INPUT STREAM sDump CLOSE.

   RETURN "".

END PROCEDURE.

PROCEDURE pCustomersAsFeed:

   FOR EACH FuncRunResult NO-LOCK WHERE
            FuncRunResult.FRExecID = FuncRunExec.FeedFromExecSeq:
      fCollectCustomer(FuncRunResult.IntParam).
   END.

END PROCEDURE.
