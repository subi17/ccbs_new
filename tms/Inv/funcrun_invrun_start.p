/* ----------------------------------------------------------------------
  MODULE .......: FUNCRUN_INVRUN_START.P
  TASK .........: Billing run from customer split files (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam (from invrun_screen.p)
  CREATED ......: 22.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

DISABLE TRIGGERS FOR LOAD OF FixedFee.
DISABLE TRIGGERS FOR LOAD OF SingleFee.

{commpaa.i}
{cparam2.i}
{timestamp.i}
{billrund.i NEW}
{funcrunprocess_run.i}
{funcrunprocess_update.i}

DEF VAR liCustQty    AS INT  NO-UNDO.
DEF VAR lhHandle     AS HANDLE NO-UNDO.
DEF VAR liQty        AS INT  NO-UNDO.
DEF VAR ldeAmt       AS DEC  NO-UNDO.
DEF VAR ldeVatAmt    AS DEC  NO-UNDO.
DEF VAR ldaInvDate   AS DATE NO-UNDO.
DEF VAR ldaEBADueDate AS DATE NO-UNDO.
DEF VAR ldaIberPayDueDate AS DATE NO-UNDO.
DEF VAR ldaFusionDueDate AS DATE NO-UNDO.
DEF VAR ldaDateFrom  AS DATE NO-UNDO.
DEF VAR ldaDateTo    AS DATE NO-UNDO.
DEF VAR liFeePeriod  AS INT  NO-UNDO.
DEF VAR lcBillRun    AS CHAR NO-UNDO.
DEF VAR lcMessage    AS CHAR NO-UNDO.
DEF VAR ldeBegTime   AS DEC  NO-UNDO.
DEF VAR ldeEndTime   AS DEC  NO-UNDO. 
DEF VAR liDurDays    AS INT  NO-UNDO.
DEF VAR liDurTime    AS INT  NO-UNDO.
DEF VAR liInvType    AS INT  NO-UNDO.
DEF VAR liCallQty    AS INT  NO-UNDO.

DEF VAR liFRProcessID    AS INT  NO-UNDO.
DEF VAR liFRExecID       AS INT  NO-UNDO.
DEF VAR lcRunMode        AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT  NO-UNDO.
DEF VAR lcMsSeqList      AS CHAR NO-UNDO.

/******** Main start ***********/

RUN pInitialize.
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pCreateInvoices.

RUN pFinalize (RETURN-VALUE).

QUIT.

/******** Main end ********/

PROCEDURE pInitialize:

   ASSIGN
      gcBrand      = '1'
      katun        = 'cron'
      ldeBegTime   = fMakeTS()
      ldaEBADueDate = ?
      ldaIberPayDueDate = ?.

   RUN pInitializeFuncRunProcess(OUTPUT liFRProcessID,
                                 OUTPUT liFRExecID,    
                                 OUTPUT lcRunMode,
                                 OUTPUT liUpdateInterval).
   IF RETURN-VALUE BEGINS "ERROR" THEN 
      RETURN RETURN-VALUE.                        

   RUN pGetFuncRunProcessParameters(liFRProcessID).

   ASSIGN 
      ldaInvDate  = fSetFuncRunDateParameter(1)
      ldaIberPayDueDate = fSetFuncRunDateParameter(2)
      ldaDateFrom = fSetFuncRunDateParameter(3)
      ldaDateTo   = fSetFuncRunDateParameter(4)
      liFeePeriod = fSetFuncRunIntParameter(5)
      liInvType   = fSetFuncRunIntParameter(6)
      ldaFusionDueDate = fSetFuncRunDateParameter(7)
      ldaEBADueDate = fSetFuncRunDateParameter(8).

   IF ldaDateFrom = ? OR ldaDateTo = ? OR ldaInvDate = ? OR 
      liFeePeriod = ? OR liInvType = ? THEN 
      RETURN "ERROR:Invalid parameters".

   IF ldaIberPayDueDate NE ? THEN DO:
      IF ldaIberPayDueDate < ldaInvDate THEN 
         RETURN "ERROR:Due date cannot be earlier than invoice date".
            
      IF ldaIberPayDueDate > ldaInvDate + 60 THEN 
          RETURN "ERROR:Due date is too far in the future".
   END.

   IF ldaEBADueDate NE ? THEN DO:
      IF ldaEBADueDate < ldaInvDate THEN 
         RETURN "ERROR:Due date cannot be earlier than invoice date".
            
      IF ldaEBADueDate > ldaInvDate + 60 THEN 
          RETURN "ERROR:Due date is too far in the future".
   END.

   IF ldaFusionDueDate NE ? THEN DO:
      IF ldaFusionDueDate < ldaInvDate THEN 
         RETURN "ERROR:DiFusion due date cannot be earlier than invoice date".
            
      IF ldaFusionDueDate > ldaInvDate + 60 THEN 
          RETURN "ERROR:Fusion due date is too far in the future".
   END.
   
   FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = liFRProcessID 
      NO-LOCK.
   FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = liFRExecID 
      NO-LOCK.
   
   FOR EACH FuncRunResult NO-LOCK WHERE
            FuncRunResult.FRExecID    = FuncRunExec.FeedFromExecSeq AND
            FuncRunResult.FRResultSeq = FuncRunProcess.ProcSeq:

      FIND FIRST Customer WHERE
                 Customer.Brand   = gcBrand AND 
                 Customer.CustNum = FuncRunResult.IntParam
      NO-LOCK NO-ERROR.
      
      FIND FIRST InvGroup OF Customer NO-LOCK.
      
      CREATE ttInvCust.
      ASSIGN
         ttInvCust.CustNr  = Customer.CustNum
         ttInvCust.MinInv  = InvGroup.MinInvAmt
         ttInvCust.CallQty = FuncRunResult.DecParam
         ttInvCust.LowVal  = FALSE
         liCustQty         = liCustQty + 1.
         
      IF lcRunMode = "test" AND FuncRunResult.CharParam BEGINS "MsSeq:" THEN 
         lcMsSeqList = lcMsSeqList + (IF lcMsSeqList > "" THEN "," ELSE "") +
                       ENTRY(2,FuncRunResult.CharParam,":").
   END.

   RUN lamupers.p PERSISTENT SET lhHandle.

END PROCEDURE.
   
PROCEDURE pCreateInvoices:   
   
   IF liInvType = 99 THEN lcBillRun = "TEST-BR".
   ELSE lcBillRun = "BR".

   /* use sequence InvFile to create a unique id for this run */
   lcBillRun = lcBillRun + 
               (IF lcRunMode = "test" 
                THEN "TEST" + STRING(liFRProcessID)
                ELSE STRING(NEXT-VALUE(InvFile))) + "_" + 
               STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
               STRING(DAY(TODAY),"99") + STRING(TIME,"99999").
               
   /* make an entry to db that run has started */            
   DO TRANS:

      CREATE ActionLog.
   
      ASSIGN
         ActionLog.ActionTS     = fMakeTS()
         ActionLog.Brand        = gcBrand
         ActionLog.TableName    = "Invoice"
         ActionLog.KeyValue     = lcBillRun
         ActionLog.UserCode     = katun
         ActionLog.ActionID     = "BillRun"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.FromDate     = ldaDateFrom
         ActionLog.ToDate       = ldaDateTo
         ActionLog.ActionStatus = 0
         ActionLog.ActionChar   = "FR Process: " + STRING(liFRProcessID).
   END.

   RUN pFunctionQueueRun IN lhHandle(ldaInvDate,
                                     ldaDateFrom,
                                     ldaDateTo,
                                     liFeePeriod,
                                     ldaIberPayDueDate,
                                     ldaEBADueDate,
                                     ldaFusionDueDate,
                                     TRUE,
                                     TRUE,
                                     liCustQty,
                                     liInvType,
                                     "*" + lcBillRun,
                                     FALSE,
                                     liFRProcessID,
                                     liUpdateInterval,
                                     lcMsSeqList).

   RETURN RETURN-VALUE.
   
END PROCEDURE.


PROCEDURE pFinalize:

   DEF INPUT PARAMETER icProcessError AS CHAR NO-UNDO.
    

   ldeEndTime = fMakeTS().

   liDurDays = DYNAMIC-FUNCTION("fTSDuration" IN ghFunc1,
                                ldeBegTime,
                                ldeEndTime,
                                OUTPUT liDurTime).

   RUN pGetAmt IN lhHandle (OUTPUT liQty,
                            OUTPUT ldeAmt,
                            OUTPUT ldeVatAmt).

   RUN pUpdInvGroup in lhHandle.
   
   lcMessage = 
      STRING(liQty) +  
      " invoices were created." + CHR(10) + 
      "Duration for creation was " +
      (IF liDurDays > 0 
       THEN STRING(liDurDays) + " days and "
       ELSE "") +
      STRING(liDurTime,"hh:mm:ss") + CHR(10) + 
      "Total amount excluding VAT was " + 
      TRIM(STRING(ldeAmt,"->>>>>>>>9.99"))  + CHR(10) +
      "Total payable amount, including VAT and reductions was " +
      TRIM(STRING(ldeVatAmt,"->>>>>>>>9.99")) + CHR(10) +
      "FR process: " + STRING(liFRProcessID) + 
         " (" + TRIM(lcRunMode) + ")".

   /* mark run as finished */
   DO TRANS:

      FIND FIRST ActionLog WHERE
                 ActionLog.Brand        = gcBrand   AND
                 ActionLog.TableName    = "Invoice" AND
                 ActionLog.KeyValue     = lcBillRun AND
                 ActionLog.ActionID     = "BillRun" AND
                 ActionLog.ActionStatus = 0 NO-LOCK NO-ERROR.
                 
      IF NOT AVAILABLE ActionLog THEN DO:
         CREATE ActionLog.
   
         ASSIGN
            ActionLog.ActionTS     = fMakeTS()
            ActionLog.Brand        = gcBrand
            ActionLog.TableName    = "Invoice"
            ActionLog.KeyValue     = lcBillRun
            ActionLog.UserCode     = katun
            ActionLog.ActionID     = "BillRun"
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                     MONTH(TODAY)
            ActionLog.FromDate     = ldaDateFrom
            ActionLog.ToDate       = ldaDateTo.
      END.
      
      ELSE FIND Current ActionLog EXCLUSIVE-LOCK.
      
      ASSIGN
         ActionLog.ActionDec    = liQty
         ActionLog.ActionChar   = lcMessage
         ActionLog.ActionStatus = IF icProcessError BEGINS "ERROR" 
                                  THEN 5
                                  ELSE 2.
   END.

   IF icProcessError BEGINS "ERROR" THEN 
      RUN pCancelFuncRunProcess(liFRProcessID,icProcessError).
   ELSE RUN pFinalizeFuncRunProcess(liFRProcessID,liQty).

   IF VALID-HANDLE(lhHandle) THEN DELETE OBJECT lhHandle NO-ERROR.

END PROCEDURE.


