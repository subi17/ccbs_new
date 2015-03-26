/* ----------------------------------------------------------------------------
  MODULE .......: invrowcounter_billed_split.p
  FUNCTION .....: Split invoices for billed invrowcounter check
  CREATED ......: 19.12.12/aam
  --------------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{timestamp.i}
{funcrunprocess_update.i}
{date.i}
{tmsconst.i}
{funcrun_replica.i}

DEF INPUT  PARAMETER idaInvDate     AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvType      AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiBatchQty     AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID  AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRExecID     AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdInterval  AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUseReplica   AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiInvCnt       AS INT  NO-UNDO. 

DEF VAR liTotal       AS INT  NO-UNDO.
DEF VAR liCallQty     AS INT  NO-UNDO.
DEF VAR lcReplica     AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttInv NO-UNDO
   FIELD InvNum    AS INT
   FIELD InvCust  AS INT
   FIELD CallQty  AS INT
   INDEX CallQty CallQty DESC.


/***** Main start ********/

RUN pInitialize.
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

RUN pCollectInvoices.
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

RUN pDivideIntoGroups.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/****** Main end *****/


PROCEDURE pInitialize:

   IF iiBatchQty = 0 THEN DO:
      RETURN "ERROR:Nothing to do".
   END.

   IF iiUseReplica > 0 THEN DO:
      lcReplica = fInitReplicaSettings(iiUseReplica,
                                       INPUT-OUTPUT iiBatchQty).
      IF lcReplica BEGINS "ERROR" THEN RETURN lcReplica.
   END.
   
   RETURN "".   
 
END PROCEDURE.

PROCEDURE pCollectInvoices:

   DEF VAR liPeriod AS INT  NO-UNDO.
   DEF VAR ldaDate  AS DATE NO-UNDO.
   
   ASSIGN 
      ldaDate = fLastDayOfMonth(TODAY)
      liPeriod = YEAR(ldaDate) * 100 + MONTH(ldaDate).

   InvoiceSelect:
   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE 
            Invoice.Brand = gcBrand AND
            Invoice.InvDate = idaInvDate AND
            Invoice.InvType = iiInvType:
         
      liCallQty = 0.
      FOR EACH InvRow OF Invoice NO-LOCK WHERE
               InvRow.RowType = 2:
         liCallQty = liCallQty + InvRow.Qty.
      END.
      
      IF liCallQty = 0 THEN liCallQty = 1.

      CREATE ttInv.
      ASSIGN 
         ttInv.InvNum = Invoice.InvNum
         ttInv.InvCust = Invoice.CustNum
         ttInv.CallQty = liCallQty
         liTotal = liTotal + liCallQty.

      oiInvCnt = oiInvCnt + 1.
      IF NOT SESSION:BATCH AND oiInvCnt MOD 1000 = 0 THEN DO:
         PAUSE 0.
         DISP oiInvCnt COLUMN-LABEL "Invoice Qty" 
            WITH 1 DOWN ROW 8 CENTERED TITLE " Collecting " 
            OVERLAY FRAME fQty.
      END.

      IF iiUpdInterval > 0 AND oiInvCnt MOD iiUpdInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiInvCnt) THEN
            RETURN "ERROR:Stopped".
      END.   
   END.

   RETURN "".
   
END PROCEDURE.
 
PROCEDURE pDivideIntoGroups: 

   DEF VAR liLimit       AS INT  NO-UNDO.
   DEF VAR liFeedOrder   AS INT  NO-UNDO.
   DEF VAR lcProcessHost AS CHAR NO-UNDO.
   DEF VAR liReplQty     AS INT  NO-UNDO.
   DEF VAR liReplLimit   AS INT  NO-UNDO.
   DEF VAR liMainLimit   AS INT  NO-UNDO.
   DEF VAR liBatchCnt    AS INT  NO-UNDO.
   
   ASSIGN
      liMainLimit = liTotal / iiBatchQty
      liBatchCnt  = 0.

   fCalculateReplicaQty(iiUseReplica,
                        iiBatchQty,
                        liTotal,        
                        INPUT-OUTPUT liMainLimit,
                        OUTPUT liReplQty,
                        OUTPUT liReplLimit).
    
   ASSIGN
      liCallQty   = -1
      liLimit = liMainLimit.

   FOR EACH ttInv:
   
      IF liCallQty < 0 OR liCallQty >= liLimit THEN DO:

         ASSIGN 
            liCallQty   = 0
            liBatchCnt  = liBatchCnt + 1
            liFeedOrder = 0
            lcProcessHost = ""
            liLimit = liMainLimit.
            
         CASE iiUseReplica:
         /* partially to replica */
         WHEN 1 THEN DO:
            IF liBatchCnt <= liReplQty THEN ASSIGN
               lcProcessHost = lcReplica
               liLimit = liReplLimit.
         END.
         /* all to replica */
         WHEN 2 THEN lcProcessHost = lcReplica.
         END CASE. 
      END.
      
      liCallQty = liCallQty + ttInv.CallQty.
            
      DO TRANS:
         CREATE FuncRunResult.
         ASSIGN 
            FuncRunResult.FRProcessID = iiFRProcessID
            FuncRunResult.FRExecID    = iiFRExecID
            FuncRunResult.FRResultSeq = liBatchCnt
            FuncRunResult.IntParam    = ttInv.InvNum
            FuncRunResult.DecParam    = ttInv.InvCust
            liFeedOrder               = liFeedOrder + 1
            FuncRunResult.ResultOrder = liFeedOrder
            FuncRunResult.ProcessHost = lcProcessHost.
      END.
   END.

   RETURN "".

END PROCEDURE.   


