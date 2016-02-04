/* -----------------------------------------------
  MODULE .......: NNPCST.P
  FUNCTION .....: release events from invoice
  APPLICATION ..: nn
  AUTHOR .......: kl
  CREATED ......: 24.03.98
  MODIFIED .....: 14.02.07/aam yoigo version
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/invseq.i}
{Mf/errors.i}
{Func/fcustbal.i}
{Ar/nnpcst.i}
{Syst/tmsconst.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:

   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).

END.

DEF INPUT PARAMETER iiInvNum LIKE Invoice.InvNum.
DEF INPUT PARAMETER iiSubInv AS INT NO-UNDO. 
DEF INPUT PARAMETER ilRecalcCounter AS LOG NO-UNDO.

DEF INPUT PARAMETER TABLE FOR wMarked. 


DEF VAR lInvSeq  AS i   NO-UNDO.
DEF VAR lPartial AS lo  NO-UNDO. 
DEF VAR lInter   AS lo  NO-UNDO.
DEF VAR ldIntAmt AS DEC NO-UNDO.
DEF VAR ldOriginalAmt AS DEC NO-UNDO.
DEF VAR liCurrentPeriod AS INT NO-UNDO. 
DEF VAR liEvents AS INT NO-UNDO. 

DEF BUFFER xInvSeq    FOR InvSeq.
DEF BUFFER xcall      FOR FixCDR. 
DEF BUFFER xMobCDR    FOR MobCDR.  
DEF BUFFER bEventCust FOR Customer.
DEF BUFFER bFatime    FOR Fatime.
DEF BUFFER bSubInv    FOR SubInvoice.
DEF BUFFER bMobSub    FOR MobSub.

DEF TEMP-TABLE ttSeq NO-UNDO
   FIELD InvSeq AS INT.

DEF TEMP-TABLE ttReCalc NO-UNDO
   FIELD InvSeq AS INT.
   
DEF BUFFER bFFItem FOR FFItem.


ASSIGN lPartial = (CAN-FIND (FIRST wMarked))
       lInter   = (CAN-FIND (FIRST wMarked WHERE wMarked.Line = -1)).
IF NOT lPartial THEN ASSIGN 
       lInter   = TRUE.

FIND FIRST Invoice WHERE
           Invoice.InvNum = iiInvNum
NO-LOCK NO-ERROR.

IF iiSubInv > 0 THEN DO:
   FIND SubInvoice WHERE
        SubInvoice.InvNum    = Invoice.InvNum AND
        SubInvoice.SubInvNum = iiSubInv NO-LOCK.

   IF AVAIL SubInvoice THEN
      FIND FIRST bMobSub WHERE
                 bMobSub.MsSeq = SubInvoice.MsSeq NO-LOCK NO-ERROR.
END.
        
/* this FIRST => smaller TRANSACTION */
FOR EACH InvRow of Invoice EXCLUSIVE-LOCK.

   IF iiSubInv > 0 AND InvRow.SubInvNum NE iiSubInv THEN NEXT. 
   
   /* IF partial Credit THEN check IF line is marked */
   IF lPartial AND 
      NOT can-find(FIRST wMarked WHERE 
                         wMarked.line = integer(recid(InvRow)))
   THEN NEXT. 

   InvRow.InvRowNum = 0.

END.

liCurrentPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).

/* unmark ALL contract fee rows */
FOR EACH FFItem EXCLUSIVE-LOCK where
         FFItem.InvNum  = Invoice.InvNum:

   IF iiSubInv > 0 AND FFItem.SubInvNum NE iiSubInv THEN NEXT. 
   
   /* partial crediting */
   IF lPartial THEN DO:
      /* contracts are grouped BY BillCode code */
      FIND FIRST InvRow of Invoice NO-LOCK WHERE
                 InvRow.SubInvNum = FFItem.SubInvNum AND
                 InvRow.BillCode  = FFItem.BillCode  AND
                 InvRow.RowType   = 3 NO-ERROR.

      IF NOT AVAILABLE InvRow OR
         NOT can-find(FIRST wMarked WHERE 
                            wMarked.line = integer(recid(InvRow)))         
      THEN NEXT. 
   END. 
         
   ASSIGN
      FFItem.Billed    = FALSE
      FFItem.InvNum    = 0
      FFItem.SubInvNum = 0
      FFItem.BillCode  = "PAYTERM" WHEN 
         LOOKUP(FFItem.BillCode,{&TF_BANK_ALL_ACTIVE_PAYTERM_BILLCODES}) > 0
      FFItem.BillCode  = "RVTERM" WHEN 
         LOOKUP(FFItem.BillCode,{&TF_BANK_ALL_ACTIVE_RVTERM_BILLCODES}) > 0.

   /* reassign the original amount for first month fee */
   IF FFItem.BillPeriod >= liCurrentPeriod THEN DO:
      FIND FIRST FixedFee OF FFItem NO-LOCK NO-ERROR.
      IF AVAILABLE FixedFee AND  
         FixedFee.ServiceLimitGroup BEGINS "PMF:" AND
         NOT CAN-FIND(FIRST bFFItem OF FixedFee WHERE
                            bFFItem.BillPeriod < FFItem.BillPeriod) AND
         AVAIL bMobSub
      THEN DO:
         FIND CURRENT FixedFee EXCLUSIVE-LOCK.
         ASSIGN 
            FFItem.Amt = (IF FixedFee.CalcAmt <> ? THEN FixedFee.CalcAmt
                          ELSE FixedFee.Amt)
            FixedFee.ServiceLimitGroup = "".
      END.
   END.
         
END.

/* unmark ALL single fee rows */
FOR EACH SingleFee EXCLUSIVE-LOCK where
         SingleFee.InvNum  = Invoice.InvNum:

   IF iiSubInv > 0 AND SingleFee.SubInvNum NE iiSubInv THEN NEXT. 

   /* partial crediting */
   IF lPartial THEN DO:
      FIND FIRST InvRow of Invoice NO-LOCK WHERE
         InvRow.FFItemNum  = SingleFee.FMItemId AND
         InvRow.RowType    = 4 NO-ERROR.

      /* bitem-lines may be combined by product */
      IF NOT AVAILABLE InvRow THEN 
      FIND FIRST InvRow of Invoice no-lock where
                 InvRow.SubInvNum = SingleFee.SubInvNum AND   
                 InvRow.BillCode  = SingleFee.BillCode  AND
                 InvRow.RowType   = 4 no-error.

      IF NOT AVAILABLE InvRow OR
         NOT can-find(FIRST wMarked WHERE 
                            wMarked.line = integer(recid(InvRow)))         
      THEN NEXT. 
   END. 

   /* created in the billing run */
   IF SingleFee.CalcObj > "" AND SingleFee.CalcObj = Invoice.BillRun THEN DO:
      IF llDoEvent AND Invoice.InvType NE {&INV_TYPE_TEST} THEN
         RUN StarEventMakeDeleteEventWithMemo(lhSingleFee,
                                              katun,
                                              "InvoiceDeletion").
      DELETE SingleFee.
   END. 
   
   ELSE DO:
      IF llDoEvent AND Invoice.InvType NE {&INV_TYPE_TEST} THEN
         RUN StarEventSetOldBuffer(lhSingleFee).
      
      ASSIGN
         SingleFee.Billed    = FALSE
         SingleFee.InvNum    = 0
         SingleFee.SubInvNum = 0.
      IF SingleFee.SourceTable EQ "FixedFee" THEN DO:
         IF LOOKUP(SingleFee.BillCode,
                   {&TF_BANK_ALL_CLOSED_PAYTERM_BILLCODES}) > 0 THEN
            SingleFee.BillCode  = "PAYTERMEND".
         ELSE IF LOOKUP(SingleFee.BillCode,
                   {&TF_BANK_ALL_CLOSED_RVTERM_BILLCODES}) > 0 THEN
            SingleFee.BillCode  = "RVTERMEND".
      END.
      
      IF llDoEvent AND Invoice.InvType NE {&INV_TYPE_TEST} THEN
         RUN StarEventMakeModifyEventWithMemo(
            lhSingleFee,
            katun,
            "InvoiceDeletion").
   END.

END.


/* unmark ALL Fatime fee rows */
FOR EACH Fatime EXCLUSIVE-LOCK WHERE
         Fatime.InvNum  = Invoice.InvNum
BY FATime.FATNum DESC:

   IF iiSubInv > 0 AND Fatime.SubInvNum NE iiSubInv THEN NEXT. 

   /* partial crediting */
   IF lPartial THEN DO:
      FIND FIRST FatGroup OF FATime NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FatGroup THEN NEXT. 
      
      FIND FIRST InvRow of Invoice NO-LOCK WHERE
                 InvRow.SubInvNum = Fatime.SubInvNum AND     
                 InvRow.BillCode  = FatGroup.BillCode AND
                 InvRow.RowType   = 7 NO-ERROR.

      IF NOT AVAILABLE InvRow OR
         NOT can-find(FIRST wMarked WHERE 
                            wMarked.line = integer(recid(InvRow)))         
      THEN NEXT. 
   END. 

   ASSIGN
      Fatime.invnum    = 0
      Fatime.SubInvNum = 0
      Fatime.used      = 0.

   /* transferred rows */
   FOR EACH bFatime USE-INDEX OrigFat WHERE
            bFatime.Brand   = gcBrand        AND
            bFatime.OrigFat = FATime.FatNum  AND
            bFATime.TransQty = 0             AND 
            (bFATime.InvNum = 0 OR
             bFATime.InvNum = Invoice.InvNum) EXCLUSIVE-LOCK:

      Fatime.TransQty = MAX(0,Fatime.TransQty - bFatime.Amt).
      
      DELETE bFatime.
   END.
                               
END.

/* overtime interests */
IF lInter THEN DO:

   FOR EACH CustIntEvent EXCLUSIVE-LOCK WHERE
            CustIntEvent.BilledInvNum = Invoice.InvNum:

      IF iiSubInv > 0 AND CustIntEvent.BilledSubInv NE iiSubInv THEN NEXT.

      IF lPartial AND 
          NOT can-find(FIRST wMarked WHERE 
                             wMarked.line   = -1 AND
                             wMarked.SubInv = CustIntEvent.BilledSubInv)
      THEN NEXT. 
      
      ASSIGN CustIntEvent.BilledInvNum = 0
             CustIntEvent.BilledSubInv = 0
             ldIntAmt                  = ldIntAmt + CustIntEvent.Amt.

      /* update customer's balance */
      fCustBal(Invoice.CustNum,
               CustIntEvent.CLI,
               "INT",
               CustIntEvent.Amt).
   END.            
END.

/* minimum consumption can be billed again */

FOR EACH SubInvoice OF Invoice NO-LOCK:

   IF iiSubInv > 0 AND SubInvoice.SubInvNum NE iiSubInv THEN NEXT.
   
   FOR FIRST MinConsumption EXCLUSIVE-LOCK WHERE
             MinConsumption.MsSeq  = SubInvoice.MsSeq AND
             MinConsumption.InvNum = Invoice.InvNum:
      DELETE MinConsumption.
   END.
 
   FOR FIRST ActionLog EXCLUSIVE-LOCK WHERE
             ActionLog.Brand        = gcBrand               AND
             ActionLog.TableName    = "MobSub"              AND
             ActionLog.KeyValue     = STRING(SubInvoice.MsSeq) AND
             ActionLog.ActionID     = "MINCONS"             AND
             ActionLog.ActionPeriod = YEAR(Invoice.ToDate) * 100 + 
                                      MONTH(Invoice.ToDate) AND
             ActionLog.ActionDec    = Invoice.InvNum:
      ActionLog.ActionStatus = 5.
   END.                                 
END.

/* mark invoice sequence AS NOT Billed */

/* when partial crediting THEN Transfer the Calls TO a NEW InvSeq */
IF lPartial THEN DO:

   EMPTY TEMP-TABLE ttReCalc.

   FOR EACH wMarked,
      FIRST InvRow NO-LOCK WHERE
            recid(InvRow) = wMarked.Line AND
            InvRow.RowType = 2,
      FIRST bSubInv NO-LOCK WHERE
            bSubInv.InvNum    = InvRow.InvNum AND
            bSubInv.SubInvNum = InvRow.SubInvNum:

      lInvSeq = fNewInvseq(Invoice.AgrCust,
                           Invoice.CustNum,
                           bSubInv.MsSeq,
                           InvRow.ToDate,
                           0).

      IF NOT CAN-FIND(FIRST ttReCalc WHERE ttReCalc.InvSeq = lInvSeq) THEN DO:
         CREATE ttReCalc.
         ttReCalc.InvSeq = lInvSeq.
      END.   
      
      /* mobile Calls */
      FOR EACH MobCDR NO-LOCK WHERE
               MobCDR.InvCust = Invoice.CustNum AND
               MobCDR.InvSeq  = bSubInv.InvSeq:

         IF MobCDR.DateSt  >= InvRow.FromDate AND
            MobCDR.DateSt  <= InvRow.ToDate   AND
            MobCDR.BillCode = InvRow.BillCode
         THEN DO:
            FIND FIRST xMobCDR where recid(xMobCDR) = recid(MobCDR) 
               exclusive-lock.
            ASSIGN xMobCDR.InvSeq = lInvSeq.
         END.
      END.
   END. 

END.

/* total Credit */
ELSE DO TRANS:

   
   IF iiSubInv > 0 THEN DO:
      FIND FIRST SubInvoice WHERE
           SubInvoice.InvNum    = Invoice.InvNum AND
           SubInvoice.SubInvNum = iiSubInv NO-LOCK.
      CREATE ttSeq.
      ttSeq.InvSeq = SubInvoice.InvSeq.
   END.
   ELSE FOR EACH bSubInv OF Invoice NO-LOCK:
      CREATE ttSeq.
      ttSeq.InvSeq = bSubInv.InvSeq.
   END.

   EMPTY TEMP-TABLE ttReCalc.
   
   FOR EACH ttSeq,
      FIRST xInvSeq EXCLUSIVE-LOCK WHERE
            xInvSeq.InvSeq = ttSeq.InvSeq:

      IF (ilRecalcCounter AND Invoice.InvType NE {&INV_TYPE_TEST}) OR
         YEAR(xInvSeq.FromDate) * 100 + MONTH(xInvSeq.FromDate) NE
         YEAR(xInvSeq.ToDate)   * 100 + MONTH(xInvSeq.ToDate)  
      THEN DO:    
         CREATE ttReCalc.
         ttReCalc.InvSeq = xInvSeq.InvSeq.
      END.
      
      ASSIGN 
         xInvSeq.Billed  = FALSE
         xInvSeq.InvNum  = 0
         xInvSeq.SubInvNum = 0.

      FOR EACH InvRowCounter EXCLUSIVE-LOCK WHERE
               InvRowCounter.InvCust = xInvSeq.CustNum AND
               InvRowCounter.InvSeq  = xInvSeq.InvSeq:
         ASSIGN 
            InvRowCounter.InvNum = 0
            InvRowCounter.SubInvNum = 0.
      END.
       
      /* IF xInvSeq contains Calls from several months */
      IF year(xInvSeq.FromDate) * 100 + month(xInvSeq.FromDate) NE 
         year(xInvSeq.ToDate)   * 100 + month(xInvSeq.ToDate)   THEN DO:

         xInvSeq.FromDate = date(month(xInvSeq.ToDate),1,year(xInvSeq.ToDate)).

         /* ALL Calls prior TO latest MONTH */
         FOR EACH MobCDR  NO-LOCK WHERE
                  MobCDR.InvCust = xInvSeq.CustNum AND
                  MobCDR.InvSeq  = xInvSeq.InvSeq  AND
                  MobCDR.DateSt  < xInvSeq.FromDate:

            /* get invoice sequence FOR CDR */
            FIND FIRST InvSeq WHERE 
                       InvSeq.CustNum   = MobCDR.InvCust  AND 
                       InvSeq.MsSeq     = MobCDR.MsSeq    AND 
                       InvSeq.FromDate <= MobCDR.DateSt   AND 
                       InvSeq.ToDate   >= MobCDR.DateSt   AND
                       InvSeq.Billed    = FALSE           AND
                       InvSeq.AgrCust   = xInvSeq.AgrCust 
            NO-LOCK NO-ERROR. 

            /* CREATE monthly based invoice sequences */
            IF AVAIL InvSeq THEN lInvSeq = InvSeq.InvSeq. 
            ELSE lInvSeq = fNewInvseq(xInvSeq.AgrCust,
                                      MobCDR.InvCust, 
                                      MobCDR.MsSeq,
                                      MobCDR.DateSt,
                                      0).

            /* UPDATE InvSeq FOR Calls */
            IF MobCDR.InvSeq NE lInvSeq THEN DO:
               FIND FIRST xMobCDR WHERE
                    recid(xMobCDR) = recid(MobCDR) 
               EXCLUSIVE-LOCK.
               ASSIGN xMobCDR.InvSeq = lInvSeq.
            END.

            IF lInvSeq NE xInvSeq.InvSeq AND
               NOT CAN-FIND(FIRST ttReCalc WHERE ttReCalc.InvSeq = lInvSeq)
            THEN DO:
               CREATE ttReCalc.
               ttReCalc.InvSeq = lInvSeq.
            END.
         END.
         
      END.   
      
   END.

END.
 
FOR EACH ttReCalc,
   FIRST xInvSeq NO-LOCK WHERE
         xInvSeq.InvSeq = ttReCalc.InvSeq:
            
   /* recalculate InvRowCounters from scratch, because tax handling methods
      (included/excluded) may have been converted in billing run according
      to the customer's VatIncl value -> now counters need to be in their 
      original tax status again */
   RUN Inv/recalculate_invrowcounter.p(Invoice.CustNum,
                                   0,
                                   xInvSeq.InvSeq,       
                                   xInvSeq.FromDate,
                                   xInvSeq.ToDate,
                                   OUTPUT liEvents).
END.

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.
