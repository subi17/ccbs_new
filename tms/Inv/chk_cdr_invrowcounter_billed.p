{commali.i}
{funcrunprocess_update.i}
{timestamp.i}
{date.i}
{cparam2.i}
{istc.i}
{chk_billed_invrowcounter.i &ttReference = "REFERENCE-ONLY"}

DEF INPUT  PARAMETER TABLE FOR ttInvoice.
DEF INPUT  PARAMETER iiRun         AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdInterval AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiCounterQty  AS INT  NO-UNDO.

DEF VAR i AS INT NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR l AS INT NO-UNDO. 
DEF VAR lcMatch   AS CHAR NO-UNDO.
DEF VAR ldaToDate AS DATE NO-UNDO.
DEF VAR llHeader  AS LOG  NO-UNDO INIT TRUE.
DEF VAR lcDir     AS CHAR NO-UNDO.
DEF VAR ldaISTCDate AS DATE NO-UNDO. 
DEF VAR ldaISTCDateOld AS DATE NO-UNDO. 

DEF TEMP-TABLE ttCounter NO-UNDO
   LIKE InvRowCounter.

DEF STREAM sLog.


lcDir = fCParamC("ChkBilledIRCounterDir").
IF lcDir = "" OR lcDir = ? THEN lcDir = "/scratch/log/invrowcounter".

OUTPUT STREAM sLog TO VALUE(lcDir + 
                            "/chk_cdr_invrowcounter_billed_" + 
                            STRING(year(today),"9999") + 
                            STRING(month(today),"99") + 
                            STRING(day(today),"99") + "_" +
                            STRING(iiRun) + ".log") APPEND.

FOR EACH ttInvoice,
   FIRST Invoice NO-LOCK WHERE
         Invoice.InvNum = ttInvoice.InvNum,
    EACH SubInvoice OF Invoice NO-LOCK,
   FIRST InvSeq NO-LOCK WHERE
         InvSeq.InvSeq = SubInvoice.InvSeq
BY Invoice.InvNum:
         
   i = i + 1.
   ldaISTCDate = fGetiSTCDate(SubInvoice.MsSeq,
                              Invoice.Custnum,
                              Invoice.ToDate).

   EMPTY TEMP-TABLE ttCounter.
         
   FOR EACH MobCDR NO-LOCK USE-INDEX InvSeq WHERE
            MobCDR.InvCust = InvSeq.CustNum AND
            MobCDR.InvSeq = InvSeq.InvSeq:
      
      IF MobCDR.DateSt >= Invoice.FromDate THEN DO:
         IF ldaISTCDate NE ? AND ldaISTCDate > MobCDR.DateSt THEN
            ldaToDate = ldaISTCDate - 1.
         ELSE ldaToDate = Invoice.ToDate.
      END.
      ELSE DO:
         /* a bit slow to check every time, but these cases seem to be rare */
         ldaISTCDateOld = fGetiSTCDate(MobCDR.MsSeq,
                                       MobCDR.InvCust,
                                       MobCDR.DateSt).
         IF ldaISTCDateOld NE ? AND
            ldaISTCDateOld > MobCDR.DateSt
         THEN ldaToDate = ldaISTCDateOld - 1.
         ELSE ldaToDate = fLastDayOfMonth(MobCDR.DateSt).
      END.
      
      FIND FIRST ttCounter WHERE 
         ttCounter.InvCust     = MobCDR.InvCust AND
         ttCounter.InvSeq      = MobCDR.InvSeq AND
         ttCounter.BillCode    = MobCDR.BillCode AND
         ttCounter.CCN         = MobCDR.CCN AND
         ttCounter.MsSeq       = MobCDR.MsSeq AND
         ttCounter.CLI         = MobCDR.CLI AND
         ttCounter.TariffNum   = MobCDR.TariffNum AND
         ttCounter.VatIncl     = MobCDR.VatIncl AND
         ttCounter.ReportingID = "," AND
         ttCounter.DCEvent     = MobCDR.DCEvent AND
         ttCounter.ToDate      = ldaToDate NO-ERROR.
      IF NOT AVAILABLE ttCounter THEN DO:
         CREATE ttCounter.
         ASSIGN 
            ttCounter.InvCust     = MobCDR.InvCust
            ttCounter.InvSeq      = MobCDR.InvSeq 
            ttCounter.BillCode    = MobCDR.BillCode
            ttCounter.CCN         = MobCDR.CCN
            ttCounter.MsSeq       = MobCDR.MsSeq
            ttCounter.CLI         = MobCDR.CLI
            ttCounter.TariffNum   = MobCDR.TariffNum
            ttCounter.VatIncl     = MobCDR.VatIncl
            ttCounter.ReportingID = "," 
            ttCounter.DCEvent     = MobCDR.DCEvent
            ttCounter.ToDate      = ldaToDate.
      END.

      ASSIGN
         ttCounter.Quantity = ttCounter.Quantity + 1
         ttCounter.Duration = ttCounter.Duration + MobCDR.BillDur
         ttCounter.Amount   = ttCounter.Amount + MobCDR.Amount 
         ttCounter.DataAmt  = ttCounter.DataAmt + 
                                 MobCDR.DataIn + MobCDR.DataOut
         ttCounter.RefPrice = ttCounter.RefPrice + MobCDR.RefPrice.
   END.
   
   lcMatch = "".
   
   FOR EACH ttCounter
   BY ttCounter.BillCode
   BY ttCounter.CCN:
   
      oiCounterQty = oiCounterQty + 1.
      
      IF iiUpdInterval > 0 AND oiCounterQty MOD iiUpdInterval = 0 
      THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiCounterQty) THEN 
            RETURN "ERROR:Stopped".
      END.   
  
      FOR EACH InvRowCounter NO-LOCK WHERE 
         InvRowCounter.InvCust     = ttCounter.InvCust AND
         InvRowCounter.InvSeq      = ttCounter.InvSeq AND
         InvRowCounter.BillCode    = ttCounter.BillCode AND
         InvRowCounter.CCN         = ttCounter.CCN AND
         InvRowCounter.MsSeq       = ttCounter.MsSeq AND
         InvRowCounter.CLI         = ttCounter.CLI AND
         InvRowCounter.TariffNum   = ttCounter.TariffNum AND
         InvRowCounter.VatIncl     = ttCounter.VatIncl AND
         InvRowCounter.ReportingID = "," AND
         InvRowCounter.DCEvent     = ttCounter.DCEvent AND
         InvRowCounter.ToDate      = ttCounter.ToDate:
         
         ACCUMULATE InvRowCounter.InvCust (COUNT).
         ACCUMULATE InvRowCounter.Quantity (TOTAL).
         ACCUMULATE InvRowCounter.DataAmt (TOTAL).
         ACCUMULATE InvRowCounter.Duration (TOTAL).
         ACCUMULATE InvRowCounter.Amount (TOTAL).

      END.
      
      IF (ACCUM COUNT InvRowCounter.InvCust) EQ 0 THEN 
         lcMatch = "ERROR: No counter found: " + 
                   STRING(ttCounter.BillCode) + "/" +
                   STRING(ttCounter.CCN) + "/" +
                   STRING(ttCounter.Quantity) + "/" +
                   STRING(ttCounter.DataAmt / 1024 * 1024) + 
                   TRIM(STRING(ttCounter.Amount,"->>>>>9.99999")).
       
      ELSE IF 
         (ACCUM TOTAL InvRowCounter.Quantity) NE ttCounter.Quantity OR
         (ACCUM TOTAL InvRowCounter.Duration) NE ttCounter.Duration OR
         (ACCUM TOTAL InvRowCounter.Amount) NE ttCounter.Amount OR
         (ACCUM TOTAL InvRowCounter.DataAmt) NE ttCounter.DataAmt THEN 
            lcMatch = "ERROR: Values differ: " + 
                      STRING(ttCounter.BillCode) + "/" +
                      STRING(ttCounter.CCN).
      ELSE IF (ACCUM COUNT InvRowCounter.InvCust) NE 1 THEN ASSIGN
         lcMatch = "WARNING: Multiple counters found: " + 
                   STRING(ttCounter.billcode) + "/" +
                   STRING(ttCounter.CCN).
       
      IF lcMatch > "" THEN LEAVE.    
  
   END.        

   IF lcMatch = "" THEN 
   FOR EACH InvRowCounter NO-LOCK WHERE
            InvRowCounter.InvCust = InvSeq.CustNum AND
            InvRowCounter.InvSeq = InvSeq.InvSeq:

      IF InvRowCounter.Quantity = 0 AND InvRowCounter.Amount = 0 THEN NEXT.
          
      k = k + 1.
            
      FIND FIRST ttCounter WHERE 
         ttCounter.InvCust     = InvRowCounter.InvCust AND
         ttCounter.InvSeq      = InvRowCounter.InvSeq AND
         ttCounter.BillCode    = InvRowCounter.BillCode AND
         ttCounter.CCN         = InvRowCounter.CCN AND
         ttCounter.MsSeq       = InvRowCounter.MsSeq AND
         ttCounter.CLI         = InvRowCounter.CLI AND
         ttCounter.TariffNum   = InvRowCounter.TariffNum AND
         ttCounter.VatIncl     = InvRowCounter.VatIncl AND
         ttCounter.ReportingID = "," AND
         ttCounter.DCEvent     = InvRowCounter.DCEvent AND
         ttCounter.ToDate      = InvRowCounter.ToDate NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ttCounter THEN DO:
         lcMatch = "Counter without CDRs: " +
                   STRING(InvRowCounter.BillCode) + "/" +
                   STRING(InvRowCounter.CCN) + "/" +
                   STRING(InvRowCounter.ToDate,"99-99-99").
         LEAVE.
      END.
   END.   
         
   IF lcMatch > "" THEN DO:
   
      IF llHeader THEN DO:
         PUT STREAM sLog UNFORMATTED
           "Invoice"  CHR(9)
           "Customer"   CHR(9)
           "InvSeq"  CHR(9)
           "Period"  CHR(9)
           "MSISDN"  CHR(9)
           "MsSeq"   CHR(9)
           "Reason"  SKIP.
         llHeader = FALSE.
      END.
      
      l = l + 1.
      PUT STREAM sLog UNFORMATTED
         Invoice.InvNum CHR(9)
         InvSeq.CustNum CHR(9)
         InvSeq.InvSeq  CHR(9)
         InvSeq.ToDate  CHR(9)
         SubInvoice.CLI CHR(9)
         InvSeq.MsSeq CHR(9)
         lcMatch SKIP.
   END.
    
   IF NOT SESSION:BATCH THEN DO:
      IF i MOD 10 = 0 THEN DO:
         PAUSE 0.
         DISP Invoice.InvNum InvSeq.MsSeq i oiCounterQty k l WITH 1 DOWN.
      END.   
   END.    
END.   

OUTPUT STREAM sLog CLOSE.
      
IF NOT SESSION:BATCH THEN DO:
   DISP i oiCounterQty k l .
END.


