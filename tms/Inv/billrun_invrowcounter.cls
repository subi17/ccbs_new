
CLASS billrun_invrowcounter:

   {lamupers_temptable.i &ttReference = "REFERENCE-ONLY"}
   {tmsconst.i}

   DEF PRIVATE VAR lcBrand AS CHAR NO-UNDO. 

   DEF PRIVATE TEMP-TABLE ttIRCounter NO-UNDO
      FIELD rCounter AS RECID.

   
   CONSTRUCTOR billrun_invrowcounter(INPUT icBrand AS CHAR):
      lcBrand = icBrand.
   END CONSTRUCTOR.

   DESTRUCTOR billrun_invrowcounter():
   END DESTRUCTOR.

   METHOD PRIVATE LOG TS2Date 
     (input ts as dec, output dte as date).

      def var yy  as i  no-undo.
      def var mm  as i  no-undo.
      def var dd  as i  no-undo.
      def var c   as c  no-undo.

      assign
         c   = substr(string(ts,"99999999.99999"),1,8)
         yy  = integer(substr(c,1,4))
         mm  = integer(substr(c,5,2))
         dd  = integer(substr(c,7,2))
         dte = date(mm,dd,yy)
      no-error.

      if error-status:error then return false.
      else return true.

   end.

   METHOD PUBLIC VOID GetInvSeqs
      (iiInvCust AS INT,
       idaFromDate AS DATE,
       idaToDate AS DATE,
       icBillDeny AS CHAR,
       icMsSeqList AS CHAR,
       OUTPUT TABLE FOR ttInvSeq):

      DEF BUFFER bOldInvSeq FOR InvSeq.
      DEF BUFFER bUpdInvSeq FOR InvSeq.
      DEF BUFFER bMobCDR    FOR MobCDR.
      DEF BUFFER bIRCounter FOR InvRowCounter.
        
      EMPTY TEMP-TABLE ttInvSeq.

      /* quicker to get invseqs through msseq than through custnum */
      GetSubscription:
      FOR EACH MsOwner NO-LOCK WHERE 
               MsOwner.InvCust = iiInvCust AND
               MsOwner.PayType = FALSE
      BREAK BY MsOwner.MsSeq:
      
         IF FIRST-OF(MsOwner.MsSeq) THEN DO:
         
            /* billing denied */
            IF LOOKUP(STRING(MsOwner.MsSeq),icBillDeny) > 0 THEN NEXT.
  
            /* specific clis */
            IF icMSSeqList > "" AND 
               LOOKUP(STRING(MsOwner.MsSeq),icMSSeqList) = 0
            THEN NEXT.
       
            FOR EACH InvSeq NO-LOCK WHERE
                     InvSeq.MsSeq = MsOwner.MsSeq AND
                     InvSeq.CustNum = MsOwner.InvCust AND
                     InvSeq.Billed = FALSE AND
                     InvSeq.ToDate > idaFromDate AND
                     InvSeq.ToDate <= idaToDate
            BY InvSeq.ToDate DESC:

               CREATE ttInvSeq.
               BUFFER-COPY InvSeq TO ttInvSeq.
               IF ttInvSeq.AgrCust = 0 THEN 
                  ttInvSeq.AgrCust = MsOwner.AgrCust.

               LEAVE.  /* take only the newest */
            END.   
         END.   
      END.

      /* separate loops so that delete bufseq doesn't mess break by */    
      FOR EACH ttInvSeq:
      
         FIND FIRST InvSeq NO-LOCK WHERE InvSeq.InvSeq = ttInvSeq.InvSeq.
            
         /* update InvSeq to cdrs & remove unused InvSeq */
         FOR EACH bOldInvSeq NO-LOCK USE-INDEX MsSeq WHERE
                  bOldInvSeq.MsSeq   = InvSeq.MsSeq   AND
                  bOldInvSeq.CustNum = InvSeq.CustNum AND
                  bOldInvSeq.Billed  = FALSE          AND
                  bOldInvSeq.ToDate <= InvSeq.ToDate  AND
                  bOldInvSeq.ToDate >  idaFromDate    AND
                  bOldInvSeq.AgrCust = InvSeq.AgrCust AND
                  RECID(bOldInvSeq) NE RECID(InvSeq):

            FOR EACH MobCDR NO-LOCK WHERE
                     MobCDR.InvCust = bOldInvSeq.CustNum AND
                     MobCDR.InvSeq  = bOldInvSeq.InvSeq  AND
                     MobCDR.MsSeq   = ttInvSeq.MsSeq:

               DO TRANS:
                  FIND FIRST bMobCDR WHERE 
                     RECID(bMobCDR) = RECID(MobCDR) EXCLUSIVE-LOCK.
                  bMobCDR.InvSeq = InvSeq.InvSeq.
               END.      
            END.

            EMPTY TEMP-TABLE ttIRCounter.
         
            /* first collect counters to temptable, so that deletion 
               and key value changes do not mess the for each loop */
            FOR EACH InvRowCounter NO-LOCK WHERE
                     InvRowCounter.InvCust = bOldInvSeq.CustNum AND
                     InvRowCounter.InvSeq  = bOldInvSeq.InvSeq:
               CREATE ttIRCounter.         
               ttIRCounter.rCounter = RECID(InvRowCounter).
            END.
         
            /* move counters */
            FOR EACH ttIRCounter,
               FIRST InvRowCounter EXCLUSIVE-LOCK WHERE
                     RECID(InvRowCounter) = ttIRCounter.rCounter:
         
               FIND FIRST bIRCounter WHERE
                  bIRCounter.InvCust   = InvSeq.CustNum AND
                  bIRCounter.InvSeq    = InvSeq.InvSeq AND
                  bIRCounter.BillCode  = InvRowCounter.BillCode AND
                  bIRCounter.CCN       = InvRowCounter.CCN AND
                  bIRCounter.MsSeq     = InvRowCounter.MsSeq AND
                  bIRCounter.CLI       = InvRowCounter.CLI AND 
                  bIRCounter.TariffNum = InvRowCounter.TariffNum AND
                  bIRCounter.VatIncl   = InvRowCounter.VatIncl AND
                  bIRCounter.ReportingID = InvRowCounter.ReportingID AND
                  bIRCounter.DCEvent   = InvRowCounter.DCEvent AND
                  bIRCounter.ToDate    = InvRowCounter.ToDate NO-ERROR.

               IF AVAILABLE bIRCounter THEN DO:
                  ASSIGN
                     bIRCounter.Quantity = bIRCounter.Quantity + 
                                              InvRowCounter.Quantity
                     bIRCounter.Duration = bIRCounter.Duration + 
                                              InvRowCounter.Duration
                     bIRCounter.Amount   = bIRCounter.Amount + 
                                              InvRowCounter.Amount
                     bIRCounter.DataAmt  = bIRCounter.DataAmt +
                                              InvRowCounter.DataAmt
                     bIRCounter.RefPrice = bIRCounter.RefPrice +
                                              InvRowCounter.RefPrice
                     bIRCounter.ExtraAmount = bIRCounter.ExtraAmount + 
                                                 InvRowCounter.ExtraAmount.
                  DELETE InvRowCounter.     
               END.
      
               ELSE InvRowCounter.InvSeq = InvSeq.InvSeq.
            END.
         
            DO TRANS:

               FIND CURRENT InvSeq EXCLUSIVE-LOCK.
               InvSeq.FromDate = MIN(bOldInvSeq.FromDate,InvSeq.FromDate).

               /* remove combined one */
               FIND bUpdInvSeq WHERE 
                  RECID(bUpdInvSeq) = RECID(bOldInvSeq) EXCLUSIVE-LOCK.
               DELETE bUpdInvSeq.
            END.

         END.
      END.

   END METHOD.   
         
   METHOD PUBLIC CHAR SplitInvSeqs
      (INPUT-OUTPUT TABLE FOR ttInvSplit,
       INPUT-OUTPUT TABLE FOR ttInvSeq):
      
      DEF VAR lcNewInvSeqs AS CHAR NO-UNDO. 
      DEF VAR liLoop AS INT NO-UNDO. 

      DEF BUFFER bNewInvSeq FOR InvSeq.
      DEF BUFFER bMobCDR FOR MobCDR.
      
      /* Do this duplicate check here since all errors that prevent invoice 
         creation should be checked before InvSeq is split */
      FOR EACH ttInvSeq,
          EACH InvRowCounter NO-LOCK WHERE
               InvRowCounter.InvCust = ttInvSeq.AgrCust AND
               InvRowCounter.InvSeq  = ttInvSeq.InvSeq AND
               InvRowCounter.InvNum  = 0:
         IF InvRowCounter.Quantity < 0 OR InvRowCounter.Amount < 0 THEN 
            RETURN "ERROR:Invalid counter value".
      END.
       
      FOR EACH ttInvSeq:
         
         FIND FIRST InvSeq NO-LOCK WHERE InvSeq.InvSeq = ttInvSeq.InvSeq.
         
         FOR FIRST ttInvSplit WHERE
                   ttInvSplit.AgrCust = InvSeq.AgrCust AND
                   ttInvSplit.MsSeq   = InvSeq.MsSeq AND
                   ttInvSplit.SplitDate <= InvSeq.ToDate AND
                   ttInvSplit.CLiEvent = "iSS":

            IF CAN-FIND(FIRST bNewInvSeq WHERE
                              bNewInvSeq.MsSeq = InvSeq.MsSeq AND
                              bNewInvSeq.Custnum = InvSeq.Custnum AND
                              bNewInvSeq.Billed = TRUE AND
                              bNewInvSeq.ToDate = ttInvSplit.SplitDate - 1)
                        THEN LEAVE.
            
            DO TRANS: 
               CREATE bNewInvSeq.
               ASSIGN
                  bNewInvSeq.MsSeq    = InvSeq.MsSeq
                  bNewInvSeq.CustNum  = InvSeq.Custnum
                  bNewInvSeq.AgrCust  = InvSeq.AgrCust
                  bNewInvSeq.FromDate = ttInvSplit.SplitDate
                  bNewInvSeq.ToDate   = InvSeq.Todate
                  bNewInvSeq.InvSeq   = NEXT-VALUE(InvSeq)
                  bNewInvSeq.Billed   = TRUE
                  ttInvSplit.InvSeq[1] = InvSeq.InvSeq
                  ttInvSplit.InvSeq[2] = bNewInvSeq.InvSeq.

               lcNewInvSeqs = "," + lcNewInvSeqs + STRING(bNewInvSeq.InvSeq).
               
               FIND CURRENT InvSeq EXCLUSIVE-LOCK.
               InvSeq.ToDate = ttInvSplit.SplitDate - 1.
               
            END.

            FOR EACH MobCDR NO-LOCK WHERE
                     MobCDR.InvCust = InvSeq.CustNum AND
                     MobCDR.InvSeq  = InvSeq.InvSeq  AND
                     MobCDR.MsSeq   = InvSeq.MsSeq AND
                     MobCDR.DateSt >= ttInvSplit.SplitDate:
               DO TRANS:
                  FIND FIRST bMobCDR WHERE 
                     RECID(bMobCDR) = RECID(MobCDR) EXCLUSIVE-LOCK.
                  bMobCDR.InvSeq = bNewInvSeq.InvSeq.
               END.      
            
               EMPTY TEMP-TABLE ttIRCounter.
               /* first collect counters to temptable, so that deletion 
                  and key value changes do not mess the for each loop */
               FOR EACH InvRowCounter NO-LOCK WHERE
                        InvRowCounter.InvCust = InvSeq.AgrCust AND
                        InvRowCounter.InvSeq  = InvSeq.InvSeq AND
                        InvRowCounter.ToDate  = ttInvSplit.ToDate:
                  CREATE ttIRCounter.         
                  ttIRCounter.rCounter = RECID(InvRowCounter).
               END.

               /* move counters */
               FOR EACH ttIRCounter,
                  FIRST InvRowCounter EXCLUSIVE-LOCK WHERE
                        RECID(InvRowCounter) = ttIRCounter.rCounter:
                  InvRowCounter.InvSeq = bNewInvSeq.InvSeq.
               END.
            END. 
         END. 

      END.

      IF lcNewInvSeqs > "" THEN DO:
         lcNewInvSeqs = SUBSTRING(lcNewInvSeqs,2).
         DO liLoop = 1 TO NUM-ENTRIES(lcNewInvSeqs):
            
            FIND FIRST InvSeq NO-LOCK WHERE
                       InvSeq.InvSeq = INT(ENTRY(liLoop,lcNewInvSeqs))
            NO-ERROR.
            IF NOT AVAIL InvSeq THEN NEXT.
            
            CREATE ttInvSeq.
            BUFFER-COPY InvSeq TO ttInvSeq.
         END.
      END.

      RETURN "".

   END.
   
   METHOD PUBLIC CHAR MarkInvSeqs
      (INPUT iiInvCust AS INT,
       INPUT idaPeriodTo AS DATE,
       INPUT TABLE FOR ttInvSeq):

      DEF VAR lcResult AS CHAR NO-UNDO.
      DEF VAR liPeriod AS INT  NO-UNDO.
      
      liPeriod = YEAR(idaPeriodTo) * 100 + MONTH(idaPeriodTo).
      
      /* rating queue */
      IF CAN-FIND(FIRST TriggerItem WHERE 
                        TriggerItem.InvCust = iiInvCust AND
                        TriggerItem.StatusCode = 0 AND
                        TriggerItem.Period <= liPeriod) THEN 
         lcResult = "RETRY:TriggerRate".
 
      ELSE DO:
         /* customer based rerate request */
         IF CAN-FIND(FIRST MsRequest WHERE      
            MsRequest.Brand = lcBrand AND
            MsRequest.CustNum = iiInvCust AND
            MsRequest.ReqType = {&REQTYPE_RERATE} AND
            MsRequest.ReqDtParam1 <= idaPeriodTo AND
            LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0)
         THEN lcResult = "RETRY:RerateRequest".

         IF lcResult = "" THEN 
         FOR EACH ttInvSeq,
            FIRST InvSeq EXCLUSIVE-LOCK WHERE
                  InvSeq.InvSeq = ttInvSeq.InvSeq:

            /* are there unhandled rating / counter events */      
            IF CAN-FIND(FIRST TMQueue WHERE
                              TMQueue.InvSeq = InvSeq.InvSeq) THEN DO:
               lcResult = "RETRY:TMQueue".
               LEAVE.
            END.

            /* subscription based rerate request (should be found already
               in the customer based check) */    
            IF CAN-FIND(FIRST MsRequest WHERE      
               MsRequest.MsSeq = InvSeq.MsSeq AND
               MsRequest.ReqType = {&REQTYPE_RERATE} AND
               MsRequest.ReqDtParam1 <= InvSeq.ToDate AND
               LOOKUP(STRING(MsRequest.ReqStatus),
                      {&REQ_INACTIVE_STATUSES}) = 0) THEN DO:
               lcResult = "RETRY:RerateRequest".
               LEAVE.
            END.

            InvSeq.Billed = TRUE.
         END.

         IF lcResult > "" THEN 
         FOR EACH ttInvSeq,
            FIRST InvSeq EXCLUSIVE-LOCK WHERE
                  InvSeq.InvSeq = ttInvSeq.InvSeq AND
                  InvSeq.Billed = TRUE:
            InvSeq.Billed = FALSE.
         END.
      END.
      
      RETURN lcResult. 
   END METHOD.
   

   METHOD PUBLIC CHAR InvoiceRowsFromCounters
      (INPUT iiInvCust         AS INT,
       INPUT idaPeriodFrom     AS DATE,
       INPUT idaPeriodTo       AS DATE,
       INPUT  TABLE FOR ttInvSeq,
       INPUT  TABLE FOR ttInvSplit,
       OUTPUT TABLE FOR ttIR,
       OUTPUT TABLE FOR ttCLI,
       INPUT-OUTPUT iodaOldest AS DATE):
   
      DEF VAR lcServRid    AS CHAR NO-UNDO.
      DEF VAR lcMPMRid     AS CHAR NO-UNDO.
      DEF VAR liRowPeriod  AS INT  NO-UNDO.

      DEF VAR llCheckInvSplit AS LOG NO-UNDO. 
      llCheckInvSplit = CAN-FIND(FIRST ttInvSplit).

      FOR EACH ttInvSeq,
         FIRST InvSeq NO-LOCK WHERE
               InvSeq.InvSeq = ttInvSeq.InvSeq,
          EACH InvRowCounter NO-LOCK USE-INDEX InvCust WHERE
               InvRowCounter.InvCust = InvSeq.CustNum AND
               InvRowCounter.InvSeq  = InvSeq.InvSeq AND
               InvRowCounter.InvNum  = 0:
    
         IF InvRowCounter.Quantity = 0 AND InvRowCounter.Amount = 0 THEN
            NEXT.

         IF InvRowCounter.Quantity < 0 OR InvRowCounter.Amount < 0 THEN 
            RETURN "ERROR:Invalid counter value".
            
         ASSIGN
            iodaOldest   = MIN(iodaOldest,InvSeq.FromDate)
            lcServRid    = ENTRY(1,InvRowCounter.ReportingID).
            liRowPeriod = YEAR(InvRowCounter.ToDate) * 100 + 
                          MONTH(InvRowCounter.ToDate).
         
         IF NUM-ENTRIES(InvRowCounter.ReportingID) > 1 THEN 
            lcMPMRid = ENTRY(2,InvRowCounter.ReportingID).
         
         IF llCheckInvSplit AND
            CAN-FIND(FIRST ttInvSplit WHERE
                           ttInvSplit.AgrCust = InvSeq.AgrCust AND
                           ttInvSplit.MsSeq   = InvSeq.MsSeq) THEN
            FIND FIRST ttIR WHERE
                       ttIR.BillCode = InvRowCounter.BillCode  AND
                       ttIR.CLI      = InvRowCounter.CLI       AND 
                       ttIR.CCN      = InvRowCounter.CCN       AND
                       ttIR.Period   = liRowPeriod             AND
                       ttIR.MsSeq    = InvRowCounter.MsSeq     AND
                       ttIR.VatIncl  = InvRowCounter.VATIncl   AND
                       ttIR.ToDate   = InvRowCounter.ToDate    AND
                       ttIR.RowType  = 2 NO-ERROR.
      
         ELSE
            FIND FIRST ttIR WHERE
                       ttIR.BillCode = InvRowCounter.BillCode  AND
                       ttIR.CLI      = InvRowCounter.CLI       AND 
                       ttIR.CCN      = InvRowCounter.CCN       AND
                       ttIR.Period   = liRowPeriod             AND
                       ttIR.MsSeq    = InvRowCounter.MsSeq     AND
                       ttIR.VatIncl  = InvRowCounter.VATIncl   AND
                       ttIR.RowType  = 2 NO-ERROR.

         IF NOT AVAILABLE ttIR THEN DO:
            CREATE ttIR.
            ASSIGN
               ttIR.InvNum    = 0
               ttIR.FFRow     = FALSE
               ttIR.FromDate  = InvRowCounter.FromDate
               ttIR.ToDate    = InvRowCounter.ToDate
               ttIR.Period    = liRowPeriod
               ttIR.BillCode  = InvRowCounter.BillCode
               ttIR.CCN       = InvRowCounter.CCN
               ttIR.CLI       = InvRowCounter.CLI
               ttIR.MsSeq     = InvRowCounter.MsSeq
               ttIR.VatIncl   = InvRowCounter.VatIncl
               ttIR.RowType   = 2.
         END.

         ASSIGN
            ttIR.Qty      = ttIR.Qty + InvRowCounter.Quantity
            ttIR.dNet     = ttIR.dNet + InvRowCounter.Amount
            ttIR.dGross   = ttIR.dGross + InvRowCounter.ExtraAmount
            ttIR.Minutes  = ttIR.Minutes + InvRowCounter.Duration
            ttIR.DataAmt  = ttIR.DataAmt + InvRowCounter.DataAmt.


         FIND FIRST ttCLI WHERE
                    ttCLI.CLI       = InvRowCounter.CLI AND
                    ttCLI.MsSeq     = InvRowCounter.MsSeq AND
                    ttCLI.CCN       = InvRowCounter.CCN AND
                    ttCLI.BillCode  = InvRowCounter.BillCode AND
                    ttCLI.TariffNum = InvRowCounter.TariffNum AND
                    ttCLI.InvSeq    = InvRowCounter.InvSeq AND
                    ttCLI.VatIncl   = InvRowCounter.VatIncl AND
                    ttCLI.ToDate    = InvRowCounter.ToDate AND
                    ttCLI.ServRid   = lcServRid AND
                    ttCLI.MPMRid    = lcMPMRid NO-ERROR.
                 
         IF NOT AVAILABLE ttCLI THEN DO:
            CREATE ttCLI.
            ASSIGN 
               ttCLI.CLI       = InvRowCounter.CLI 
               ttCLI.MsSeq     = InvRowCounter.MsSeq
               ttCLI.CCN       = InvRowCounter.CCN 
               ttCLI.BillCode  = InvRowCounter.BillCode 
               ttCLI.TariffNum = InvRowCounter.TariffNum 
               ttCLI.InvSeq    = InvRowCounter.InvSeq 
               ttCLI.VatIncl   = InvRowCounter.VatIncl
               ttCLI.FromDate  = InvRowCounter.FromDate
               ttCLI.ToDate    = InvRowCounter.ToDate
               ttCLI.ServRid   = lcServRid 
               ttCLI.MPMRid    = lcMPMRid.
         END.
      
         ASSIGN
            ttCLI.Qty      = ttCLI.Qty + InvRowCounter.Quantity
            ttCLI.Minutes  = ttCLI.Minutes + InvRowCounter.Duration
            ttCLI.Amt      = ttCLI.Amt + InvRowCounter.Amount
            ttCLI.GenPrice = ttCLI.GenPrice + InvRowCounter.ExtraAmount
            ttCLI.MPMAmt   = ttCLI.MPMAmt + InvRowCounter.ExtraAmount
            ttCLI.DataAmt  = ttCLI.DataAmt + InvRowCounter.DataAmt.
      END.
   
      RETURN "".
      
   END METHOD.

   METHOD PUBLIC VOID UnifyInvRowCounterTaxHandling
      (INPUT iiInvCust     AS INT,
       INPUT iiInvSeq      AS INT,
       INPUT ilCustomerTax AS LOG,
       INPUT TABLE FOR ttIR):

      DEF VAR ldTaxFactor AS DEC  NO-UNDO.  
      
      DEF BUFFER bIRCounter FOR InvRowCounter.
      
       
      FOR EACH InvRowCounter EXCLUSIVE-LOCK USE-INDEX InvCust WHERE
               InvRowCounter.InvCust = iiInvCust AND
               InvRowCounter.InvSeq  = iiInvSeq  AND
               InvRowCounter.InvNum  = 0         AND
               InvRowCounter.VatIncl NE ilCustomerTax,
         FIRST ttIR WHERE
               ttIR.BillCode = InvRowCounter.BillCode AND
               ttIR.CLI      = InvRowCounter.CLI AND
               ttIR.MsSeq    = InvRowCounter.MsSeq:

         ldTaxFactor = (1 + ttIR.VatPerc / 100).
         
         IF ilCustomerTax THEN ASSIGN 
            InvRowCounter.Amount      = InvRowCounter.Amount * ldTaxFactor
            InvRowCounter.RefPrice    = InvRowCounter.RefPrice * ldTaxFactor
            InvRowCounter.ExtraAmount = InvRowCounter.ExtraAmount * 
                                           ldTaxFactor.
         ELSE ASSIGN
            InvRowCounter.Amount      = InvRowCounter.Amount / ldTaxFactor
            InvRowCounter.RefPrice    = InvRowCounter.RefPrice / ldTaxFactor
            InvRowCounter.ExtraAmount = InvRowCounter.ExtraAmount / 
                                           ldTaxFactor.

         FIND FIRST bIRCounter USE-INDEX InvCust WHERE
                    bIRCounter.InvCust   = InvRowCounter.InvCust AND
                    bIRCounter.InvSeq    = InvRowCounter.InvSeq AND
                    bIRCounter.BillCode  = InvRowCounter.BillCode AND
                    bIRCounter.CCN       = InvRowCounter.CCN AND
                    bIRCounter.MsSeq     = InvRowCounter.MsSeq AND
                    bIRCounter.CLI       = InvRowCounter.CLI AND 
                    bIRCounter.TariffNum = InvRowCounter.TariffNum AND
                    bIRCounter.VatIncl   = ilCustomerTax AND
                    bIRCounter.ReportingID = InvRowCounter.ReportingID AND
                    bIRCounter.DCEvent   = InvRowCounter.DCEvent AND
                    bIRCounter.ToDate    = InvRowCounter.ToDate AND
                    bIRCounter.InvNum    = 0 NO-ERROR.

         IF AVAILABLE bIRCounter THEN DO:

            ASSIGN
               bIRCounter.Quantity = bIRCounter.Quantity + 
                                        InvRowCounter.Quantity
               bIRCounter.Duration = bIRCounter.Duration + 
                                        InvRowCounter.Duration
               bIRCounter.Amount   = bIRCounter.Amount + InvRowCounter.Amount
               bIRCounter.DataAmt  = bIRCounter.DataAmt +
                                        InvRowCounter.DataAmt
               bIRCounter.RefPrice = bIRCounter.RefPrice +
                                           InvRowCounter.RefPrice
               bIRCounter.ExtraAmount = bIRCounter.ExtraAmount + 
                                           InvRowCounter.ExtraAmount.
             
            DELETE InvRowCounter.     
         END.
      
         ELSE InvRowCounter.VatIncl = ilCustomerTax.
      END.
      
   END METHOD.

   METHOD PUBLIC INTEGER MarkInvRowCountersBilled
      (INPUT iiInvCust   AS INT,
       INPUT iiInvSeq    AS INT,
       INPUT iiInvNum    AS INT,
       INPUT iiSubInvNum AS INT):
   
      DEF VAR liMarked AS INT  NO-UNDO.
      
      FOR EACH InvRowCounter EXCLUSIVE-LOCK USE-INDEX InvCust WHERE
               InvRowCounter.InvCust = iiInvCust AND
               InvRowCounter.InvSeq  = iiInvSeq  AND
               InvRowCounter.InvNum  = 0:
               
         ASSIGN
            InvRowCounter.InvNum    = iiInvNum
            InvRowCounter.SubInvNum = iiSubInvNum
            liMarked                = liMarked + 1.
      END.
   
      RETURN liMarked. 
      
   END METHOD.

END CLASS.

