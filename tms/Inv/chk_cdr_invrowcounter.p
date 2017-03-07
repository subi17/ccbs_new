{Syst/commali.i}
{Syst/funcrunprocess_update.i}
{Func/timestamp.i}
{Func/date.i}
{Func/cparam2.i}
{Func/istc.i}
{Inv/chk_cdr_invrowcounter.i &ttReference = "REFERENCE-ONLY"}

DEF INPUT  PARAMETER TABLE FOR ttSubs.
DEF INPUT  PARAMETER icRunID       AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idaPeriodEnd  AS DATE NO-UNDO.
DEF INPUT  PARAMETER iIFRProcessID AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdInterval AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiCounterQty  AS INT  NO-UNDO.

DEF VAR i AS INT NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR l AS INT NO-UNDO. 
DEF VAR ldaToDate AS DATE NO-UNDO.
DEF VAR lcMatch   AS CHAR NO-UNDO.
DEF VAR liRepeat  AS INT  NO-UNDO.
DEF VAR liLoop    AS INT  NO-UNDO.
DEF VAR llHeader  AS LOG  NO-UNDO INIT TRUE.
DEF VAR lcDir     AS CHAR NO-UNDO.
DEF VAR llISTCChecked AS LOG NO-UNDO. 
DEF VAR ldaISTCDate AS DATE NO-UNDO. 
DEF VAR ldaCounterToDate AS DATE NO-UNDO. 
DEF VAR llErrorFound AS LOGICAL NO-UNDO. 
DEF VAR lcCLI AS CHAR NO-UNDO. 
DEF VAR liDuration AS INT64 NO-UNDO.

DEF TEMP-TABLE ttCounter NO-UNDO
   LIKE InvRowCounter.

lcDir = fCParamC("ChkUnbilledIRCounterDir").
IF lcDir = "" OR lcDir = ? THEN lcDir = "/scratch/log/invrowcounter".

DEF STREAM sLog.
OUTPUT STREAM sLog TO VALUE(lcDir + "/chk_cdr_invrowcounter_" + 
                            STRING(year(today),"9999") + 
                            STRING(month(today),"99") + 
                            STRING(day(today),"99") + "_" +
                            STRING(icRunID) + ".log") APPEND.

ldaToDate = idaPeriodEnd.
IF ldaToDate = ? THEN ldaToDate = fLastDayOfMonth(TODAY).

FOR EACH ttSubs,
   FIRST MsOwner NO-LOCK WHERE
         MsOwner.MsSeq = ttSubs.MsSeq AND
         MsOwner.InvCust = ttSubs.InvCust AND
         MsOwner.PayType = FALSE:
   
   ASSIGN
      i = i + 1
      liLoop = 0
      llErrorFound = FALSE
      llISTCChecked = FALSE
      ldaISTCDate = ?.
   
   ChkCounter:
   REPEAT:
   
      liLoop = liLoop + 1.
   
      FOR EACH InvSeq NO-LOCK WHERE
               InvSeq.MsSeq = MsOwner.MsSeq AND
               InvSeq.CustNum = MsOwner.InvCust AND
               InvSeq.Billed = FALSE AND
               InvSeq.ToDate = ldaToDate:
               
         IF NOT llISTCChecked THEN DO:
            ldaISTCDate = fGetiSTCDate(Invseq.MsSeq, 
                                       InvSeq.Custnum,
                                       InvSeq.ToDate).
            llISTCChecked = TRUE.
         END.
            
         EMPTY TEMP-TABLE ttCounter.
    
         FOR EACH MobCDR NO-LOCK USE-INDEX InvSeq WHERE
                  MobCDR.InvCust = InvSeq.CustNum AND
                  MobCDR.InvSeq = InvSeq.InvSeq:
            IF ldaISTCDate NE ? AND ldaISTCDate > MobCDR.DateSt THEN 
               ldaCounterToDate = ldaISTCDate - 1.
            ELSE ldaCounterToDate = ldaToDate.

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
               ttCounter.ToDate      = ldaCounterToDate NO-ERROR.

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
                  ttCounter.ToDate      = ldaCounterToDate.
            END.

            ASSIGN
               ttCounter.Quantity = ttCounter.Quantity + 1
               ttCounter.Duration = ttCounter.Duration + MobCDR.BillDur
               ttCounter.Amount   = ttCounter.Amount + MobCDR.Amount 
               ttCounter.DataAmt  = ttCounter.DataAmt + 
                                    MobCDR.DataIn + MobCDR.DataOut
               ttCounter.RefPrice = ttCounter.RefPrice + MobCDR.RefPrice.
         END.
   
         ASSIGN
            lcMatch = ""
            lcCLI = "".
   
         FOR EACH ttCounter
         BY ttCounter.billcode
         BY ttCounter.ccn:
   
            IF liLoop = 1 THEN oiCounterQty = oiCounterQty + 1.
            
            IF iiUpdInterval > 0 AND oiCounterQty MOD iiUpdInterval = 0 
            THEN DO:
               IF NOT fUpdateFuncRunProgress(iIFRProcessID,oiCounterQty) THEN 
                  RETURN "ERROR:Stopped".
            END.   

            liDuration = 0.
      
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
               ACCUMULATE InvRowCounter.Amount (TOTAL).

               liDuration = liDuration + InvRowCounter.Duration.

            END.

            IF (ACCUM COUNT InvRowCounter.InvCust) EQ 0 THEN ASSIGN
               lcCLI = ttCounter.CLI
               lcMatch = "ERROR: No counter found: " + 
                         STRING(ttCounter.billcode) + "/" +
                         STRING(ttCounter.CCN) + "/" +
                         STRING(ttCounter.Quantity) + "/" +
                         STRING(ttCounter.DataAmt / 1024 * 1024) + 
                         TRIM(STRING(ttCounter.Amount,"->>>>>9.99999")).
      
            ELSE IF 
               (ACCUM TOTAL InvRowCounter.Quantity) NE ttCounter.Quantity OR
                liDuration NE ttCounter.Duration OR
               (ACCUM TOTAL InvRowCounter.Amount) NE ttCounter.Amount OR
               (ACCUM TOTAL InvRowCounter.DataAmt) NE ttCounter.DataAmt THEN ASSIGN
                  lcCLI = ttCounter.CLI
                  lcMatch = "ERROR: Values differ: " + 
                            STRING(ttCounter.billcode) + "/" +
                            STRING(ttCounter.CCN).
            ELSE IF (ACCUM COUNT InvRowCounter.InvCust) NE 1 THEN ASSIGN
               lcCLI = ttCounter.CLI
               lcMatch = "WARNING: Multiple counters found: " + 
                         STRING(ttCounter.billcode) + "/" +
                         STRING(ttCounter.CCN).

            IF lcMatch > "" THEN LEAVE.    
         END.        

         IF lcMatch = "" THEN 
         FOR EACH InvRowCounter NO-LOCK WHERE
            InvRowCounter.InvCust = InvSeq.CustNum AND
            InvRowCounter.InvSeq = InvSeq.InvSeq:

            IF InvRowCounter.Quantity = 0 AND InvRowCounter.Amount = 0 THEN 
               NEXT.
          
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
               lcCLI   = InvRowCounter.CLI.
               lcMatch = "Counter without CDRs: " +
                         STRING(InvRowCounter.billcode) + "/" +
                         STRING(InvRowCounter.CCN) + "/" +
                         STRING(InvRowCounter.ToDate,"99-99-99").
               LEAVE.
            END.
         END.   
         
         IF lcMatch > "" THEN DO:
   
            IF CAN-FIND(FIRST TMQueue WHERE TMQueue.InvSeq = InvSeq.InvSeq)
            THEN DO:
               PAUSE 0.
               liRepeat = liRepeat + 1.
               IF NOT SESSION:BATCH THEN DO:
                  DISP MsOwner.CLI MsOwner.MsSeq i oiCounterQty k l liRepeat
                      WITH 1 DOWN.
               END.       
               PAUSE 5 NO-MESSAGE.    
               NEXT ChkCounter.
            END.
      
            l = l + 1.
            llErrorFound = TRUE.

            IF llHeader THEN DO:
               PUT STREAM sLog UNFORMATTED
                  "MSISDN"  CHR(9)
                  "MsSeq"   CHR(9)
                  "InvSeq"  CHR(9)
                  "Period"  CHR(9)
                  "Reason"  SKIP.
               llHeader = FALSE.
            END.
         
            PUT STREAM sLog UNFORMATTED
               lcCLI CHR(9)
               MsOwner.MsSeq CHR(9)
               InvSeq.InvSeq CHR(9)
               InvSeq.ToDate CHR(9)
               lcMatch SKIP.
         END.
   
      END. 

      ttSubs.ErrorFound = llErrorFound.
 
      LEAVE.
   END.
   
   IF NOT SESSION:BATCH THEN DO:
      IF i MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP MsOwner.CLI MsOwner.MsSeq i oiCounterQty k l liRepeat 
            WITH 1 DOWN.
      END.
   END.
      
END.   

OUTPUT STREAM sLog CLOSE.
      
IF NOT SESSION:BATCH THEN DO:
   DISP i oiCounterQty k l .
END.
   

