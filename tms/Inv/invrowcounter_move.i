/* invrowcounter_move.i  03.11.11/aam 
   move / combine InvrowCounters between InvSeqs
*/

&IF "{&invrowcounter_move}" NE "YES"
&THEN

&GLOBAL-DEFINE invrowcounter_move YES

DEF TEMP-TABLE ttIRCounter NO-UNDO
   FIELD rCounter AS RECID.

FUNCTION fMoveInvrowCounters RETURNS LOGIC
   (iiInvCust AS INT,
    iiCurrentInvSeq AS INT,
    iiNewInvSeq AS INT):
    
   DEF BUFFER bIRCounter FOR InvRowCounter.
   
   DEF VAR llFound AS LOG  NO-UNDO INIT FALSE.
    
   EMPTY TEMP-TABLE ttIRCounter.

   /* first collect counters to temptable, so that deletion and key value 
      changes do not mess the for each loop */
   FOR EACH InvRowCounter NO-LOCK WHERE
            InvRowCounter.InvCust = iiInvCust AND
            InvRowCounter.InvSeq  = iiCurrentInvSeq:
      CREATE ttIRCounter.         
      ttIRCounter.rCounter = RECID(InvRowCounter).
   END.
         
   FOR EACH ttIRCounter,
      FIRST InvRowCounter EXCLUSIVE-LOCK WHERE
            RECID(InvRowCounter) = ttIRCounter.rCounter:
            
      FIND FIRST bIRCounter WHERE
         bIRCounter.InvCust   = iiInvCust AND
         bIRCounter.InvSeq    = iiNewInvSeq AND
         bIRCounter.BillCode  = InvRowCounter.BillCode AND
         bIRCounter.CCN       = InvRowCounter.CCN AND
         bIRCounter.MsSeq     = InvRowCounter.MsSeq AND
         bIRCounter.CLI       = InvRowCounter.CLI AND 
         bIRCounter.TariffNum = InvRowCounter.TariffNum AND
         bIRCounter.VatIncl   = InvRowCounter.VatIncl AND
         bIRCounter.ReportingID = InvRowCounter.ReportingID AND
         bIRCounter.DCEvent   = InvRowCounter.DCEvent AND
         bIRCounter.ToDate    = InvRowCounter.ToDate 
         EXCLUSIVE-LOCK NO-ERROR.

      IF AVAILABLE bIRCounter THEN DO:
         ASSIGN
            bIRCounter.Quantity = bIRCounter.Quantity + InvRowCounter.Quantity
            bIRCounter.Duration = bIRCounter.Duration + InvRowCounter.Duration
            bIRCounter.Amount   = bIRCounter.Amount + InvRowCounter.Amount
            bIRCounter.DataAmt  = bIRCounter.DataAmt + InvRowCounter.DataAmt
            bIRCounter.RefPrice = bIRCounter.RefPrice + InvRowCounter.RefPrice
            bIRCounter.ExtraAmount = bIRCounter.ExtraAmount + 
                                        InvRowCounter.ExtraAmount.
         DELETE InvRowCounter.     
      END.
      ELSE InvRowCounter.InvSeq = InvSeq.InvSeq.
 
      llFound = TRUE.
   END.

   EMPTY TEMP-TABLE ttIRCounter.

   RETURN llFound. 

END FUNCTION.

&ENDIF

