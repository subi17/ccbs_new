FUNCTION fTotalHU RETURNS LOGICAL
   (INPUT  iccli    AS CHAR,
    OUTPUT oDeTotal AS DEC). 
   
   DEF BUFFER xxInvseq for Invseq.                          
   odetotal =  0.

   DEF VAR liPeriod AS INT NO-UNDO.
   
   FIND FIRST Mobsub WHERE 
              Mobsub.cli = iccli NO-LOCK NO-ERROR.
              
   IF NOT avail mobsub then next.

   FOR EACH xxinvseq WHERE 
            xxInvseq.MSSeq   = Mobsub.msseq   AND 
            xxInvSeq.Custnum = mobsub.Custnum AND 
            xxInvSeq.Billed  = false NO-LOCK.

      liPeriod = YEAR(xxInvseq.fromdate) * 100 + MOnth(xxInvseq.fromdate).
      
      FOR EACH SaldoCounter WHERE 
               SaldoCounter.msseq    = mobsub.msseq AND 
               SaldoCounter.period   = liperiod  No-LOCK.
         odetotal = odetotal + SaldoCounter.amt.
      END.
   END.
                      
END FUNCTION.    
