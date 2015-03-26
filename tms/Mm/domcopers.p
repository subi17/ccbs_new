/* -----------------------------------------------------------------
  MODULE .......: NNDOMCO.P
  TASK .........: Erases double MOBILE calls fom database
  APPLICATION ..: NN
  AUTHOR .......: JP
  CREATED ......: 20.09.2005
  CHANGED ......: 04.01.06/aam SaldoCounter, update only if marked cdr
                               has been on an invseq 
  Version ......: 
  ------------------------------------------------------------------ */

{commali.i}
{cparam2.i}
{error_codes.i}

DEF VAR logfile       AS CHAR NO-UNDO.

DEF STREAM excel.

ASSIGN
   logfile = "/tmp/" 
   logfile = logfile + "dblcalls_" + 
                      string(year(today)) +  string(Month(today)) +
                      string(day(today)) + ".txt".

PROCEDURE pDoubleCalls:

   DEF INPUT PARAMETER iiInvCust AS INT NO-UNDO.

   DEF VAR i         AS INT  NO-UNDO.
   DEF VAR lii       AS INT  NO-UNDO.
   DEF VAR liMarked  AS INT  NO-UNDO.
   DEF VAR liAgrCust AS INT  NO-UNDO.

   DEF BUFFER xxSeq FOR InvSeq.
   DEF BUFFER double FOR MobCDR.
   DEF BUFFER markcdr FOR MobCDR.


   IF NOT SESSION:BATCH THEN 
      PUT SCREEN ROW 2 COL 78 "2".

   ASSIGN
      lii = 0
      i   = 0
      liMarked = 0.
   
   FOR EACH InvSeq NO-LOCK WHERE 
            InvSeq.Custnum = iiInvCust   AND 
            InvSeq.Billed  = FALSE       AND 
            InvSeq.fromdate > today - 90 AND 
            Invseq.todate   < today,
       EACH MobCDR NO-LOCK WHERE 
            Mobcdr.InvCust = iiInvCust      AND 
            Mobcdr.Invseq = Invseq.invseq:
                 
      IF MobCDR.ErrorCode =  {&CDR_ERROR_DOUBLE_CALL} OR
         MobCDR.ErrorCode =  {&CDR_ERROR_DOUBLE_CCGW_CDR} OR
         MobCDR.ErrorCode =  {&CDR_ERROR_DOUBLE_DATA_CDR} THEN NEXT.

      lii = lii + 1.
     
      IF NOT SESSION:BATCH AND lii mod 100 = 0 THEN DO:
         PUT SCREEN ROW 1 STRING(lii).
      END.
      
      FOR EACH double USE-INDEX cli  NO-LOCK WHERE
               double.DateSt      = MobCDR.DateSt       AND
               double.TimeSt      = MobCDR.TimeSt       AND
               double.CLI         = MobCDR.CLI          AND
               recid(double)  NE recid(MobCDR)
      WITH FRAME LOG:

         IF double.BillDur    ne MobCDR.BillDur  OR
            double.GsmBnr     ne MobCDR.GsmBnr   OR
            double.spocmt     ne mobcdr.spocmt   OR
            double.ccharge    NE mobcdr.ccharge  OR
            double.ErrorCode  = {&CDR_ERROR_DOUBLE_CALL}          OR
            double.ErrorCode  = {&CDR_ERROR_DOUBLE_CCGW_CDR}      OR
            double.ErrorCode  = {&CDR_ERROR_DOUBLE_DATA_CDR}    
         THEN NEXT.
        
         i = i + 1.

         FIND FIRST xxSeq WHERE 
                    xxSeq.InvSeq = Double.InvSeq NO-LOCK NO-ERROR.
         IF NOT AVAILABLE xxSeq THEN NEXT.   
                     
         DO TRANSACTION:
                          
            IF xxSeq.Billed = FALSE THEN DO:
               FIND FIRST markcdr WHERE
                  RECID(markcdr) = recid(double) EXCLUSIVE-LOCK.
               liAgrCust = xxSeq.AgrCust.
            END.
            ELSE DO:
               FIND FIRST markcdr WHERE
                  RECID(markcdr) = recid(MobCDR) EXCLUSIVE-LOCK.
               liAgrCust = InvSeq.AgrCust.
            END.
            
            IF Markcdr.ErrorCode = 0 AND
               MarkCdr.Invseq    > 0 THEN DO:
            
               FIND FIRST SaldoCounter WHERE
                          SaldoCounter.MsSeq  = MarkCDR.MsSeq AND
                          SaldoCounter.Period = YEAR(MarkCDR.DateSt) * 100 + 
                                                MONTH(MarkCDR.DateSt)
               EXCLUSIVE-LOCK NO-ERROR NO-WAIT.             

               IF AVAIL SaldoCounter AND NOT locked(SaldoCounter) THEN DO:
                  SaldoCounter.amt = SaldoCounter.Amt - MarkCDR.Amount.

                  RELEASE SaldoCounter.
               END.    

               CREATE TMQueue.
               BUFFER-COPY MarkCDR TO TMQueue.
               ASSIGN
                  TMQueue.Qty     = -1
                  TMQueue.EventID = MarkCDR.DtlSeq
                  TMQueue.AgrCust = liAgrCust
                  TMQueue.Source  = MarkCDR.MSCID
                  TMQueue.PayType = 1 + INT(MarkCDR.PPFlag > 0)
                  TMQueue.ReportingID = MarkCDR.ServRid + "," + MarkCDR.MPMRid
                  TMQueue.ExtraAmount = MarkCDR.MPMAmt.

               /* do not update fraud counters from old events */
               IF YEAR(MarkCDR.DateSt) < YEAR(TODAY) OR
                  MONTH(MarkCDR.DateSt) < MONTH(TODAY) 
               THEN TMQueue.AccumTarget = "InvRow".
               
               RELEASE TMQueue.
            END.
         
            ASSIGN 
               Markcdr.ErrorCode  = IF MarkCDR.MSCID = "CCGW" THEN 
                                       {&CDR_ERROR_DOUBLE_CCGW_CDR}
                                    ELSE IF MarkCDR.MSCID = "POSTD" THEN 
                                       {&CDR_ERROR_DOUBLE_DATA_CDR}
                                    ELSE {&CDR_ERROR_DOUBLE_CALL}
               MarkCdr.Invseq     =  0
               liMarked           = liMarked + 1.
         END.
         
      END.
   END.

   IF i > 0 THEN DO:
      OUTPUT STREAM excel TO value(logfile) APPEND.

      PUT STREAM excel UNFORMATTED 
         today format "99-99-9999"   "|"
         string(time,"hh:mm:ss")     "|" 
         iiInvCust                   "|"
         i                           "|"
         liMarked                    SKIP.
     OUTPUT STREAM excel CLOSE.
   END.

END PROCEDURE.


