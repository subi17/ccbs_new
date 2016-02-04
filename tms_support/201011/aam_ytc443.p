{Syst/testpaa.i}
katun = "ari".

DEF VAR i         AS I  NO-UNDO.
DEF VAR lii       AS i  NO-UNDO.
DEF VAR logfile   AS c  NO-UNDO.
DEF VAR ErrCode   AS i  NO-UNDO.
DEF VAR liMarked  AS INT  NO-UNDO.
def var li93      as int  no-undo.
def var lccli as char no-undo.

DEF BUFFER xxSeq FOR InvSeq.
DEF BUFFER double FOR MobCDR.
DEF BUFFER markcdr FOR MobCDR.

DEF STREAM sLog.

def temp-table ttcli no-undo
   field cli as char
   index cli cli.
   
ErrCode = 8001.   

ASSIGN 
  logfile = "/tmp/" .
  logfile = logfile + "dblcalls_" + 
                      string(year(today)) +  string(Month(today)) +
                      string(day(today)) + ".txt".

ASSIGN
   lii = 0
   i   = 0
   liMarked = 0.
   
FOR EACH MobCDR NO-LOCK use-index date WHERE 
         MobCDR.DateSt >= 11/23/10 and
         MobCDR.DateSt <= 11/24/10 and
         MobCDR.ErrorCode = 0,
   first InvSeq no-lock where
         InvSeq.InvSeq = MobCDR.InvSeq and
         InvSeq.Billed = false:

      lii = lii + 1.
     
      IF lii mod 1000 = 0 THEN DO:
         pause 0.
         disp mobcdr.datest
              string(mobcdr.timest,"hh:mm:ss")
              lii
              limarked
              li93
         with 1 down.
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
            double.ErrorCode  = ErrCode    
         THEN NEXT.
        
         i = i + 1.

         FIND FIRST xxSeq WHERE 
                    xxSeq.InvSeq = Double.InvSeq NO-LOCK NO-ERROR.
         IF NOT AVAILABLE xxSeq THEN NEXT.   
                     
         DO TRANSACTION:
                          
            IF xxSeq.Billed = FALSE THEN FIND FIRST markcdr WHERE
                  RECID(markcdr) = recid(double) EXCLUSIVE-LOCK.
            ELSE FIND FIRST markcdr WHERE
                  RECID(markcdr) = recid(MobCDR) EXCLUSIVE-LOCK.
            
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
            END.
         
            ASSIGN 
               Markcdr.ErrorCode  = errcode
               MarkCdr.Invseq     =  0
               liMarked           = liMarked + 1.
               
            if markcdr.spocmt = 93 then li93 = li93 + 1.   
               
            find first ttcli where ttcli.cli = markcdr.cli no-error.
            if not available ttcli then do:
               create ttcli.
               ttcli.cli = markcdr.cli.
               
               output stream slog to 
                  /apps/yoigo/tms_support/201011/aam_ytc443_clilist.log
                  append.
               put stream slog unformatted
                  ttcli.cli skip.
               output stream slog close.
            end.
         END.
         
      END.
END.

/*
IF i > 0 THEN DO:
   OUTPUT STREAM slog TO value(logfile) APPEND.

   PUT STREAM slog UNFORMATTED 
      today format "99-99-9999"   "|"
      string(time,"hh:mm:ss")     "|" 
      "ytc-443"                   "|"
      i                           "|"
      liMarked                    SKIP.
  OUTPUT STREAM slog CLOSE.

END.
*/

/*
output stream slog to /apps/yoigo/tms_support/201011/aam_ytc443_rated.log
    append.
    
i = 0.
for each ttcli:

    i = i + 1.
    pause 0.
    put screen row 18 "Rerate " + ttcli.cli + " qty: " + string(i).

    RUN Rate/cli_rate.p (ttcli.cli,
                    11/1/10,
                    11/30/10,
                    true).
                    
    put stream slog unformatted
       ttcli.cli skip.
end.

output stream slog close.
*/



