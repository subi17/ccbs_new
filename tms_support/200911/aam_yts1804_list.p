{Syst/testpaa.i}
{Func/timestamp.i}

def var limsseq as int no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var ldbal as dec no-undo.
def var ldreq as dec no-undo.
def var ldcdr as dec no-undo.
def var ldinit as dec no-undo.
def var lddiff as dec no-undo.

def buffer breq for prepaidrequest.

def stream slog.
output stream slog to /apps/snet/200911/yts1803_topup_balance_chk.txt.

put stream slog unformatted
   "Subscription ID" chr(9)
   "MSISDN"  chr(9)
   "Activated" chr(9)
   "Topups"    chr(9)
   "Usage"     chr(9)
   "Calc. Bal" chr(9)
   "Current Bal" chr(9)
   "Diff"      chr(9)
   "Initial topup" skip.

for each breq no-lock where
         breq.brand = "1" and
         breq.source = "web order" and
         breq.tsrequest >= 20091031 and
         breq.tsrequest <= 20091031.86400,
   first mobsub no-lock where
         mobsub.msseq = breq.msseq:

   i = i + 1.

   ldinit = breq.topupamt / 100.
   
   ldcdr = 0.
   for each prepcdr no-lock where
            prepcdr.cli = mobsub.cli and
            prepcdr.datest >= 10/31/9:
      ldcdr = ldcdr + prepcdr.charge.
   end.
   ldcdr = round(ldcdr,2).
   
   ldreq = 0.
   for each prepaidrequest no-lock where
            prepaidrequest.brand = "1" and 
            prepaidrequest.msseq = mobsub.msseq and
            prepaidrequest.respcode = 0 and
            prepaidrequest.ppstatus = 2 and
            prepaidrequest.tsrequest >= 20091031:
      ldreq = ldreq + prepaidrequest.topupamt / 100.      
   end.

   RUN Gwy/balancequery.p(MobSub.CLI).
   ldBal = INT(RETURN-VALUE) / 100.
   

   lddiff = ldreq - ldcdr - ldbal.

   disp 
      i format ">>9" 
      j format ">>9"
      mobsub.cli format "x(10)"
      mobsub.msseq 
      fts2hms(mobsub.activationts) format "x(19)"
      lddiff format "->>>>>9.99"
      /*
      ldbal format "->>>9.99"
      ldreq format "->>>9.99"
      ldcdr format "->>>9.99"
      ldreq - ldcdr format "->>>9.99" column-label "calc bal"
      */
      .

   if lddiff > 2 then do:
      j = j + 1.
  
      put stream slog unformatted
      mobsub.msseq chr(9)
      mobsub.cli   chr(9)
      fts2hms(mobsub.activationts) chr(9)
      ldreq        chr(9)
      ldcdr        chr(9)
      ldreq - ldcdr chr(9)
      ldbal        chr(9)
      lddiff       chr(9)
      ldinit       skip.
   end.
      
end.


