{Syst/commpaa.i}
gcBrand = "1".

def var i as int no-undo.
def var j as int no-undo.
def var licount as int no-undo. 
def var lccli as char no-undo.

pause 0.
update lccli format "x(12)" label "MSISDN"
   with overlay row 10 centered title " RECALC COUNTER " 
        side-labels frame fCalc.
hide frame fCalc no-pause.
if lccli = "" then return.


for first msowner no-lock use-index cli_s where          
          msowner.cli = lccli:
  
   for each invseq no-lock use-index msseq where
            invseq.msseq = msowner.msseq and
            invseq.custnum = msowner.invcust and
            invseq.billed = false:

      i = i + 1.

      RUN billing/conv_invrowcounter.p(InvSeq.InvSeq,
                               OUTPUT liCount).

      j = j + liCount.
      pause 0.
      disp i j.
   end.

end.

message i "unbilled InvSeqs were handled, containing" 
        j "CDRs"
view-as alert-box.
 
