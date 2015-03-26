def var lisubs as int no-undo.
def var libill as int no-undo.
def var lideny as int no-undo.
def var linew  as int no-undo.
def var lldeny as log no-undo.
def var llbill as log no-undo.
def var llnew  as log no-undo.
def var ldfirst as dec no-undo.

def buffer bowner for msowner.


def temp-table ttseq no-undo
   field msseq as int
   index msseq msseq.

def stream slog.
output stream slog to /apps/snet/200709/aam_yts169_II.log.   

put stream slog unformatted
   "Unclear subs:" skip
   "MSISDN"  chr(9)
   "MsSeq"   chr(9)
   "Inv.Customer" chr(9)
   "Inv.Group"    skip.

   
for each msowner no-lock where 
         brand = "1" and
         tsbeg < 20070901 and
         tsend > 20070801 and
         clitype = "cont" and
         cli > "":

   if can-find(first ttseq where ttseq.msseq = msowner.msseq) then next.
   
   lisubs = lisubs + 1.
   
   assign 
      lldeny  = false
      llbill  = false
      llnew   = false
      ldfirst = msowner.tsbeg.

   find mobsub where mobsub.msseq = msowner.msseq no-lock no-error.
   if available mobsub and mobsub.repcodes = "x" then assign
      lldeny = true
      lideny = lideny + 1.
      
   if can-find(first invoice use-index cli where 
                     invoice.brand = "1" and
                     invoice.cli = msowner.cli and
                     invoice.invtype = 1 and
                     invoice.invdate >= 9/13/7)
   then assign 
      llbill = true
      libill = libill + 1.
      
   else if not lldeny and msowner.tsbeg > 20070801 then do:
     for each bowner no-lock where
              bowner.msseq = msowner.msseq
     by bowner.tsbeg:
     
        ldfirst = min(ldfirst,bowner.tsbeg).
     end.   
    
     if ldfirst > 20070801 then assign
        llnew = true
        linew = linew + 1.
   end.
   
   if lisubs mod 1000 = 0 then do:
      pause 0.
      disp lisubs lideny libill linew with 1 down.
   end.
   
   if not lldeny and not llbill and not llnew then do:
      find customer where customer.custnum = msowner.invcust no-lock no-error.
      
      put stream slog unformatted
         msowner.cli     chr(9)
         msowner.msseq   chr(9)
         msowner.invcust chr(9)
         (if available customer then customer.invgroup else "") skip.
   end.
      
   create ttseq.
   ttseq.msseq = msowner.msseq.

end.

disp lisubs lideny libill linew .

put stream slog unformatted
    skip(3)
    "Totals:" skip
    "Subscriptions"  chr(9)
    "Denied"         chr(9)
    "Billed"         chr(9)
    "New"            skip
    lisubs   chr(9)
    lideny   chr(9)
    libill   chr(9)
    linew    skip.

output stream slog close.


