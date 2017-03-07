{Syst/testpaa.i}
katun = "Qvantel".
{Func/ftaxdata.i}
{Func/ftopup.i}

def var lcmaintask as char no-undo init "YOT-566".
def var liperiod   as int  no-undo init 201002.

def stream sread.
input stream sread from value("/apps/snet/" +
                             string(liperiod,"999999") + 
                             "/PREPAGO_F_F_" +
                             string(liperiod,"999999") + ".txt").

def stream slog.
output stream slog to value("/apps/snet/" +
                            string(liperiod,"999999") + "/" + 
                            lc(replace(lcmaintask,"-","")) +
                            "_topup_995_25.log") append.


def var lccli    as char no-undo.
def var ldamt    as dec  no-undo.
def var i        as int  no-undo.
def var j        as int  no-undo.
def var lcerror  as char no-undo.
def var lctaxzone as char no-undo.
def var lirequest as int no-undo.

function flog returns logic
  (icmessage as char):

   put stream slog unformatted
      lccli  ";"
      icmessage skip.
 
end function.

ldamt = 25.

repeat:

   import stream sread unformatted lccli.
   
   i = i + 1.
      
   disp lccli    format "x(10)"
        ldamt    format ">>>9.99".

   find first mobsub use-index cli_u where
              mobsub.cli = lccli and
              mobsub.paytype = true no-lock no-error.
   if not available mobsub then do:
      if can-find(first mobsub where mobsub.cli = lccli) then do:
         flog("Incorrect payment type").
      end.
      else do:
         flog("Unknown MSISDN").
      end.   
      next.
   end.
 
   if lookup(mobsub.clitype,"tarj3,tarjrd1") > 0 then do:
      flog("Invalid subscription type").
      next.
   end.
 
   find first customer where customer.custnum = mobsub.invcust no-lock.
   lcTaxZone = fRegionTaxZone(Customer.Region).
   
   liRequest = fCreateTopUpRequest(mobsub.msseq,
                                   mobsub.cli,
                                   "RefillTRequest",
                                   "COMP",
                                   "RefillTRequest",
                                   "995",
                                   lcmaintask,
                                   lcTaxZone,
                                   0,
                                   ldAmt * 100,
                                   0.0).
   
   if liRequest = 0 then do:
      flog("Topup was not created").
   end.
   
   else do:
      flog("OK").
   end.
   
   j = j + 1.
end.

input stream sread close.
output stream slog close.

disp i j.


