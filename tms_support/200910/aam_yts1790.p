{testpaa.i}
katun = "ari".
{ftaxdata.i}
{ftopup.i}

def stream sread.
input stream sread from /apps/snet/200910/Make_initial_topup.txt.

def stream slog.
output stream slog to /apps/snet/200910/aam_yts1790.log append.

def var lccli    as char no-undo.
def var ldamt    as dec  no-undo.
def var i        as int  no-undo.
def var j        as int  no-undo.
def var lcerror  as char no-undo.
def var lctaxzone as char no-undo.
def var lirequest as int no-undo.
def var lcline as char no-undo.
def var limsseq as int no-undo.

function flog returns logic
  (icmessage as char):

   put stream slog unformatted
      limsseq  ";"
      ldamt ";"
      icmessage skip.
 
end function.

repeat:

   import stream sread unformatted lcline.
   
   assign
      limsseq = integer(entry(1,lcline,chr(9))) 
      ldamt   = decimal(entry(9,lcline,chr(9)))
      no-error.
      
   if error-status:error or limsseq = 0 then next.
   
   i = i + 1.
      
   disp limsseq
        ldamt    format ">>>9.99".

   find first mobsub where mobsub.msseq = limsseq no-lock no-error.
   if not available mobsub or mobsub.paytype = false then do:
      flog("Unknown MSISDN").
      next.
   end.
 
   if ldamt = 0 then do:
      flog("Nothing to do").
      next.
   end.
 
   find first prepaidrequest no-lock where
              prepaidrequest.brand = "1" and
              prepaidrequest.msseq = mobsub.msseq and
              prepaidrequest.source = "web order" and
              prepaidrequest.tsrequest < 20091022 no-error.
   if not available prepaidrequest or 
      prepaidrequest.topupamt / 100 ne ldamt 
   then do:
      flog("Conflict in amount").
      next.
   end.
   disp prepaidrequest.ppreqpref
        prepaidrequest.topupamt
        prepaidrequest.vatamt.
              
   find first customer where customer.custnum = mobsub.invcust no-lock.
   lcTaxZone = fRegionTaxZone(Customer.Region).
   
   find current prepaidrequest exclusive-lock.
   prepaidrequest.ppstatus = 4.
   
   liRequest = fCreateTopUpRequest(mobsub.msseq,
                                   mobsub.cli,
                                   "RefillTRequest",
                                   "Web Order",
                                   "RefillTRequest",
                                   "997",
                                   "YTS-1789",
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


