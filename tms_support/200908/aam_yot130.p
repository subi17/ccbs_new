{Syst/testpaa.i}
katun = "ari".

def stream sread.
input stream sread from /apps/snet/200908/FAT_BARRED_CUSTOMER.txt.

def stream slog.
output stream slog to /apps/snet/200908/aam_yot130.log append.

def var lcline   as char no-undo.
def var lccli    as char no-undo.
def var lccustid as char no-undo.
def var licust   as int  no-undo.
def var ldamt    as dec  no-undo.
def var lcfatgrp as char no-undo.
def var i        as int  no-undo.
def var j        as int  no-undo.
def var lcerror  as char no-undo.

function flog returns logic
  (icmessage as char):

   put stream slog unformatted
      lcline  ";"
      icmessage skip.
 
end function.


lcfatgrp = "COMPPRE1000".


repeat:

   import stream sread unformatted lcline.
   
   assign
      licust = 0
      ldamt  = 0
      i = i + 1.
      
   assign 
      lccli    = entry(1,lcline,";")
      lccustid = entry(2,lcline,";")
      licust   = integer(entry(3,lcline,";"))
      ldamt    = decimal(entry(4,lcline,";"))
      no-error.
      
   if error-status:error or ldamt <= 0 then next.
      
   pause 0.
   disp lccli    format "x(10)"
        lccustid format "x(10)"
        licust   format ">>>>>>>9"
        ldamt    format ">>>9.99".
        
   find first customer where customer.custnum = licust no-lock no-error.
   if not available customer then do:
      flog("ERROR:Unknown customer").
      next.
   end.

   if customer.orgid ne lccustid then do:
      flog("ERROR:Wrong customer id").
      next.
   end.
   
   find first msowner use-index invcust where
              msowner.invcust = customer.custnum and
              msowner.cli = lccli and
              msowner.paytype = false no-lock no-error.
   if not available msowner then do:
      flog("Unknown MSISDN").
      next.
   end.

   run /home/ari/work/creafat.p (msowner.invcust,
                  msowner.msseq,
                  lcfatgrp,
                  ldamt,
                  0,
                  ?,
                  200908,
                  999999,
                  OUTPUT lcError).
   if lcerror > "" then do:
      flog("FATime could not be created: " + lcerror).
   end.
   
   else do:
      flog("OK").
   end.
   
   j = j + 1.
end.

input stream sread close.
output stream slog close.

disp i j.





