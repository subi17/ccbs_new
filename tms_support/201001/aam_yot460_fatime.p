{Syst/testpaa.i}
katun = "ari".

session:numeric-format = "european".

def stream sread.
input stream sread from 
     /apps/snet/201001/yot460_haiti_fat.txt.

def stream slog.
output stream slog to /apps/snet/201001/yot459_haiti_fatime.log append.

def var lcline   as char no-undo.
def var lccli    as char no-undo.
def var ldamt    as dec  no-undo.
def var lcfatgrp as char no-undo.
def var i        as int  no-undo.
def var j        as int  no-undo.
def var lcerror  as char no-undo.

function flog returns logic
  (icmessage as char):

   put stream slog unformatted
      lccli  ";"
      icmessage skip.
 
end function.


lcfatgrp = "CPFAT".

repeat:

   import stream sread unformatted lcline.
    
   assign 
      lccli = trim(entry(1,lcline,chr(9)))
      ldamt = round(decimal(entry(2,lcline,chr(9))),2) no-error.
      
   if error-status:error then next.
   
   i = i + 1.
      
   find first msowner use-index cli_s where
              msowner.cli = lccli and
              msowner.paytype = false no-lock no-error.
   if not available msowner then do:
      if can-find(first msowner where msowner.cli = lccli) then do:
         flog("Incorrect payment type").
      end.
      else do:
         flog("Unknown MSISDN").
      end.   
      next.
   end.

   pause 0.
   disp i
        lccli    format "x(10)"
        ldamt    format ">>>9.99".

   RUN /home/ari/work/creafat.p (msowner.invcust,
                  msowner.msseq,
                  lcfatgrp,
                  ldamt,
                  0,
                  ?,
                  201001,
                  999999,
                  "YOT-459",
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

session:numeric-format = "american".

disp i j.


