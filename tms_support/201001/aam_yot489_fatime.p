{Syst/testpaa.i}
Syst.Var:katun = "ari".

def stream sread.
input stream sread from /apps/snet/201001/CONTRATO_F_F_201001.txt.

def stream slog.
output stream slog to /apps/snet/201001/yot487_fatime_cpfat_25.log append.

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


assign
   lcfatgrp = "CPFAT"
   ldamt    = 25.

repeat:

   import stream sread unformatted lccli.
   
   lccli = trim(lccli).
   
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

   if lookup(msowner.clitype,"cont,cont2,cont4,cont5") = 0 then do:
      flog("Invalid subscription type").
      next.
   end.
   
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
                  "YOT-487",
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


