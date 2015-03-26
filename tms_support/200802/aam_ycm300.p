def stream sread.
input stream sread from /apps/snet/200802/deny_bill_200802.txt.

def stream slog.
output stream slog to /apps/snet/200802/aam_ycm300.log.

def var lcline as char no-undo.
def var lccli  as char no-undo.
def var i      as int  no-undo.

repeat:

   import stream sread unformatted lcline.

   lccli = right-trim(entry(1,lcline,chr(9))).
   
   if lccli = "" or lccli = "msisdn" then next.
   
   find mobsub where mobsub.cli = lccli exclusive-lock no-error.
   if not available mobsub then do:
      put stream slog unformatted
         lccli ": not found" skip.
      next.
   end.

   mobsub.repcodes = "x".

   i = i + 1.
   pause 0.
   disp i with 1 down.
end.


