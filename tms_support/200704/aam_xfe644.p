def stream sread.
input stream sread from /apps/snet/200704/aam_xfe644.txt.

def stream slog.
output stream slog to /apps/snet/200704/aam_xfe644.log.

def var lcline  as char no-undo.
def var lccli   as char no-undo.
def var i       as int  no-undo.
def var llfound as log  no-undo.
def var lcres   as char no-undo.

def temp-table ttsub no-undo
   field cli as char
   index cli cli.
   
put stream slog unformatted
   "MSISDN"  chr(9)
   "Result"  skip.
  
repeat:

   import stream sread unformatted lcline.

   lccli = trim(entry(2,lcline,chr(9))).

   if lccli = "" then next. 
   
   disp lccli.

   if can-find(first ttsub where ttsub.cli = lccli) then next.
   
   create ttsub.
   ttsub.cli = lccli.
   
   find mobsub where mobsub.cli = lccli no-lock no-error.
   if not available mobsub then lcres = "Subscription not available".
   
   else do:
      
      i = i + 1.
      disp i mobsub.msstat mobsub.repcodes.

      llfound = can-find(first mobcdr use-index cli where
                               mobcdr.cli     = mobsub.cli and
                               mobcdr.datest >= 3/1/7      and
                               mobcdr.datest <= 3/31/7     and
                               mobcdr.invseq > 0).

      if llfound then lcres = "Traffic found".
      
      else do:
         find current mobsub exclusive-lock.
         assign 
            mobsub.repcodes = "X"
            lcres           = "Billing denied".
      end.
   end.
     
   put stream slog unformatted
      lccli   chr(9)
      lcres   skip.
     
end.

