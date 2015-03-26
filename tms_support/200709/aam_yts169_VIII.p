def var lcline as char no-undo.
def var lccli  as char no-undo.
def var lcdeny as char no-undo.
def var llinv  as log  no-undo.
def var i      as int  no-undo.
def var j      as int  no-undo.

def buffer bowner for msowner.

def stream slog.
def stream sread.

i = 0.    
   
input stream sread from /apps/snet/200709/should_notbe_barred.txt.
output stream slog to /apps/snet/200709/aam_yts169_VIII.log append.

repeat:
   import stream sread unformatted lcline.

   lccli = entry(1,lcline,chr(9)).

   find mobsub where mobsub.cli = lccli exclusive-lock no-error.
   if available mobsub and mobsub.repcodes = "x" then do:
   
      put stream slog unformatted 
         mobsub.cli skip.
         
      mobsub.repcodes = "".
      j = j + 1.
   end.
   
   i = i + 1.
   pause 0.
   disp i j with 1 down.
end.

input stream sread close.
output stream slog close.


