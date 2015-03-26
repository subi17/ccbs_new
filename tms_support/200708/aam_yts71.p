def stream sread.

def stream slog.
output stream slog to /apps/snet/200708/aam_yts71.log append.

def var lcline  as char no-undo.
def var lccli   as char no-undo.
def var i       as int  no-undo.
def var j       as int  no-undo.
def var llfound as log  no-undo.
def var lcres   as char no-undo.

def temp-table ttsimok no-undo
   field cli as char
   index cli cli.
   
def temp-table ttbarred no-undo
   field cli as char
   index cli cli.

def buffer bmobsub for mobsub.

   
input stream sread from /apps/snet/200708/table_a_july.txt.
  
repeat:

   import stream sread unformatted lcline.

   do j = 1 to num-entries(lcline,chr(9)):
   
      lccli = trim(entry(j,lcline,chr(9))).

      if lccli = "" then next. 
   
      if can-find(first ttsimok where ttsimok.cli = lccli) then next.
   
      create ttsimok.
      ttsimok.cli = lccli.
      
      i = i + 1.
   end.
   
   pause 0.
   disp i with 1 down.
end.
   
input stream sread close.

message i "clis in sim file"
view-as alert-box.
   
input stream sread from /apps/snet/200708/table_c_july.txt.
  
i = 0.
  
repeat:

   import stream sread unformatted lcline.

   do j = 1 to num-entries(lcline,chr(9)):
   
      lccli = trim(entry(j,lcline,chr(9))).

      if lccli = "" then next. 
   
      if can-find(first ttbarred where ttbarred.cli = lccli) then next.
   
      create ttbarred.
      ttbarred.cli = lccli.
      
      i = i + 1.
   end.
   
   pause 0.
   disp i with 1 down.
end.
   
input stream sread close.

message i "clis in barred file"
view-as alert-box.
   
assign i = 0
       j = 0.
       
for each mobsub no-lock where
         mobsub.clitype begins "cont" and
         mobsub.repcodes ne "x":
         
   i = i + 1.

   if i mod 100 = 0 then do:
      pause 0.
      disp i j with 1 down.
   end.

   find first msowner no-lock where msowner.cli = mobsub.cli.
   if msowner.tsbeg > 20070801 then next.


   /* any traffic in july */ 
   llfound = can-find(first mobcdr use-index cli where
                            mobcdr.cli     = mobsub.cli and
                            mobcdr.datest >= 7/1/7      and
                            mobcdr.datest <= 7/31/7     and
                            mobcdr.invseq > 0).

   if llfound then next.

   lcres = "".

   /* cont2 */
   if mobsub.clitype = "cont2" then 
      lcres = "cont2".
      
   /* barred */
   else if can-find(first ttbarred where ttbarred.cli = mobsub.cli) then 
      lcres = "barred".
      
   else if not can-find(first ttsimok where ttsimok.cli = mobsub.cli) then
      lcres = "no sim".
      
   if lcres > "" then do:

      find bmobsub where recid(bmobsub) = recid(mobsub) exclusive-lock.
      bmobsub.repcodes = "X".
     
      put stream slog unformatted
         mobsub.cli chr(9)
         lcres      skip.

      j = j + 1.
   end.
   
end.

disp i j.


