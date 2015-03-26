def var i as int no-undo.
def var j as int no-undo.
def var lcforward as char no-undo.
def var lcmsrn as char no-undo.
def var linewcode as int no-undo.
def var liph1 as int no-undo.
def var liph2 as int no-undo.
def var liph3 as int no-undo.

def buffer bcdr for mobcdr.

def stream slog.
output stream slog to /apps/yoigo/tms_support/201008/aam_ydr133_cc3and4.log 
   append.


for each mobcdr no-lock use-index errorcode where
         mobcdr.errorcode = 9011 and
         lookup(string(mobcdr.spocmt),"3,4") > 0,
   first mcdrdtl2 no-lock where
         mcdrdtl2.datest = mobcdr.datest and
         mcdrdtl2.dtlseq = mobcdr.dtlseq:

   i = i + 1.
   if i mod 100 = 0 then do:
      pause 0.
      disp i j mobcdr.spocmt mobcdr.datest with 1 down.
   end.
          
   if mobcdr.datest < 7/22/10 or
      (mobcdr.datest = 7/22/10 and mobcdr.timest < 36000) then do:
      liph1 = liph1 + 1.
      next.
   end.
   
   linewcode = mobcdr.errorcode.
   
   if mobcdr.datest < 8/2/10 or
      (mobcdr.datest = 8/2/10 and mobcdr.timest < 14400) then do:
      linewcode = 9900.
      liph2 = liph2 + 1.
   end.
     
   else if mobcdr.datest > 8/2/10 or
      (mobcdr.datest = 8/2/10 and mobcdr.timest >= 14400) then do:
      linewcode = 9900.
      liph3 = liph3 + 1.
   end.
         
   if linewcode ne mobcdr.errorcode then do:
      find first bcdr where recid(bcdr) = recid(mobcdr) exclusive-lock.
      assign 
         bcdr.errorcode = linewcode
         j = j + 1.
   end.

end.

put stream slog unformatted 
   "Phase1" chr(9)   liph1 skip
   "Phase2 To Rating" chr(9)   liph2 skip
   "Phase3 To Rating" chr(9)   liph3 skip.
   
output stream slog close.

disp i j 
     liph1 liph2 liph3 .
     
