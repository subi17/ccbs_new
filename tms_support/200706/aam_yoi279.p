def stream sread.
input stream sread from /apps/snet/200706/Alist_yoi279.txt.

def stream sremlog.
output stream sremlog to /apps/snet/200706/aam_yoi279_rem.log append.

def stream slog.
output stream slog to /apps/snet/200706/aam_yoi279.log append.

def var lcline  as char no-undo.
def var lccli   as char no-undo.
def var i       as int  no-undo.
def var j       as int  no-undo.
def var limsseq as int  no-undo.
def var linosim as int  no-undo.
def var liold   as int  no-undo.
def var lldel   as log  no-undo.
def var llold   as log  no-undo.

def buffer bfee for singlefee.

def temp-table ttcli no-undo
   field cli   as char 
   field msseq as int
   index msseq msseq.

repeat:

   import stream sread unformatted lcline.
   
   do j = 1 to 3 by 2:

      if num-entries(lcline,chr(9)) < j then next. 
        
      lccli = entry(j,lcline,chr(9)).
          
      limsseq = 0.
          
      find mobsub where mobsub.cli = lccli no-lock no-error.
      if available mobsub then limsseq = mobsub.msseq.
      else for first msowner no-lock use-index cli where
                     msowner.brand = "1" and
                     msowner.cli   = lccli:
         limsseq = msowner.msseq.
      end.

      if limsseq = 0 then next.

      create ttcli.
      assign ttcli.cli   = lccli
             ttcli.msseq = limsseq.
      
      i = i + 1.
      
   end.
   
   pause 0.
   disp i column-label "Read file" with 1 down.
end.   

input  stream sread close.

i = 0.

for each singlefee no-lock where
         singlefee.brand    = "1" and 
         singlefee.billcode = "TERMPERIOD" and
         singlefee.billed   = false:
      
   i = i + 1.
  
   lldel = false.

   find first msowner where msowner.msseq = integer(singlefee.keyvalue)
      no-lock no-error.
   if not available msowner then next. 
   
   llold = false.
   
   /* terminated before this billing period */
   if msowner.tsend < 20070501 then assign 
      liold = liold + 1
      lldel = true
      llold = true.
           
   /* sim not delivered */
   else if not can-find(first ttcli where 
                              ttcli.msseq = integer(singlefee.keyvalue))
   then do:

      if available msowner and   
         not can-find(first mobcdr where mobcdr.cli = msowner.cli)
      then assign linosim = linosim + 1
                  lldel   = true.
   end.

   if lldel then do:

      put stream slog unformatted 
            msowner.cli chr(9)
            llold format "Old/No SIM" 
            skip.

      export stream sremlog singlefee.
         
      find bfee where recid(bfee) = recid(singlefee) exclusive-lock.
      delete bfee. 

   end.
   
   pause 0.
   disp i column-label "All" msowner.cli liold linosim lldel with 1 down.

end.         
    
output stream slog close.
output stream sremlog close.
