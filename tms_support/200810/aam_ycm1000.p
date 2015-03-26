def stream sread.
input stream sread from /apps/snet/200810/deny_billing_081027.txt.

def stream slog.
output stream slog to /apps/snet/200810/aam_ycm1000.log.

put stream slog unformatted
   "MSISDN"   chr(9)
   "Subs.ID"  chr(9)
   "Agr.Customer" skip.

def var lcline    as char no-undo.
def var lccli     as char no-undo.
def var limsseq   as int  no-undo.
def var liagrcust as int  no-undo.
def var i         as int  no-undo.
def var j         as int  no-undo.

def temp-table ttcurr no-undo
   field cli   as char 
   field msseq as int
   field agrcust as int
   index cli cli msseq.
   
for each actionlog no-lock where
         actionlog.brand = "1" and
         actionlog.actionid = "denybill" and
         actionlog.actionstatus = 0 and
         actionlog.todate >= 10/1/8 and
         actionlog.fromdate <= 10/31/8:
         
   liagrcust = 0.
   
   for first msowner no-lock where
             msowner.msseq = integer(ActionLog.KeyValue) and
             msowner.invcust = actionlog.custnum and
             msowner.cli = actionlog.actionchar:
      liagrcust = msowner.agrcust.       
   end.                  

   create ttcurr.
   assign
      ttcurr.cli     = actionlog.actionchar
      ttcurr.msseq   = integer(ActionLog.KeyValue)
      ttcurr.agrcust = liagrcust.
      
   i = i + 1.
   if i mod 100 = 0 then do:
      pause 0.
      disp i "curr" with 1 down.
   end.
end.
   
i = 0.

   
repeat:

   import stream sread unformatted lcline.

   limsseq = 0.
   
   assign 
      lccli     = entry(1,lcline,chr(9))
      limsseq   = integer(entry(9,lcline,chr(9)))
      liagrcust = integer(entry(6,lcline,chr(9)))
      no-error.

   if error-status:error then next. 

   i = i + 1.
   
   find first msowner where
              msowner.msseq = limsseq and
              msowner.cli   = lccli   and
              msowner.agrcust = liagrcust no-lock no-error.
   if not available msowner then do:
   
      put stream slog unformatted
         lccli     chr(9)
         limsseq   chr(9)
         liagrcust chr(9)
         "Subscr. not found" skip.
      j = j + 1.   
      next.
   end.

   find first ttcurr where 
              ttcurr.cli = lccli and
              ttcurr.msseq = limsseq no-error.
   if not available ttcurr then do:
      put stream slog unformatted
         lccli     chr(9)
         limsseq   chr(9)
         liagrcust chr(9)
         "Current deny not found" skip.
      j = j + 1.   
   end.        
   
   else for each ttcurr where ttcurr.cli = lccli:
      delete ttcurr.
   end.
   
   if i mod 100 = 0 then do:
      pause 0.
      disp i j with 1 down.
   end.
end.

for each ttcurr:

   put stream slog unformatted
      ttcurr.cli     chr(9)
      ttcurr.msseq   chr(9)
      ttcurr.agrcust chr(9)
      "Not in yoigo list" skip.
end.   

input stream sread close.
output stream slog close.

disp i j.

