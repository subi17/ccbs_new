define input parameter icOutputFile as char no-undo.
define input parameter ldtInvDate as date no-undo.
define input parameter ilDisp as log no-undo.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
   "TEST yoigo_printrep.p" skip(1).

def var liPrinted    as int no-undo.
def var liNotPrinted as int no-undo.
def var liDenied     as int no-undo.
def var i            as int no-undo.
def var lcDenied     as char no-undo.
def var liDD         as int no-undo.

if ldtinvdate = ? then return.

for each invoice no-lock where
         invoice.brand = "1" and
         invoice.invdate = ldtInvDate and
         invoice.invtype = 1:
   
   i = i + 1.

   if invoice.printstate = 0 then liNotPrinted = liNotPrinted + 1.
   else liPrinted = liPrinted + 1.
   
   if invoice.invcfg[1] then do:
      liDenied = liDenied + 1.
      if ilDisp then
         lcDenied = lcDenied + (if lcDenied > "" then "," else "") +
                    invoice.extinvid.
   end.
   if invoice.chargetype = 2 and
      invoice.ddstate   = 1 then liDD = liDD + 1.
    
end.   
      
pause 0.
      
disp stream sout i column-label "Qty"
     liPrinted label "Printed"
     liNotPrinted label "Not Printed"
     liDD label "Direct Debits"
     liDenied label "Denied" skip(1).
     
put stream sout unformatted "Denied list: " lcdenied.

put stream sout unformatted skip(2).
output stream sout close.
