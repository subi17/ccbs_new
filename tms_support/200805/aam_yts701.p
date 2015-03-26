def var i as int no-undo.

def stream sread.
input stream sread from /apps/snet/200805/dextra_missing_080515.txt.

def stream slog.
output stream slog to /apps/snet/200805/aam_yts701.log append.

def var lccli  as char no-undo.
def var llmiss as log  no-undo.

def temp-table ttcli no-undo
   field cli as char format "x(12)"
   field mark as log
   index cli cli.
   
repeat:
   import stream sread lccli.
   
   if lccli > "" then do:
      if can-find(first ttcli where ttcli.cli = lccli) then next.
      
      create ttcli.
      ttcli.cli = lccli.
      ttcli.mark = false.
      i = i + 1.
   end.
end.

message i "clis from file"
view-as alert-box.

i = 0.

for each order no-lock use-index stamp where
         order.brand = "1" and
         order.crstamp > 20080501 and
         (order.statuscode = "6" or
          order.invnum > 0) and
          order.statuscode ne "7" and
          order.statuscode ne "73",
   first orderaccessory of order no-lock where
         orderaccessory.productcode = "P023615B2":

   llmiss = false.
   
   if order.invnum = 0 then do:
      llmiss = true.
   end.
   else do:
     find invoice where invoice.invnum = order.invnum no-lock.
     if not can-find(first invrow of invoice where 
                           invrow.billcode = "P023615B2")
     then do:
        llmiss = true.
        i = i + 1.
     end.
   end.
            
   find first ttcli where ttcli.cli = order.cli no-error.
   if available ttcli then ttcli.mark = true.
   
   disp i
        order.crstamp order.orderid 
        order.statuscode
        order.paytype
        order.invnum
        available ttcli
        llmiss.

   if llmiss then do:
        
      put stream slog unformatted
         order.orderid chr(9)
         order.invnum  chr(9)
         (if order.invnum > 0 and available invoice 
          then invoice.extinvid
          else "")    skip.
   end.

end.

/*
for each ttcli where ttcli.mark = false:
    disp ttcli.
end.
*/
