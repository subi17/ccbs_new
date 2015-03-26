def var i       as int no-undo.
def var j       as int no-undo.
def var limonth as int no-undo.

def stream slog.

def temp-table ttamt no-undo
   field month  as int
   field cliqty as int
   field cdrqty as int
   field amt    as dec
   index month month.
   
for each invseq no-lock where
         invseq.msseq = 0:
         
   if can-find(first mobcdr where
                     mobcdr.invcust = invseq.custnum and
                     mobcdr.invseq  = invseq.invseq)
   then do:

      limonth = year(invseq.todate) * 100 + month(invseq.todate).
      
      find first ttamt where ttamt.month = limonth no-error.
      if not available ttamt then do:
         create ttamt.
         ttamt.month = limonth.
      end.
      
      ttamt.cliqty = ttamt.cliqty + 1.
      
      for each mobcdr no-lock where
               mobcdr.invcust = invseq.custnum and
               mobcdr.invseq  = invseq.invseq:
      
         assign 
            ttamt.cdrqty = ttamt.cdrqty + 1
            ttamt.amt    = ttamt.amt + mobcdr.amount.
      end.  

      j = j + 1.

   end.

   i = i + 1.

   pause 0.
   disp i j invseq.todate with 1 down.
end.

output stream slog to /apps/snet/200709/aam_yts169_V.log.

put stream slog unformatted
   "Month"    chr(9)
   "Subs.Qty" chr(9)
   "CDR Qty"  chr(9)
   "Amount"   skip.
   
for each ttamt:
   put stream slog unformatted
      ttamt.month  chr(9)
      ttamt.cliqty chr(9)
      ttamt.cdrqty chr(9)
      ttamt.amt    skip.
end.

output stream slog close.


   

