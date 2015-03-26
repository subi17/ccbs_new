def var lcline as char no-undo.
def var lccli  as char no-undo.
def var lcdeny as char no-undo.
def var llinv  as log  no-undo.
def var i      as int  no-undo.
def var ldlast as dec  no-undo.

def buffer bowner for msowner.

def stream slog.
def stream sread.

def temp-table ttcli no-undo
   field cli     as char
   field custnum as int
   field reason  as int
   index cli cli
   index reason reason cli.

   
for each invoice no-lock where
         invoice.brand   = "1" and
         invoice.invdate = 9/18/7 and
         invoice.invtype = 1:

   create ttcli.
   assign ttcli.cli     = invoice.cli
          ttcli.custnum = invoice.custnum
          ttcli.reason  = if can-find(first invrow of invoice where
                                            invrow.rowtype ne 5 and
                                            invrow.rowtype ne 7)
                          then 1
                          else 3.

   i = i + 1.
   pause 0.
   disp "Inv" i with 1 down row 1.
   
end.         

i = 0.

input stream sread from /apps/snet/200709/aam_yts169_II.log.

repeat:
   import stream sread unformatted lcline.

   lccli = entry(1,lcline,chr(9)).
   if lccli = "" then next.

   find first msowner no-lock where
              msowner.cli = lccli and
              msowner.tsend > 20070801 and
              msowner.tsbeg < 20070901 no-error.
   if not available msowner then next.

   if can-find(first ttcli where ttcli.cli = msowner.cli) then next.
   
   find customer where customer.custnum = msowner.invcust no-lock.
   if customer.invgroup = "unknown" then next.

   ldlast = msowner.tsend.
   
   for each bowner no-lock where
            bowner.msseq = msowner.msseq
   by bowner.tsend desc:
      ldlast = bowner.tsend.
      leave.
   end.   

   if ldlast < 20070802 then next.
   
   create ttcli.
   assign ttcli.cli     = msowner.cli
          ttcli.custnum = msowner.invcust
          ttcli.reason  = 1. 

   i = i + 1.
   pause 0.
   disp "Unclear" i with 1 down row 8.
   
end.

input stream sread close.

i = 0.    
   
input stream sread from /apps/snet/200709/should_notbe_barred.txt.

repeat:
   import stream sread unformatted lcline.

   lccli = entry(1,lcline,chr(9)).

   find first msowner no-lock where
              msowner.cli = lccli and
              msowner.tsend > 20070801 and
              msowner.tsbeg < 20070901 no-error.
   if not available msowner then next.

   if can-find(first ttcli where ttcli.cli = lccli) then next.
   
   lcdeny = "".
   
   find mobsub where mobsub.cli = lccli no-lock no-error.
   if available mobsub then lcdeny = mobsub.repcodes.
   
   create ttcli.
   assign ttcli.cli     = msowner.cli
          ttcli.custnum = msowner.invcust
          ttcli.reason  = if lcdeny = "x" 
                          then 2
                          else 1. 

   i = i + 1.
   pause 0.
   disp "Barred" i with 1 down row 16.
end.

input stream sread close.


output stream slog to /apps/snet/200709/aam_yts169_III.log.
 
run pPrint ("Not billed, not barred",
            1).
run pPrint ("Not billed, barred",
            2).
run pPrint ("Not billed, min.consumption",
            3).

output stream slog close.


procedure pPrint:

   def input parameter ictitle  as char no-undo.
   def input parameter iireason as int  no-undo.
   
   put stream slog unformatted
      ictitle skip.

   for each ttcli where 
            ttcli.reason = iireason:
      put stream slog unformatted
         ttcli.cli     chr(9)
         ttcli.custnum skip.
   end.
          
   put stream slog skip(2).
end.


