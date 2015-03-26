def var ldamt as dec no-undo.
def var ldfix as dec no-undo.
def var ldmin as dec no-undo.
def var ldsin as dec no-undo.
def var ldfat as dec no-undo.
def var i     as int no-undo.

def temp-table ttcli no-undo
   field cli as char
   field mincons as dec
   field penalty as dec
   field custnum as int
   field invnum  as char
   index cli cli.

session:numeric-format = "european".

function fcreate returns logic:
   find first ttcli where ttcli.cli = invoice.cli no-error.
   if not available ttcli then do:
      create ttcli.
      assign
         ttcli.cli = invoice.cli
         ttcli.custnum = invoice.custnum
         ttcli.invnum = invoice.extinvid.
   end.
   
end function.


for each invoice no-lock use-index invdate where
         brand = "1" and
         invdate = 8/1/8 and
         invtype = 1,
    each invrow of invoice no-lock:

   case invrow.rowtype:
   when 5 then do:
      fcreate().
      ttcli.mincons = ttcli.mincons + invrow.amt.
      ldmin = ldmin + invrow.amt.
   end.
   when 7 then ldfat = ldfat + invrow.amt.
   when 3 then ldfix = ldfix + invrow.amt.
   when 4 then do:
      if invrow.billcode = "termperiod" then do:
         fcreate().
         ttcli.penalty = ttcli.penalty + invrow.amt.
      end.
      ldsin = ldsin + invrow.amt.
   end.
   otherwise ldamt = ldamt + invrow.amt.
   end case.
   
   i = i + 1.
   
   if i mod 1000 = 0 then do:
       pause 0.
       disp i with 1 down.
   end.
end.

disp ldmin  format "->>>>>>9.999"
     ldfat  format "->>>>>>9.999"
     ldfix  format "->>>>>>9.999"
     ldsin  format "->>>>>>9.999"
     ldamt  format "->>>>>>9.999"
     (ldmin + ldfat + ldfix + ldamt)  format "->>>>>>9.999".

def stream slog.

/*
output stream slog to /tmp/rowamt.txt.

put stream slog 
   "Minimum cons: " ldmin  format "->>>>>>9.999" skip
   "Fatime      : " ldfat  format "->>>>>>9.999" skip
   "Fixed fee   : " ldfix  format "->>>>>>9.999" skip
   "Single fee  : " ldsin  format "->>>>>>9.999" skip
   "Others      : " ldamt  format "->>>>>>9.999" skip.
*/

output stream slog to /tmp/mincons_200808.txt.
put stream slog unformatted
   "MSISDN"   chr(9)
   "Customer" chr(9)
   "Invoice"  chr(9)
   "Amount"   skip.
   
for each ttcli where ttcli.mincons ne 0:
   put stream slog unformatted 
      ttcli.cli     chr(9)
      ttcli.custnum chr(9)
      ttcli.invnum  chr(9)
      ttcli.mincons skip.
end.
output stream slog close.

output stream slog to /tmp/penalty_200808.txt.
put stream slog unformatted
   "MSISDN"   chr(9)
   "Customer" chr(9)
   "Invoice"  chr(9)
   "Amount"   skip.
   
for each ttcli where ttcli.penalty ne 0:
   put stream slog unformatted 
      ttcli.cli     chr(9)
      ttcli.custnum chr(9)
      ttcli.invnum  chr(9)
      ttcli.penalty skip.
end.
output stream slog close.

session:numeric-format = "american".

