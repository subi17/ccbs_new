def stream slog.

def var ldtinvdate as date no-undo.
def var lcfile     as char no-undo.
def var lclogfile  as char no-undo.
def var lccli      as char no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.

def temp-table ttfind no-undo
   field cli as char.
   
assign
   lclogfile = "/tmp/find_invoices_file_result.txt"
   lcfile    = "/tmp/find_invoices_file.txt".

pause 0.
update ldtinvdate label "Invoice Date" colon 15 format "99-99-99"
       lcfile     label "Read File"    colon 15 format "x(40)"
       lclogfile  label "Result File"  colon 15 format "x(40)"
with overlay side-labels row 10 centered title " FIND INVOICES "
     frame ffind.

hide frame ffind no-pause.
     
if ldtinvdate = ? or lcfile = "" or lclogfile = "" then return.

if search(lcfile) = ? then do:
   message "File not found" view-as alert-box.
   return.
end.

input stream slog from value(lcfile).

repeat:
   import stream slog unformatted lccli.
   
   create ttfind.
   ttfind.cli = lccli.
end.

input stream slog close.

output stream slog to value(lclogfile).

put stream slog unformatted
   "MSISDN"  chr(9)
   "Subs.ID" chr(9)
   "Cust"    chr(9)
   "Invoice" skip.

for each ttfind:

    i = i + 1.
    
    put stream slog unformatted
       ttfind.cli chr(9).

    for each subinvoice no-lock use-index cli where
             subinvoice.brand = "1" and
             subinvoice.cli = ttfind.cli,
       first invoice no-lock where
             invoice.invnum = subinvoice.invnum and
             invoice.invdate = ldtinvdate and
             invoice.invtype = 1:
               
       put stream slog unformatted
          invoice.msseq chr(9)
          invoice.custnum chr(9)
          invoice.extinvid.
          
       j = j + 1.
    end.

    put stream slog skip.
    
    pause 0.
    disp i label "Checked" 
         j label "Found" 
    with overlay row 10 centered frame fqty.
    
end.

hide frame fqty no-pause.

output stream slog close.



   


