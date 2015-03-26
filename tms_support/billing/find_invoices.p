{timestamp.i}

def stream slog.

def var ldtinvdate as date no-undo.
def var lclogfile  as char no-undo.
def var lcclitype  as char no-undo.
def var lcbillcode as char no-undo.
def var liqty      as int  no-undo.
def var ldfromper  as dec  no-undo.
def var ldtoper    as dec  no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.
def var lctype     as char no-undo.
def var lccli      as char no-undo.
def var limsseq    as int  no-undo.
def var ldrowperf  as dec  no-undo.
def var ldrowpert  as dec  no-undo.

assign
   lclogfile = "/tmp/find_invoices_result.txt"
   liqty     = 50.

pause 0.
update ldtinvdate label "Invoice Date" colon 15 format "99-99-99"
       lcclitype  label "CLI Type"     colon 15 format "x(16)"
       lcbillcode label "Billing Item" colon 15 format "x(16)"
       liqty      label "Pick"         colon 15 format ">>>>>>9" skip(1)
       lclogfile  label "Result File"  colon 15 format "x(40)"
with overlay side-labels row 10 centered title " FIND INVOICES "
     frame ffind.

hide frame ffind no-pause.
     
if ldtinvdate = ? or lclogfile = "" then return.

output stream slog to value(lclogfile).

put stream slog unformatted
   "Invoice"  chr(9)
   "Inv.Date" chr(9)
   "Cust"     chr(9)
   "MSISDN"   chr(9)
   "Subs.ID"  chr(9)
   "CLI Type" chr(9)
   "Billing Item" skip.

for each invoice no-lock use-index invdate where
         invoice.brand   = "1" and
         invoice.invdate = ldtinvdate and
         invoice.invtype = 1:

    i = i + 1.

    if i mod 100 = 0 then do:
       pause 0.
       disp i label "Checked" 
            j label "Found" 
       with overlay row 10 centered frame fqty.
    end.

    assign
       ldfromper = fmake2dt(invoice.fromdate,1)
       ldtoper   = fmake2dt(invoice.todate,86399)
       lccli     = ""
       lctype    = ""
       limsseq   = 0.
       
    for each subinvoice of invoice no-lock,
        each invrow of invoice no-lock where
             invrow.subinvnum = subinvoice.subinvnum and
             (if lcbillcode > "" 
              then invrow.billcode = lcbillcode
              else true):
                  
       if invrow.fromdate ne ? and invrow.todate ne ? then assign
          ldrowperf = fmake2dt(invrow.fromdate,1)
          ldrowpert = fmake2dt(invrow.todate,86399).
       else assign
          ldrowperf = ldfromper
          ldrowpert = ldtoper.
              
       for each msowner no-lock where
                msowner.cli = subinvoice.cli and
                msowner.invcust = invoice.custnum and
                msowner.tsend > ldrowperf and
                msowner.tsbeg < ldrowpert:
           
          if lcclitype > "" and msowner.clitype ne lcclitype then next.
       
          assign 
             lctype = msowner.clitype
             lccli  = subinvoice.cli
             limsseq = subinvoice.msseq.
          leave.   
       end.

       if lctype > "" then leave.
    end.   

    if lctype = "" then next.

    put stream slog unformatted
       invoice.extinvid chr(9)
       invoice.invdate  chr(9)
       invoice.custnum  chr(9)
       lccli            chr(9)
       limsseq          chr(9)
       lctype           chr(9)
       lcbillcode       skip.
          
    j = j + 1.
    
    if j >= liqty then leave. 
end.

hide frame fqty no-pause.

output stream slog close.



   


