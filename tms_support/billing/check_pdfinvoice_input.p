def stream sin.
def stream sout.
def temp-table ttInvoice no-undo
    field InvNum     AS INT
    FIELD ExtInvID   AS CHAR
    FIELD InvDate    AS DATE
    FIELD PrintState AS INT
    FIELD PrintDen   AS LOG
    INDEX InvNum InvNum
    INDEX InvDate InvDate DESC.

def var lcline as char no-undo.
def var i as int no-undo.
input stream sin from 
"/home/hulujan/genPDF/yot4024/PDF_INVOICES_201508.txt".
output stream sout to 
"/home/hulujan/genPDF/yot4024/PDF_INVOICES_201508.log" append.
repeat:
import stream sin unformatted lcline.
if lcline = "" or lcline = ? then next.
find first invoice where
           invoice.brand = "1" and
           invoice.extinvid = replace(entry(2,lcline,";"),'"',"")
     no-lock no-error.
if not avail invoice then do:
   put stream sout unformatted lcline ";ERROR:Invoice not found" skip.
   next.
end.    
if invoice.invtype <> 1 then do:
   put stream sout unformatted lcline ";ERROR:Invalid Invoice Type: " STRING(invoice.invtype) skip.
   next.
end.
if can-find(first ttInvoice where ttInvoice.invnum = invoice.invnum) then do:
   put stream sout unformatted lcline ";ERROR:Duplicate Invoice" skip.
   next.
end.
CREATE ttInvoice.
ASSIGN ttInvoice.InvNum = invoice.invnum
       ttInvoice.ExtInvId = invoice.extinvid
       ttInvoice.InvDate = invoice.invdate
       ttInvoice.PrintState = Invoice.PrintState
       ttInvoice.PrintDen = Invoice.InvCfg[1].
i = i + 1.
status default invoice.extinvid + "-" + string(i).
end.
input stream sin close.
output stream sout close.
def var lcfilename as char no-undo.
for each ttInvoice break by ttInvoice.InvDate:
   if first-of(ttInvoice.InvDate) then do:
      lcfilename = "/apps/yoigo/tms_support/billing/log/invoices/invoices_" +
                   string(year(ttInvoice.InvDate)) + "_" +
                   string(month(ttInvoice.InvDate)) + "_" +
                   string(day(ttInvoice.InvDate)) + ".log".
      output to value(lcfilename) append.
   end.
   put unformatted ttInvoice.ExtInvId "|" ttInvoice.invnum "|" ttInvoice.InvDate "|" ttInvoice.PrintState "|" ttInvoice.PrintDen skip.
   if last-of(ttInvoice.InvDate) then
      output close.
end.
