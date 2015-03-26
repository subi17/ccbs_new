define input parameter icOutputFile as char no-undo.
def input param ldtinv  as date no-undo.

DEFINE VARIABLE ok AS LOGICAL NO-UNDO init true.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
  "TEST chk_inv_acc.p" skip(1).

def var i       as int  no-undo.
def var j       as int  no-undo.
def var k       as int  no-undo.
def var ldamt   as dec  no-undo.
def var xvatamt as dec  no-undo.
def var xvattot as dec  no-undo.
def var ldtot   as dec  no-undo.

for each invoice no-lock use-index invdate where
         invoice.brand   = "1"     and
         invoice.invdate = ldtinv:
   k = k + 1.
end.
for each invoice no-lock use-index invdate where
         invoice.brand   = "1"     and
         invoice.invdate = ldtinv:

   i = i + 1.
   if i mod 10 = 0 then do:
      pause 0.
      disp (string(i) + "/" + string(k)) format "x(16)" label "Handled"
      invoice.invdate with 1 down overlay centered row 10 frame fInfo.
   end.
  
   ldtot = 0.
   
   ldamt = Invoice.InvAmt - 
              Invoice.Rounding -
              Invoice.InterestAmt -
              Invoice.AdvPaym -
              Invoice.OverPaym.

      xVatTot = 0.
      do j = 1 to 10:
         assign ldamt   = ldamt - Invoice.VATAmount[j]
                xVatTot = xVatTot + Invoice.VatAmount[j].
      end.

      for each InvRow of Invoice no-lock where
               InvRow.VATPerc = 0:
         ldamt = ldamt - InvRow.Amt.
      end.
   
      for each InvRow of Invoice no-lock where
               InvRow.VATPerc > 0
      break by InvRow.VatPerc
           by InvRow.Amt:

         /* if VAT is included then remove it */
        xVatAmt = 0.
      
         if Invoice.VatIncl then do:

            if last(InvRow.Amt) then 
               assign xVatAmt = xVatTot
                      xVatTot = 0.
            else assign xVatAmt = round(InvRow.Amt * InvRow.VATPerc
                                        / (100 + InvRow.VATPerc),3)
                        xVatTot = xVatTot - xVatAmt.
        end.

        ldamt = ldamt - (InvRow.Amt - xVatAmt).
     end.
    
     if ldamt >= -0.005 and ldamt <= 0.005 then ldamt = 0.
 
     ldtot = ldtot + ldamt.

  if ldtot ne 0 then do:
      if ok then ok = false.
      disp stream sout  
         invoice.invnum skip
         invoice.custnum skip
         invoice.invdate skip
         invoice.invamt skip
         ldtot format "->>>>>>9.999"
         with 1 down frame fstat 
           title "Accounting check".
  end.

end.

hide frame fInfo no-pause.
if ok then put stream sout "OK". 
put stream sout unformatted skip(2).
output stream sout close.
