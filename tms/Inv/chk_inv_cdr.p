define input parameter icOutputFile as char no-undo.
def input param ldtinv  as date no-undo.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
  "TEST chk_inv_cdr.p" SKIP(1).
def var ok as log no-undo init true.

def var i       as int  no-undo.
def var j       as int  no-undo.
def var ldamt   as dec  no-undo.
def var ldtot   as dec  no-undo.
def var liQty   as int  no-undo.
def var liMobCdr as int no-undo.

for each invoice no-lock where
         invoice.brand   = "1"     and
         invoice.invtype = 1       and
         invoice.invdate = ldtinv  and
         invoice.crinvnum = 0:
   j = j + 1.
end.         

for each invoice no-lock where
         invoice.brand   = "1"     and
         invoice.invtype = 1       and
         invoice.invdate = ldtinv  and
         invoice.crinvnum = 0:

   i = i + 1.
   if i mod 10 = 0 then do:
      pause 0.
      disp 
         (string(i) + "/" + string(j)) format "x(16)" label "Handled"
            invoice.invdate with 1 down overlay centered row 10 frame finfo.
   end.
  
   ldamt = 0.
   ldtot = 0.
   for each invrow of invoice no-lock where
            rowtype = 2:
     ldamt = ldamt + invrow.amt.
     liQty = liQty + invrow.qty.
   end.
  
   for each subinvoice of invoice no-lock,
       each mobcdr no-lock where
            mobcdr.invcust = invoice.custnum and
            mobcdr.invseq  = Subinvoice.invseq:
     ldtot = ldtot + mobcdr.amount.
     liMobCdr = liMobCdr + 1.
   end.
   
   if (truncate(ldamt,1) ne truncate(ldtot,1) and
      round(ldamt,1) ne round(ldtot,1)) or
      liQty ne liMobCdr then do:
      
      if ok then ok = false.
      
      disp stream sout invoice.invnum 
         invoice.custnum
         invoice.invdate
         invoice.invamt
         ldamt format "->>>>>>9.999"
         ldtot format "->>>>>>9.999"
         liQty
         liMobCdr
         with 1 down frame fstat 
           title "CDR check".
  end.

end.
hide frame finfo no-pause.
if ok then put stream sout "OK". 
put stream sout unformatted skip(2).
output stream sout close.
