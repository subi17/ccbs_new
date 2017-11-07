{Syst/commali.i} 

def var ldainvdate as date no-undo.
def var i as int no-undo.
def var llok as log no-undo.

pause 0.
update ldainvdate 
   format "99-99-9999" 
   label "Invoice Date" 
   help "Date of production (invoice type=1) invoices"
with overlay row 10 centered side-labels title " DELETE INVOICES " frame fDel.
hide frame fDel no-pause.

if ldainvdate = ? then return.

llok = false.
message "Start deleting production (type 1) invoices?"
view-as alert-box question
buttons yes-no
set llok.
if not llok then return.

for each invoice no-lock use-index invdate where
         invoice.brand = "1" and
         invoice.invdate = ldainvdate and
         invoice.invtype = 1:

   i = i + 1.

   pause 0.
   disp i column-label "Deleted" with overlay 1 down.
   
   RUN Inv/del_inv.p (invoice.invnum).
end.

message i "invoices deleted"
view-as alert-box title "Done".

