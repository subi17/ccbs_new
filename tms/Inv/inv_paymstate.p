def var liinv as int no-undo.

pause 0.
update liinv format ">>>>>>>9"
       label "Invoice"
       with side-labels overlay row 10 centered frame fpaym.

find invoice where invoice.invnum = liinv exclusive-lock.
update invoice.paymstate with frame fpaym.

hide frame fpaym no-pause.

