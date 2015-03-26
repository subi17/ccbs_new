def var liinvnum as int no-undo.

pause 0.
update liinvnum format ">>>>>>>>>>9" label "Internal Number" skip(1)
with side-labels.

find first invoice no-lock where invoice.invnum = liinvnum.

disp 
     invoice.extinvid skip
     invoice.custnum  skip
     invoice.invdate  skip
     invoice.invtype  skip
     invoice.invamt.