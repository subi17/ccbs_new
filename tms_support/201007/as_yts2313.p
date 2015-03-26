input from as_yts2313.input.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcInvGrp AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcNewInvGrp AS CHARACTER NO-UNDO. 
repeat:
   import unformatted lcline.
   liCustnum = int(entry(1,lcline,"|")).
   lcInvGrp = entry(5,lcLine,"|").
   lcNewInvGrp = trim(entry(4,lcLine,"|")).
   disp liCustnum lcInvGrp "->" lcNewInvGrp. 

   do trans:
   find customer where
      customer.brand = "1" and
      customer.custnum = liCustnum and
      customer.invgroup = lcInvGrp EXCLUSIVE-LOCK.

   assign
      customer.invgroup = lcNewInvGrp.
   release customer.
   end.
end.
