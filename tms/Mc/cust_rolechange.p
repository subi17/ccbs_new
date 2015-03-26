def var liCustNum as int no-undo.

find first customer no-lock.

pause 0.
form  liCustNum label "Customer" format ">>>>>>>9" skip
       Customer.CustName skip
       Customer.InvCust  skip
       Customer.AgrCust  skip
       with overlay 1 column side-labels row 10 centered frame fCust.

update liCustNum with frame fCust.

find customer where customer.custnum = liCustNum exclusive-lock no-error.
if not available customer then do:
   message "Unknown customer"
   view-as alert-box error.
end.

else do:
    disp customer.custname + " " + customer.firstname @ customer.custname
         customer.invcust
         customer.agrcust 
         with frame fCust.

    update customer.invcust customer.agrcust with frame fCust.
end. 

hide frame fCust no-pause.


