def var i as int no-undo.
def var j as int no-undo.

def buffer bcust for customer.
def buffer binv for invoice.

for each customer no-lock:

    i = i + 1.

    if customer.deltype = 0 then do:
        j = j + 1.
        
        find first bcust where recid(bcust) = recid(customer) exclusive-lock.
        bcust.deltype = 1.
    end.

    if i mod 1000 = 0 then do:
       pause 0.
       disp i j with 1  down.
    end.
end.

disp i j.
pause message "start invoices".

i = 0.
j = 0.

for each invoice no-lock use-index invdate where
         invoice.brand = "1" and
         invoice.invdate >= 10/1/9 and
         invoice.invtype = 1:

    i = i + 1.

    if invoice.deltype = 0 then do:
        j = j + 1.
        
        find first binv where recid(binv) = recid(invoice) exclusive-lock.
        binv.deltype = 1.
    end.

    if i mod 1000 = 0 then do:
       pause 0.
       disp i j with 1  down.
    end.
end.

disp i j.
