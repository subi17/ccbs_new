def var lldisp as log no-undo.
def var llcont as log no-undo.
def var llAgrChange as log no-undo.

def stream slog.
output stream slog to /apps/snet/200911/address_check.txt.

def var i as int no-undo.
def var j as int no-undo.

for each customer no-lock where custnum > 1000:

    i = i + 1.
    if i mod 1000 = 0 then do:
       pause 0.
       disp i j with 1 down frame a.
    end.
    
    if custnum = 233718 then next. 
    
    if (index(address,"de la vega") > 0 and
        index(address,"15") > 0 and
        index(address,"avenida") = 0 and
        index(address,"avd") > 0)  or
       address = "alcobendas" then 
    do:

      lldisp = true.
      
      llcont = false.
      llAgrChange = false.
      
      for each msowner no-lock where
               agrcust = customer.custnum,
         first order no-lock where
               order.msseq = msowner.msseq,
         first ordercustomer of order no-lock where
               ordercustomer.rowtype = 1:

         if ordercustomer.address = customer.address then lldisp = false.
         if msowner.paytype = false then llcont = true.

         if can-find(first msrequest no-lock where
                           msrequest.msseq = msowner.msseq and
                           msrequest.reqtype = 10 and
                           msrequest.reqstat = 2)
         then llAgrChange = true.
      end.

      if lldisp and llcont then do: 
         pause 0.
         disp stream slog customer.custnum 
              customer.custname format "x(25)" 
              customer.address 
              llAgrChange column-label "Agr.Change" with frame b
                  50 down.
         down with frame b.
         j = j + 1.     
      end.   
   end.
end.

disp i j.


       
