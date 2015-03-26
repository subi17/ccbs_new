def stream slog.
output stream slog to /tmp/premium_counter_special_limit.txt.

put stream slog unformatted
   "Customer"   chr(9)
   "Limit Nbr"  chr(9)
   "Amount"     skip.

def var i as int no-undo.
def var j as int no-undo.

for each customer no-lock use-index custnum where
         customer.brand = "1":

    i = i + 1.
    if i mod 1000 = 0 then do:
       pause 0.
       disp i j with 1  down.
    end.
    
    for each limit no-lock use-index custnum where 
             limit.custnum = customer.custnum and
             limit.tmruleseq = 1 and
             limit.defvalue = false
    by limit.limitid:

       put stream slog unformatted
          customer.custnum  chr(9)
          limit.limitid     chr(9)
          limit.limitamt    skip.
       j = j + 1.
    end.
end.

output stream slog close.

disp i j.
