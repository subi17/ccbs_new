{testpaa.i}
katun = "ari".


def stream slog.
output stream slog to /apps/snet/200903/mnp_onlysim_initial_topup_II.txt.

def var lcline  as char no-undo.
def var lccli   as char no-undo.
def var liorder as int  no-undo.
def var lireq   as int  no-undo.
def var i       as int  no-undo.
def var j       as int  no-undo.

for each mobsub no-lock by msseq desc:

    if activationdate < 3/4/9 then next.

    if paytype = false then next.

    find first order where order.msseq = mobsub.msseq no-lock.
    if order.crstamp > 20090304 then next.  

    if can-find(first prepaidrequest use-index msseq where
                     prepaidrequest.brand = "1" and
                     prepaidrequest.msseq = mobsub.msseq and
                     prepaidrequest.source = "web order")
    then next.

   i = i + 1.
   pause 0.
   disp i j with 1 down.

   put stream slog unformatted
      mobsub.cli chr(9)
      order.orderid skip.
      
   run topupcamp.p (order.msseq,
                    output lireq).
   j = j + 1.

end.

disp i j.

