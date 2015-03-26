def var i as int no-undo.

def buffer bsub for mobsub.

for each mobsub no-lock where salesman = "pre-act":


    find bsub where recid(bsub) = recid(mobsub) exclusive-lock.
    bsub.salesman = "YOIGO".
    
    find first order where order.msseq = bsub.msseq exclusive-lock.
    order.salesman = bsub.salesman.
    order.orderchannel = "PRE-ACT".
    
    i = i + 1.

    if i mod 1000 = 0 then do:
       pause 0.
       disp i with 1 down.
   end.
end.

disp i.
