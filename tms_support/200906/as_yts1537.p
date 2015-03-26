{timestamp.i}
DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
ldeNow = fmakets().
DEFINE VARIABLE lcClis AS CHARACTER NO-UNDO. 
lcClis = "633326143 633326184 633326182 633326190 633326199 633326196 633338360 633326070 633326116 633326255 633326263 633326141 633326180 633326314 633326323 633326315".

DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCli2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCli AS int NO-UNDO. 
DEFINE VARIABLE adder AS INTEGER NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE k AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO.

output to /apps/snet/200906/order_clis.txt.

main_loop:
do i = 1 to num-entries(lcClis, " "):
   adder = 1.
   lcCli = entry(i, lcCLis, " ").
   
   do j = 1 to 6:
      lcCli2 = lcCli.
     
      do k = 0 to 9:
         substring(lcCli2,(10 - j),1,"CHARACTER") = string(k). 
        FIND FIRST msisdn NO-LOCK
            WHERE MSISDN.Brand EQ "1"
            AND msisdn.statuscode EQ 1
            AND MSISDN.ValidTo GE ldeNow 
            AND MSISDN.POS EQ "ONLINE" 
            AND msisdn.cli EQ lcCLi2 
            and msisdn.lockedto < ldeNow NO-ERROR.
 
         if avail(msisdn) then do:
            find current msisdn EXCLUSIVE-LOCK.
            assign msisdn.lockedto = ldeNow + 1.
            put unformatted lcCli "->" lcCli2 skip.   
            next main_loop.
         end.

      end.
   end.
end.
