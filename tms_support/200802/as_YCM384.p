DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCustnum AS INTEGER NO-UNDO.
DEFINE VARIABLE llSimulate AS LOGICAL NO-UNDO.

lcCLI       = "622463055".
lcCustnum   = 382234.
llSimulate  = FALSE.

/* msowner fix */
output to /apps/snet/200802/as_ycm384_msowner2.bak.
FOR EACH msowner where
   brand = "1" and
   cli   = lcCli and 
   custnum = lcCustnum EXCLUSIVE-LOCK:
   export msowner.
   if not llSimulate then delete msowner.
END.

FOR FIRST msowner where
   brand = "1" and
   cli   = lcCli EXCLUSIVE-LOCK:
   export msowner.
   if not llSimulate then msowner.tsend = 99999999.99999.
END.
output close.

/* msisdn fix */
output to /apps/snet/200802/as_ycm384_msisdn2.bak.
FOR FIRST msisdn where
   brand = "1" and
   cli   = lcCli and
   custnum = lcCustnum EXCLUSIVE-LOCK:
   export msisdn.
   if not llSimulate then delete msisdn.
END.

FOR first msisdn where
   brand = "1" and
   cli   = lcCli EXCLUSIVE-LOCK:
   export msisdn.
   if not llSimulate THEN
   msisdn.validto = 99999999.99999.
END.
output close.

/* mobsub fix */
output to /apps/snet/200802/as_ycm384_mobsub2.bak.
For EACH mobsub where mobsub.cli = lcCli EXCLUSIVE-LOCK:
   export mobsub.
if not llSimulate THEN
assign
   mobsub.custnum = 233718
   mobsub.invcust = 233718
   mobsub.agrcust = 233718
   mobsub.clitype = "TARJ3"
   mobsub.salesman = "YOIGO".
END.
