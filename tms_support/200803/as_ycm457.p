DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCustnum AS INTEGER NO-UNDO.
DEFINE VARIABLE llSimulate AS LOGICAL NO-UNDO.

lcCLI       = "622506609".
lcCustnum   = 392373.
llSimulate  = FALSE.

/* cdr bak */
output to /apps/snet/200803/as_ycm457_prepcdr.bak.
FOR EACH prepcdr where cli = lcCLi NO-LOCK:
   export prepcdr.
END.
output close.

/* msowner fix */
output to /apps/snet/200803/as_ycm457_msowner.bak.
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
output to /apps/snet/200803/as_ycm457_msisdn.bak.
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
output to /apps/snet/200803/as_ycm457_mobsub.bak.
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
