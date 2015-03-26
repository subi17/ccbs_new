DEFINE VARIABLE liPrinted AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCredited AS INTEGER NO-UNDO. 
DEFINE VARIABLE liSentToBank AS INTEGER NO-UNDO. 
DEFINE VARIABLE liTotal AS INTEGER NO-UNDO. 

FOR EACH invoice where
         invoice.brand = "1" and
         invoice.invdate = 3/1/2011 and
         invtype = 1 NO-LOCK:

      if invoice.PrintState = 1 then liPrinted = liPrinted + 1.
      if invoice.ddstate = 1 then liSentToBank = liSentToBank + 1.
      if invoice.crinvnum > 0 then liCredited = liCredited + 1.
      
      liTotal = liTotal + 1.

end.

disp liSentToBank liPrinted liCredited liTotal.
