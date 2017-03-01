/*Program initializes timestamps so that same period can be tested several times.*/
FOR EACH actionlog  where
         actionlog.actionid eq "UpsellForAzul":
  actionlog.actionts = 20170201.
  actionlog.actionstatus = 2.
  DISP actionlog.

END.

