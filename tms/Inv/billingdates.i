
/* ----------------------------------------------------------------------
  MODULE .......: billingdates.i 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: timok 
  CREATED ......: 12.02.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */


ldfrom = YEAR(ttBInv.fromdate) * 10000 +
         MONTH(ttBInv.fromdate) * 100 +
         DAY(ttBInv.fromdate).

ldto  = YEAR(ttBInv.todate) * 10000 +
        YEAR(ttBInv.todate) * 100 +
        YEAR(ttBInv.todate) + 0.86399.


