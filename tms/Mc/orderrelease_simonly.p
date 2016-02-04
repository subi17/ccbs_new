/* ----------------------------------------------------------------------
  MODULE .......: orderrelease_simonly.p
  TASK .........: Release SIM only orders from new 99 queue
  APPLICATION ..: TMS
  AUTHOR .......: jannetou
  CREATED ......: 04.06.15
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/timestamp.i}

DEF VAR liReleaseCycle AS INT NO-UNDO.
DEF VAR liCycleinSec AS INT NO-UNDO. 
DEF VAR ldtset AS DATETIME NO-UNDO.
DEF VAR ldtnow AS DATETIME NO-UNDO.
DEF VAR lidiffer AS INT NO-UNDO. 


liReleaseCycle = fCParamI("SIMonlyReleaseHours").
liCycleinSec = liReleaseCycle * 3600.

FOR EACH Order NO-LOCK WHERE
         Order.Brand = gcBrand AND
         Order.StatusCode = {&ORDER_STATUS_SIM_ONLY_MNP_IN}:


   FIND LAST OrderTimeStamp NO-LOCK WHERE
             OrderTimeStamp.Brand = gcBrand AND
             OrderTimeStamp.OrderId = Order.OrderID AND
             OrderTimeStamp.RowType = {&ORDERTIMESTAMP_SIMONLY} NO-ERROR.
   IF AVAIL OrderTimeStamp THEN DO:
      ASSIGN
         ldtset = fTimeStamp2DateTime(OrderTimeStamp.TimeStamp)
         ldtnow = fTimeStamp2DateTime(fMakeTS())
         lidiffer = INT(ldtnow - ldtset) / 1000.
      IF lidiffer >= liCycleinSec THEN
         RUN Mc/orderinctrl.p(Order.OrderId,0,TRUE).
   END.
END.
             
IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
