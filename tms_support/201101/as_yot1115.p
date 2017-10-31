
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 19.03.10
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commpaa.i}
katun  = "anttis".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/msreqfunc.i}
{Func/orderfunc.i}

find mnpprocess where
   mnpprocess.portrequest = "00500311101230174225624" EXCLUSIVE-LOCK. 

find order where
   order.brand = "1" and
   order.orderid = mnpprocess.orderid EXCLUSIVE-LOCK.

disp order.statuscode mnpprocess.statuscode order.mnpstatus.
         
/* Cancel pending SMS messages */
FOR EACH CallAlarm WHERE
         CallAlarm.Brand = gcBrand AND
         CallAlarm.CLI = Order.CLI AND
         CallAlarm.DeliStat = 1 AND
         CallAlarm.CreditType = 12 EXCLUSIVE-LOCK:
    CallAlarm.DeliStat = 4.  /* CANCELLED */
    MESSAGE "foo" VIEW-AS ALERT-BOX.
END.
/*         
fSetOrderStatus(Order.OrderId,"7").
*/

ASSIGN         
   MNPProcess.UpdateTS = Func.Common:mMakeTS()
   MNPProcess.StatusCode = {&MNP_ST_ACAN}
   MNPProcess.statusreason = "CANC_ABONA".
   Order.MNPStatus = MNPProcess.StatusCode + 1. 

