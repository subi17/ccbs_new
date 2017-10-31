/* ----------------------------------------------------------------------
  MODULE .......: mnp_release_orders.p 
  TASK .........: Release pending MNP orders.
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.01.12
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.CUICommon:katun = "Cron".
Syst.CUICommon:gcBrand = "1".
{Syst/tmsconst.i}
{Func/orderfunc.i}

FOR EACH Order NO-LOCK WHERE
         Order.Brand = Syst.CUICommon:gcBrand AND
         Order.StatusCode = {&ORDER_STATUS_MNP_PENDING}:
   fSetOrderStatus(Order.OrderId,"3").
END.
