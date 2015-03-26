/* ----------------------------------------------------------------------
  MODULE .......: mnp_release_orders.p 
  TASK .........: Release pending MNP orders.
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.01.12
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{tmsconst.i}
{orderfunc.i}

FOR EACH Order NO-LOCK WHERE
         Order.Brand = gcBrand AND
         Order.StatusCode = {&ORDER_STATUS_MNP_PENDING}:
   fSetOrderStatus(Order.OrderId,"3").
END.
