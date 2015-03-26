/* ----------------------------------------------------------------------
  MODULE .......: mnp_release_hold_orders.p 
  TASK .........: Release MNP On Hold orders
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 28.03.14
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{tmsconst.i}
{orderfunc.i}
{mnp.i}

DEF VAR ldeTimeStamp        AS DEC  NO-UNDO.
DEF VAR ldMNPPortingDate    AS DATE NO-UNDO.

ldeTimeStamp = fMakeTS().

FOR EACH Order WHERE
         Order.Brand = gcBrand AND
         Order.StatusCode = {&ORDER_STATUS_MNP_ON_HOLD} NO-LOCK,
   FIRST OrderCustomer OF Order WHERE
         OrderCustomer.RowType = 1 NO-LOCK:

   ldMNPPortingDate = fMNPChangeWindowDate(ldeTimeStamp,
                                           Order.OrderChannel,
                                           OrderCustomer.Region).

   IF Order.PortingDate > ldMNPPortingDate THEN NEXT.

   fSetOrderStatus(Order.OrderId,"3").

END.
