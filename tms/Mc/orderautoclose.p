/* ----------------------------------------------------------------------
  MODULE .......: orderautoclose.p
  TASK .........: Automatically close old orders in queue 44 - More
                  documentation needed (YOT-44, YOT-1754) 
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 22.01.10
  Version ......: xfera
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{timestamp.i}
{tmsconst.i}

DEFINE VARIABLE ldeCrStamp AS DECIMAL NO-UNDO.
ldeCrStamp = fMake2Dt(TODAY - 45, 0).

FOR EACH order where
   order.brand = gcbrand and
   order.statuscode = {&ORDER_STATUS_MORE_DOC_NEEDED} and
   order.crstamp < ldeCrStamp NO-LOCK:

   RUN orderbyfraud.p(order.orderid,TRUE,{&ORDER_STATUS_AUTO_CLOSED}).

END.

