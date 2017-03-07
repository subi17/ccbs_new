/* ----------------------------------------------------------------------
  MODULE .......: mnp_release_hold_orders.p 
  TASK .........: Release MNP On Hold orders
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 28.03.14
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/orderfunc.i}
{Mnp/mnp.i}
{Mm/fbundle.i}

DEF VAR ldeTimeStamp        AS DEC  NO-UNDO.
DEF VAR ldMNPPortingDate    AS DATE NO-UNDO.
DEF VAR lcProduct           AS CHAR NO-UNDO. 
DEF VAR lcTariffType        AS CHAR NO-UNDO.
DEF VAR lcSIMonlyMNP        AS CHAR NO-UNDO.
DEF VAR liReleaseCycle      AS INT NO-UNDO.
DEF VAR ldeTimeSIMMNP       AS DEC  NO-UNDO.
DEF VAR ldeUsedTimeStamp    AS DEC  NO-UNDO.

ldeTimeStamp = fMakeTS().

ASSIGN
   lcSIMonlyMNP   = TRIM(fCParamC("SIMonlyMNPorder"))
   liReleaseCycle = fCParamI("SIMonlyReleaseHours")   /* Hold hours in state 99 */
   liReleaseCycle = liReleaseCycle + 1                /* Add one extra hour */
   ldeTimeSIMMNP  = fOffSetTS(liReleaseCycle).        /* Count time to liReleaseCycle value */

FOR EACH Order WHERE
         Order.Brand = gcBrand AND
         Order.StatusCode = {&ORDER_STATUS_MNP_ON_HOLD} NO-LOCK,
   FIRST OrderCustomer OF Order WHERE
         OrderCustomer.RowType = 1 NO-LOCK:

   FIND FIRST OrderAccessory NO-LOCK WHERE
              OrderAccessory.Brand = gcBrand AND
              OrderAccessory.OrderId = Order.OrderID AND
              OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-ERROR.
   IF AVAIL OrderAccessory THEN
      lcProduct = "T".
   ELSE
      lcProduct = "S".
   
   lcTariffType = fGetDataBundleInOrderAction(Order.OrderId,
                                                    Order.CLIType).
   IF lcTariffType = "" THEN
      lcTariffType = Order.CLIType.

   /* YTS-7322 Correction. MNP in status 99 hold extra time. 
      See ordersender.i  and mnp_release_hold_orders.p */
   IF lcSIMonlyMNP EQ "true" AND
      Order.OrderType = 1 AND
      Order.CrStamp >= 20150616.40200 AND
      Order.MNPStatus = 1 AND
      LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 AND
      NOT CAN-FIND(FIRST OrderAccessory NO-LOCK WHERE
                    OrderAccessory.Brand = gcBrand AND
                    OrderAccessory.OrderId = Order.OrderID) AND
      NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                    OrderTimeStamp.Brand   = gcBrand  AND
                    OrderTimeStamp.OrderID = Order.OrderID AND
                    OrderTimeStamp.RowType = {&ORDERTIMESTAMP_SIMONLY}) THEN DO:
      ldeUsedTimeStamp = ldeTimeSIMMNP.   /* Use longer time because hold in state 99 */
   END.
   ELSE DO:
      ldeUsedTimeStamp = ldeTimeStamp.    /* Use current time */
   END.
    /* YTS-7322 END */

   ldMNPPortingDate = fMNPChangeWindowDate(ldeUsedTimeStamp,
                                           Order.OrderChannel,
                                           OrderCustomer.Region,
                                           lcProduct,
                                           lcTariffType).

   IF Order.PortingDate > ldMNPPortingDate THEN NEXT.

   fSetOrderStatus(Order.OrderId,"3").

END.
