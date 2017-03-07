/* Terminal creation for specified orders */
{Syst/commpaa.i}
{Syst/tmsconst.i}
{Func/fsubsterminal.i}
gcbrand = "1".

DEF VAR lcOrders AS CHAR NO-UNDO.
DEF VAR liLoop AS INT NO-UNDO.

/* Add order numbers here as list. , as separator. */
lcOrders = "70012108".

FUNCTION fCreateTerminals RETURNS LOGICAL (INPUT icOrders AS CHAR).

   DO liLoop = 1 TO NUM-ENTRIES(icOrders):
      FIND FIRST Order WHERE 
                 Order.brand EQ "1" AND
                 Order.orderid EQ INT(ENTRY(liLoop, lcOrders)) 
                 NO-LOCK NO-ERROR.
      IF NOT AVAIL order THEN NEXT.
      DO TRANSACTION:
         CREATE OrderAccessory.
         ASSIGN
            OrderAccessory.OrderId       = INT(ENTRY(liLoop, icOrders))
            OrderAccessory.TerminalType  = {&TERMINAL_TYPE_PHONE}
            OrderAccessory.brand         = "1"
            OrderAccessory.IMEI          = "111122223333"
            OrderAccessory.discount      = 0
            OrderAccessory.Model         = "Q25Phone"
            OrderAccessory.Manufacturer  = "Qvantel"
            OrderAccessory.ModelColor    = "Black"
            OrderAccessory.HardBook      = 1
            OrderAccessory.ProductCode   = "P034S6EN2".
         RELEASE OrderAccessory.
      END.
      fCreateSubsTerminal(BUFFER order).

   END.
END.


fCreateTerminals(lcOrders).
