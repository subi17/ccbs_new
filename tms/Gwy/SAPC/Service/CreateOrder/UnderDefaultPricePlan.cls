 
 /*------------------------------------------------------------------------
    File        : CreateOrder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Gwy.SAPC.ProCommandOrder.
USING Gwy.SAPC.Service.ServiceObjectFactory.
USING Gwy.SAPC.Service.CreateOrder.SIM.
USING Gwy.SAPC.Service.CreateOrder.PeriodicDataPlan.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.CreateOrder.UnderDefaultPricePlan INHERITS ServiceObjectFactory: 

   /*
      SIM (1..1)
      PERIODIC_DATAPLAN (0..n)
   
      (Note: No upsells is possible here)
   */
   CONSTRUCTOR UnderDefaultPricePlan(ioPCO AS CLASS ProCommandOrder):

      DEFINE BUFFER OrderAction FOR OrderAction.
      DEFINE VARIABLE lii AS INTEGER NO-UNDO.

      mAddObject(NEW SIM(ioPCO, "SIM")).
      FOR EACH OrderAction NO-LOCK WHERE
               OrderAction.Brand = "1" AND
               OrderAction.OrderId = ioPCO:aiOrderId AND
               OrderAction.ItemType = "OptionalBundleItem":
         lii = lii + 1.
         mAddObject(NEW PeriodicDataPlan(ioPCO, SUBSTITUTE("PDP&1", lii), OrderAction.ItemKey)).
      END.

   END CONSTRUCTOR.

END CLASS.