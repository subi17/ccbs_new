 
 /*------------------------------------------------------------------------
    File        : MobileLineSAPC
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING Gwy.ProCommandSubscription.
USING Gwy.SAPC.Service.IService.
USING Gwy.SAPC.Service.MobileLineSAPC.SIM.
USING Gwy.SAPC.Service.MobileLineSAPC.PeriodicDataPlan.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.MobileLineSAPC.UnderDefaultPricePlan IMPLEMENTS IService: 

   /*
      SIM (1..1)
      PERIODIC_DATAPLAN (0..n)
   
      (Note: No upsells is possible here)
   */
   METHOD PUBLIC VOID mCreateServices
      ( ioJsonObject AS CLASS JsonObject,
        ioPCS        AS CLASS ProCommandSubscription ):

      DEFINE BUFFER OrderAction FOR OrderAction.

      DEFINE VARIABLE loJsonArray  AS CLASS JsonArray  NO-UNDO.
      DEFINE VARIABLE lii AS INTEGER NO-UNDO.

      loJsonArray = NEW JsonArray().
      loJsonArray:Add(NEW SIM(ioPCS, "SIM"):ServiceObject).

      FOR EACH OrderAction NO-LOCK WHERE
               OrderAction.Brand = "1" AND
               OrderAction.OrderId = ioPCS:aiOrderId AND
               OrderAction.ItemType = "OptionalBundleItem":
         lii = lii + 1.
         loJsonArray:Add(NEW PeriodicDataPlan(ioPCS, SUBSTITUTE("PDP&1", lii), OrderAction.ItemKey):ServiceObject).
      END.

      ioJsonObject:Add("Services", loJsonArray).
      
   END METHOD.

END CLASS.