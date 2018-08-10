 
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

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.MobileLineSAPC.UnderDefaultPricePlan IMPLEMENTS IService: 

   /*
      Service Hierarchy:

      MOBILE_LINE_SAPC
         DEFAULT_PRICE_PLAN (1..1)
         SIM (1..1)
         PERIODIC_DATAPLAN (0..n) (the interface supports 0..n but we have 0..1
   
      (Note: No upsells is possible here)
   */
   METHOD PUBLIC VOID mCreateServices
      ( ioJsonObject AS CLASS JsonObject,
        ioPCS        AS CLASS ProCommandSubscription ):

      DEFINE VARIABLE loJsonArray  AS CLASS JsonArray  NO-UNDO.

      loJsonArray = NEW JsonArray().
      /*
      loJsonArray:Add(NEW DefaultPricePlan(ioPCS, "DPP"):mGetData()).
      /*...*/
      */
      ioJsonObject:Add("Services", loJsonArray).
      
   END METHOD.

END CLASS.