 
 /*------------------------------------------------------------------------
    File        : UnderRoot
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING Gwy.SAPC.ProCommandOrder.
USING Gwy.SAPC.Service.IService.
USING Gwy.SAPC.Service.MobileLineSAPC.DefaultPricePlan.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.MobileLineSAPC.UnderRoot IMPLEMENTS IService: 

   /*
      Service Hierarchy:

      MOBILE_LINE_SAPC
         DEFAULT_PRICE_PLAN (1..1)
         SIM (1..1) 
         PERIODIC_DATAPLAN (0..n)
   
      (Note: No upsells is possible here)
   */
   METHOD PUBLIC VOID mCreateServices
      ( ioJsonObject AS CLASS JsonObject,
        ioPCO        AS CLASS ProCommandOrder ):

      DEFINE VARIABLE loJsonArray  AS CLASS JsonArray  NO-UNDO.

      loJsonArray = NEW JsonArray().
      
      loJsonArray:Add(NEW DefaultPricePlan(ioPCO, "DPP"):ServiceObject).

      ioJsonObject:Add("Services", loJsonArray).
      
   END METHOD.

END CLASS.