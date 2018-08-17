 
 /*------------------------------------------------------------------------
    File        : UnderRoot
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Gwy.SAPC.ProCommandOrder.
USING Gwy.SAPC.Service.ServiceObjectFactory.
USING Gwy.SAPC.Service.CreateOrder.MobileLineSAPC.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.CreateOrder.UnderRoot INHERITS ServiceObjectFactory: 

   /*
      Service Hierarchy:

      MOBILE_LINE_SAPC
         DEFAULT_PRICE_PLAN (1..1)
         SIM (1..1) 
         PERIODIC_DATAPLAN (0..n)
   
      (Note: No upsells is possible here)
   */
   CONSTRUCTOR UnderRoot(ioPCO AS CLASS ProCommandOrder):
      mAddObject(NEW MobileLineSAPC(ioPCO, "MLSAPC")).
   END CONSTRUCTOR.

END CLASS.