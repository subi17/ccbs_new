 
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
USING Gwy.SAPC.Service.ChangeAPI.MobileLine.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.ChangeAPI.UnderRoot INHERITS ServiceObjectFactory: 

   /*
      Service Hierarchy:
      DEFAULT_PRICE_PLAN
         SIM (optional)
         PERIODIC_DATAPLAN (optional)
   */
   CONSTRUCTOR UnderRoot(ioPCO AS CLASS ProCommandOrder):
      mAddObject(NEW MobileLine(ioPCO, "ML")).
   END CONSTRUCTOR.

END CLASS.