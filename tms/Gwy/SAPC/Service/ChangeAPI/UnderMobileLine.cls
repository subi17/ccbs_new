 
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
USING Gwy.SAPC.OrderEnum.
USING Gwy.SAPC.Service.ServiceObjectFactory.
USING Gwy.SAPC.Service.ChangeAPI.DefaultPricePlan.
USING Gwy.SAPC.Service.ChangeAPI.PeriodicDataPlan.
USING Gwy.SAPC.Service.ChangeAPI.SIM.
USING Gwy.SAPC.Service.ChangeAPI.Upsell.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.ChangeAPI.UnderMobileLine INHERITS ServiceObjectFactory: 

   CONSTRUCTOR UnderMobileLine(ioPCO AS CLASS ProCommandOrder):

      mAddObject(NEW DefaultPricePlan(ioPCO, "DPP")).

      CASE ioPCO:aoOrderEnum:
         WHEN OrderEnum:DATAPLAN_ADD OR WHEN
              OrderEnum:DATAPLAN_DELETE OR WHEN
              OrderEnum:DATAPLAN_MODIFY
         THEN mAddObject(NEW PeriodicDataPlan(ioPCO, "PDP")).

         WHEN OrderEnum:UPSELL_ADD
         THEN mAddObject(NEW Upsell(ioPCO, "Upsell")).

         WHEN OrderEnum:CHANGE_SIM
         THEN mAddObject(NEW SIM(ioPCO, "SIM")).

      END CASE.

   END CONSTRUCTOR.

END CLASS.