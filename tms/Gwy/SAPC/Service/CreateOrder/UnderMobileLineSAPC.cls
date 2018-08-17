 
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
USING Gwy.SAPC.Service.CreateOrder.DefaultPricePlan.
USING Gwy.SAPC.Service.CreateOrder.SIM.
USING Gwy.SAPC.Service.CreateOrder.PeriodicDataPlan.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.CreateOrder.UnderMobileLineSAPC INHERITS ServiceObjectFactory: 

   /*
      SIM (1..1)
      PERIODIC_DATAPLAN (0..n)
   
      (Note: No upsells is possible here)
   */
   CONSTRUCTOR UnderMobileLineSAPC(ioPCO AS CLASS ProCommandOrder):

      DEFINE BUFFER OrderAction FOR OrderAction.
      DEFINE BUFFER TermMsRequest FOR MsRequest.
      DEFINE BUFFER SubMsRequest FOR MsRequest.
      DEFINE BUFFER DayCampaign FOR DayCampaign.

      DEFINE VARIABLE lii AS INTEGER NO-UNDO.

      mAddObject(NEW SIM(ioPCO, "SIM")).
      mAddObject(NEW DefaultPricePlan(ioPCO, "DPP")).

      CASE ioPCO:aoOrderEnum:
         WHEN OrderEnum:SUBSCRIPTION_CREATE THEN
            FOR EACH OrderAction NO-LOCK WHERE
                     OrderAction.Brand = "1" AND
                     OrderAction.OrderId = ioPCO:aiOrderId AND
                     OrderAction.ItemType = "OptionalBundleItem",
                EACH DayCampaign NO-LOCK WHERE
                     DayCampaign.Brand   = "1" AND
                     DayCampaign.DCEvent = OrderAction.ItemKey AND
                     DayCampaign.ValidTo >= TODAY AND
                     DayCampaign.EMACode > "":
               lii = lii + 1.
               mAddObject(NEW PeriodicDataPlan(ioPCO, SUBSTITUTE("PDP&1", lii), DayCampaign.EMACode)).
            END.
         WHEN OrderEnum:SUBSCRIPTION_REACTIVATE THEN DO:
            FOR
               LAST TermMsRequest NO-LOCK WHERE
                    TermMsRequest.MsSeq     = MsRequest.MsSeq AND
                    TermMsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                    TermMsRequest.ReqStatus = {&REQUEST_STATUS_DONE},
               EACH SubMsRequest NO-LOCK WHERE
                    SubMsRequest.OrigRequest = TermMsRequest.MsRequest     AND
                    SubMsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                    (SubMsRequest.ReqStatus = {&REQUEST_STATUS_DONE} OR
                     SubMsRequest.ReqStatus = {&REQUEST_STATUS_SUB_REQUEST_DONE} ),
               FIRST DayCampaign NO-LOCK WHERE
                     DayCampaign.Brand   = "1" AND
                     DayCampaign.DCEvent = SubMsRequest.ReqCParam3 AND
                     DayCampaign.ValidTo >= TODAY AND
                     DayCampaign.EMACode > "":
               lii = lii + 1.
               mAddObject(NEW PeriodicDataPlan(ioPCO, SUBSTITUTE("PDP&1", lii), DayCampaign.EMACode)).
            END.
         END.
      END CASE.

   END CONSTRUCTOR.

END CLASS.