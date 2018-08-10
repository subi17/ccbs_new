 
 /*------------------------------------------------------------------------
    File        : DefaultPricePlan
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING Gwy.SAPC.Service.ServiceObject.
USING Gwy.ProCommandSubscription.
USING Gwy.SAPC.OrderEnum.
USING Gwy.SAPC.Service.MobileLineSAPC.UnderDefaultPricePlan.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.MobileLineSAPC.DefaultPricePlan INHERITS ServiceObject: 

   CONSTRUCTOR DefaultPricePlan
      ( ioPCS  AS CLASS ProCommandSubscription,
        icType AS CHARACTER ):

      SUPER(ioPCS, icType).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      mAddCharacteristicsArray(mNameValue("emaDataPlan", "mikä value?")). 
      mAddCharacteristicsArray(mNameValue("subscriptionType", "prepaid/postpaid")). /* order.... */
     
      ServiceObject:Add("Characteristics", aoCharacteristicArray).
      ServiceObject:Add("Action",
                        IF aoProCommandSubscription:aoOrderEnum EQ
                           OrderEnum:TERMINATION
                        THEN "Delete"
                        ELSE "Add").
      ServiceObject:Add("Type", "DEFAULT_PRICE_PLAN").

      aoService = NEW UnderDefaultPricePlan().
      aoService:mCreateServices(ServiceObject, aoProCommandSubscription).

   END METHOD.

END CLASS.