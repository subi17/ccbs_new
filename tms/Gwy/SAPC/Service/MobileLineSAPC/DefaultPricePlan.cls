 
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
USING Gwy.SAPC.Service.ServiceObject.
USING Gwy.ProCommandSubscription.
USING Gwy.SAPC.Service.MobileLineSAPC.UnderDefaultPricePlan.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.MobileLineSAPC.DefaultPricePlan INHERITS ServiceObject: 

   CONSTRUCTOR DefaultPricePlan
      ( ioPCS  AS CLASS ProCommandSubscription,
        icType AS CHARACTER ):

      SUPER(ioPCS, icType).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      DEFINE BUFFER CLIType FOR CLIType.
      DEFINE BUFFER DayCampaign FOR DayCampaign.

      FIND CLIType NO-LOCK WHERE
           CLIType.Brand = "1" AND
           CLIType.CLIType = aoProCommandSubscription:ahOrder::CLIType
      NO-ERROR.

      IF NOT AVAILABLE CLIType
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot CLIType '&1' which order has",
                                               aoProCommandSubscription:ahOrder::CLIType)).

      FIND DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = "1" AND
           DayCampaign.DCEvent = CLIType.BaseBundle
      NO-ERROR.

      IF NOT AVAILABLE DayCampaign
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find base mobile bundle '&1' which CLIType '&2' has",
                                               CLIType.BaseBundle, CLIType.CLIType)).


      ServiceObject:Add("Type", "DEFAULT_PRICE_PLAN").

      mAddCharacteristicsArray(mNameValue("emaDataPlan", DayCampaign.EMACode)).
      mAddCharacteristicsArray
         (mNameValue("subscriptionType",
                     STRING(aoProCommandSubscription:ahOrder::PayType,
                            "prepaid/postpaid"))).

      ServiceObject:Add("Characteristics", aoCharacteristicArray).

      ServiceObject:Add("Line", mLineObject()).

      aoService = NEW UnderDefaultPricePlan().
      aoService:mCreateServices(ServiceObject, aoProCommandSubscription).

   END METHOD.

END CLASS.