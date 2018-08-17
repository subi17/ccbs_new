 
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
USING Gwy.SAPC.ProCommandOrder.
USING Gwy.SAPC.OrderEnum.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.CreateOrder.DefaultPricePlan INHERITS ServiceObject: 

   CONSTRUCTOR DefaultPricePlan
      ( ioPCS  AS CLASS ProCommandOrder,
        icType AS CHARACTER ):

      SUPER(ioPCS, icType).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      DEFINE BUFFER CLIType FOR CLIType.
      DEFINE BUFFER DayCampaign FOR DayCampaign.

      FIND CLIType NO-LOCK WHERE
           CLIType.Brand = "1" AND
           CLIType.CLIType = aoProCommandOrder:acCLIType
      NO-ERROR.

      IF NOT AVAILABLE CLIType
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find CLIType '&1'",
                                               aoProCommandOrder:acCLIType)).

      FIND DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = "1" AND
           DayCampaign.DCEvent = CLIType.BaseBundle
      NO-ERROR.

      IF NOT AVAILABLE DayCampaign
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find base mobile bundle '&1' which CLIType '&2' has",
                                               CLIType.BaseBundle, CLIType.CLIType)).

      aoJsonObject:Add("action",
                        IF aoProCommandOrder:aoOrderEnum EQ
                           OrderEnum:SUBSCRIPTION_TERMINATE
                        THEN "Delete"
                        ELSE "Add").

      aoJsonObject:Add("type", "DEFAULT_PRICE_PLAN").

      mAddCharacteristicsArray(mNameValue("emaDataPlan", DayCampaign.EMACode)).
      mAddCharacteristicsArray
         (mNameValue("subscriptionType",
                     STRING(aoProCommandOrder:alPayType,
                            "PREPAID/POSTPAID"))).

      aoJsonObject:Add("Characteristics", aoCharacteristicArray).

      aoJsonObject:Add("Line", mLineObject()).

   END METHOD.

END CLASS.