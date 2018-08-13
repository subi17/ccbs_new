 
 /*------------------------------------------------------------------------
    File        : PeriodicDataPlan
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

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.MobileLineSAPC.PeriodicDataPlan INHERITS ServiceObject: 

   DEFINE PRIVATE VARIABLE acBundle AS CHARACTER NO-UNDO.

   CONSTRUCTOR PeriodicDataPlan
      ( ioPCS    AS CLASS ProCommandSubscription,
        icType   AS CHARACTER,
        icBundle AS CHARACTER ):

      SUPER(ioPCS, icType).
      acBundle = icBundle.

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      DEFINE BUFFER DayCampaign FOR DayCampaign.

      FIND DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = "1" AND
           DayCampaign.DCEvent = acBundle
      NO-ERROR.

      IF NOT AVAILABLE DayCampaign
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find OptionalBundleItem '&1'",
                                               acBundle)).

      ServiceObject:Add("Type", "PERIODIC_DATAPLAN").

      mAddCharacteristicsArray(mNameValue("emaDataPlan", DayCampaign.EMACode)).
      ServiceObject:Add("Characteristics", aoCharacteristicArray).

   END METHOD.

END CLASS.