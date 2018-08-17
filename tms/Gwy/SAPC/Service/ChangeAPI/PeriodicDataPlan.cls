 
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
USING Gwy.SAPC.ProCommandOrder.
USING Gwy.SAPC.OrderEnum.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.ChangeAPI.PeriodicDataPlan INHERITS ServiceObject: 

   CONSTRUCTOR PeriodicDataPlan
      ( ioPCO    AS CLASS ProCommandOrder,
        icType   AS CHARACTER ):

      SUPER(ioPCO, icType).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      DEFINE BUFFER DayCampaign FOR DayCampaign.

      DEFINE VARIABLE lcAction AS CHARACTER NO-UNDO.

      FIND DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = "1" AND
           DayCampaign.DCEvent = aoProCommandOrder:ahMsRequest::ReqCParam3
      NO-ERROR.

      IF NOT AVAILABLE DayCampaign
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find bundle '&1' stored to msrequest",
                                               aoProCommandOrder:ahMsRequest::ReqCParam3)).

      CASE aoProCommandOrder:aoOrderEnum:
         WHEN OrderEnum:DATAPLAN_DELETE THEN lcAction = "Delete".
         WHEN OrderEnum:DATAPLAN_ADD    THEN lcAction = "Add".
         WHEN OrderEnum:DATAPLAN_MODIFY THEN lcAction = "Modify".
         OTHERWISE UNDO, THROW NEW AppError(SUBSTITUTE("Cannot handle type &1",
                                                       STRING(aoProCommandOrder:aoOrderEnum))).
      END CASE.

      aoJsonObject:Add("action", lcAction).

      aoJsonObject:Add("type", "PERIODIC_DATAPLAN").

      IF aoProCommandOrder:aoOrderEnum EQ OrderEnum:DATAPLAN_MODIFY
      THEN DO:
         mAddCharacteristicsArray(mNameValue("newEmaDataPlan", DayCampaign.EMACode)).
         mAddCharacteristicsArray(mNameValue("currentEmaDataPlan",
                                             aoProCommandOrder:acEMACode)).
      END.
      ELSE mAddCharacteristicsArray(mNameValue("emaDataPlan", DayCampaign.EMACode)).

      mAddCharacteristicsArray
         (mNameValue("subscriptionType",
                     STRING(aoProCommandOrder:alPayType,
                            "prepaid/postpaid"))).

      aoJsonObject:Add("Characteristics", aoCharacteristicArray).

   END METHOD.

END CLASS.