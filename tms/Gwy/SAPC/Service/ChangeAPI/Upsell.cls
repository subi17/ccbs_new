 
 /*------------------------------------------------------------------------
    File        : Upsell
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : dpastrana
    Created     : Mon Aug 20 08:00:00 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Gwy.SAPC.Service.ServiceObject.
USING Gwy.SAPC.ProCommandOrder.
USING Gwy.SAPC.OrderEnum.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.ChangeAPI.Upsell INHERITS ServiceObject: 

   CONSTRUCTOR Upsell
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
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find upsell '&1' stored to msrequest",
                                               aoProCommandOrder:ahMsRequest::ReqCParam3)).

      CASE aoProCommandOrder:aoOrderEnum:
         WHEN OrderEnum:UPSELL_ADD    THEN lcAction = "Add".
         OTHERWISE UNDO, THROW NEW AppError(SUBSTITUTE("Cannot handle type &1",
                                                       STRING(aoProCommandOrder:aoOrderEnum))).
      END CASE.

      aoJsonObject:Add("action", lcAction).

      aoJsonObject:Add("type", "UPSELL").

      mAddCharacteristicsArray(mNameValue("emaUnshapedLimit", DayCampaign.EMACode)).

      aoJsonObject:Add("Characteristics", aoCharacteristicArray).

   END METHOD.

END CLASS.