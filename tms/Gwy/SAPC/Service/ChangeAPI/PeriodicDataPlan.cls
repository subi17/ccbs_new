 
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

      FIND DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = "1" AND
           DayCampaign.DCEvent = aoProCommandOrder:ahMsRequest::ReqCParam3
      NO-ERROR.

      IF NOT AVAILABLE DayCampaign
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find bundle '&1' stored to msrequest",
                                               aoProCommandOrder:ahMsRequest::ReqCParam3)).

      aoJsonObject:Add("Action",
                        IF aoProCommandOrder:aoOrderEnum EQ
                           OrderEnum:DATAPLAN_DELETE
                        THEN "Delete"
                        ELSE "Add").

      aoJsonObject:Add("Type", "PERIODIC_DATAPLAN").

      mAddCharacteristicsArray(mNameValue("emaDataPlan", DayCampaign.EMACode)).
      aoJsonObject:Add("Characteristics", aoCharacteristicArray).

   END METHOD.

END CLASS.