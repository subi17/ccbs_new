 
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

CLASS Gwy.SAPC.Service.CreateOrder.PeriodicDataPlan INHERITS ServiceObject: 

   DEFINE PRIVATE VARIABLE acEMACode AS CHARACTER NO-UNDO.

   CONSTRUCTOR PeriodicDataPlan
      ( ioPCO     AS CLASS ProCommandOrder,
        icType    AS CHARACTER,
        icEMACode AS CHARACTER ):

      SUPER(ioPCO, icType).
      acEMACode = icEMACode.

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      aoJsonObject:Add("action",
                        IF aoProCommandOrder:aoOrderEnum EQ
                           OrderEnum:SUBSCRIPTION_TERMINATE
                        THEN "Delete"
                        ELSE "Add").

      aoJsonObject:Add("type", "PERIODIC_DATAPLAN").

      mAddCharacteristicsArray(mNameValue("emaDataPlan", acEMACode)).
      aoJsonObject:Add("Characteristics", aoCharacteristicArray).

   END METHOD.

END CLASS.