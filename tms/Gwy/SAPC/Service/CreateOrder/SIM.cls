 
 /*------------------------------------------------------------------------
    File        : SIM
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

CLASS Gwy.SAPC.Service.CreateOrder.SIM INHERITS ServiceObject: 

   CONSTRUCTOR SIM
      ( ioPCO  AS CLASS ProCommandOrder,
        icType AS CHARACTER ):

      SUPER(ioPCO, icType).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      DEFINE BUFFER IMSI FOR IMSI.
      DEFINE BUFFER SIM FOR SIM.

      FIND SIM NO-LOCK WHERE
           SIM.ICC = aoProCommandOrder:acICC
      NO-ERROR.

      IF NOT AVAILABLE SIM
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find SIM with ICC '&1'",
                                               aoProCommandOrder:acICC)).
      
      FIND FIRST IMSI NO-LOCK WHERE 
                 IMSI.ICC = SIM.ICC
      NO-ERROR.

      IF NOT AVAILABLE IMSI
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find IMSI with ICC '&1'",
                                               SIM.ICC)).

      aoJsonObject:Add("Type", "SIM").

      aoJsonObject:Add("Action",
                        IF aoProCommandOrder:aoOrderEnum EQ
                           OrderEnum:SUBSCRIPTION_TERMINATE
                        THEN "Delete"
                        ELSE "Add").

      mAddCharacteristicsArray(mNameValue("Iccid", SIM.ICC)).
      mAddCharacteristicsArray(mNameValue("Imsi", IMSI.IMSI)).
      mAddCharacteristicsArray(mNameValue("Ki", IMSI.KI)).
      aoJsonObject:Add("Characteristics", aoCharacteristicArray).

   END METHOD.

END CLASS.