 
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

      DEFINE BUFFER ServiceLimit FOR ServiceLimit.

      DEFINE VARIABLE ldeAmount AS DECIMAL INITIAL ? NO-UNDO.

      FOR EACH ServiceLimit NO-LOCK WHERE
               ServiceLimit.GroupCode = aoProCommandOrder:ahMsRequest::ReqCParam3
          BY ServiceLimit.ValidTo DESCENDING:

         IF ServiceLimit.ValidTo >= TODAY AND ServiceLimit.ValidFrom <= TODAY
         THEN DO:
            ldeAmount = ServiceLimit.InclAmt.
            LEAVE.
         END.
      END.

      IF ldeAmount EQ ?
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find servicelimit for upsell '&1'",
                                               aoProCommandOrder:ahMsRequest::ReqCParam3)).

      aoJsonObject:Add("action", "Add").

      aoJsonObject:Add("type", "UPSELL").

      mAddCharacteristicsArray(mNameValue("emaUnshapedLimit", STRING(ldeAmount))).

      mAddCharacteristicsArray
         (mNameValue("subscriptionType",
                     STRING(aoProCommandOrder:alPayType,
                            "prepaid/postpaid"))).

      aoJsonObject:Add("Characteristics", aoCharacteristicArray).

   END METHOD.

END CLASS.