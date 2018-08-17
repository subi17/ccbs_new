 
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

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.ChangeAPI.DefaultPricePlan INHERITS ServiceObject: 

   CONSTRUCTOR DefaultPricePlan
      ( ioPCS  AS CLASS ProCommandOrder,
        icType AS CHARACTER ):

      SUPER(ioPCS, icType).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      aoJsonObject:Add("action", "None").

      aoJsonObject:Add("type", "DEFAULT_PRICE_PLAN").

      aoJsonObject:AddNull("Characteristics").

      aoJsonObject:Add("Line", mLineObject()).

   END METHOD.

END CLASS.