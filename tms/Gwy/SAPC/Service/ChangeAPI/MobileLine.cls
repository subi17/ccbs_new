 
 /*------------------------------------------------------------------------
    File        : MobileLine
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
USING Gwy.SAPC.Service.ChangeAPI.UnderMobileLine.
USING Gwy.SAPC.OrderEnum.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.ChangeAPI.MobileLine INHERITS ServiceObject: 

   CONSTRUCTOR MobileLine
      ( ioPCS  AS CLASS ProCommandOrder,
        icType AS CHARACTER ):

      SUPER(ioPCS, icType).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      aoJsonObject:Add("Action", "None").

      aoJsonObject:Add("Type", "MOBILE_LINE").
      
      aoJsonObject:AddNull("Characteristics").

      aoService = NEW UnderMobileLine(aoProCommandOrder).
      aoService:mCreateServices(aoJsonObject).

   END METHOD.

END CLASS.