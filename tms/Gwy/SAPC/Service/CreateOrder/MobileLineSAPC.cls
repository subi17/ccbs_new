 
 /*------------------------------------------------------------------------
    File        : MobileLineSAPC
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
USING Gwy.SAPC.Service.CreateOrder.UnderMobileLineSAPC.
USING Gwy.SAPC.OrderEnum.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.CreateOrder.MobileLineSAPC INHERITS ServiceObject: 

   CONSTRUCTOR MobileLineSAPC
      ( ioPCS  AS CLASS ProCommandOrder,
        icType AS CHARACTER ):

      SUPER(ioPCS, icType).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE VOID mFetchData():

      aoJsonObject:Add("action", "None").

      aoJsonObject:Add("type", "MOBILE_LINE_SAPC").
      
      aoJsonObject:AddNull("Characteristics").

      aoService = NEW UnderMobileLineSAPC(aoProCommandOrder).
      aoService:mCreateServices(aoJsonObject).

   END METHOD.

END CLASS.