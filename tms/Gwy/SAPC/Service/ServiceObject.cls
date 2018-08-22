 
 /*------------------------------------------------------------------------
    File        : ServiceObject
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Json.ObjectModel.*.
USING Gwy.SAPC.Service.IService.
USING Gwy.SAPC.ProCommandOrder.
USING Gwy.SAPC.OrderEnum.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.ServiceObject ABSTRACT: 

   DEFINE PROTECTED VARIABLE aoCharacteristicArray AS CLASS JsonArray       NO-UNDO.
   DEFINE PROTECTED VARIABLE aoProCommandOrder     AS CLASS ProCommandOrder NO-UNDO.
   DEFINE PROTECTED VARIABLE aoService             AS CLASS IService        NO-UNDO.
   
   DEFINE PUBLIC PROPERTY aoJsonObject  AS CLASS JsonObject   NO-UNDO
      GET.
      PROTECTED SET.
   
   DEFINE PROTECTED VARIABLE acServiceID              AS CHARACTER          NO-UNDO.

   CONSTRUCTOR ServiceObject
      ( ioPCO  AS CLASS ProCommandOrder,
        icType AS CHARACTER ):
      aoProCommandOrder = ioPCO.
      aoJsonObject = NEW JsonObject().
      aoJsonObject:Add("serviceID", SUBSTITUTE("&1_&2", ioPCO:aiProCommandId, icType)).
      mFetchData().

   END CONSTRUCTOR.

   DESTRUCTOR ServiceObject():
      IF VALID-OBJECT(aoJsonObject)
      THEN DELETE OBJECT aoJsonObject.

      IF VALID-OBJECT(aoCharacteristicArray)
      THEN DELETE OBJECT aoCharacteristicArray.
      
      IF VALID-OBJECT(aoService)
      THEN DELETE OBJECT aoService.

      aoProCommandOrder = ?.
   END DESTRUCTOR.

   METHOD PROTECTED JsonObject mNameValue
      ( icName  AS CHARACTER,
        icValue AS CHARACTER ):

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("name", icName).
      loJsonObject:Add("value", icValue).

      RETURN loJsonObject.

   END METHOD.

   METHOD PROTECTED VOID mAddCharacteristicsArray
      ( ioJsonObject AS JsonObject ):
      IF NOT VALID-OBJECT(aoCharacteristicArray)
      THEN aoCharacteristicArray = NEW JsonArray().
      aoCharacteristicArray:Add(ioJsonObject).
   END METHOD.

   METHOD PROTECTED JsonObject mLineObject():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("phoneNumber", aoProCommandOrder:acCLI).

      IF aoProCommandOrder:acDonorOperator > ""
      THEN DO:
         loJsonObject:Add("portabilityType", "Portabilidad").
         loJsonObject:Add("portabilityDate", DATETIME-TZ(Func.Common:mTimeStamp2DateTime(aoProCommandOrder:ahMsRequest::ActStamp))).
         loJsonObject:Add("donorOperator", aoProCommandOrder:acDonorOperator).
      END.

      RETURN loJsonObject.

   END METHOD.

   METHOD PROTECTED ABSTRACT VOID mFetchData().

END CLASS.