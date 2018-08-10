 
 /*------------------------------------------------------------------------
    File        : SAPCServiceObject
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING Gwy.SAPC.Service.IService.
USING Gwy.ProCommandSubscription.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.ServiceObject ABSTRACT: 

   DEFINE PROTECTED VARIABLE aoCharacteristicArray    AS CLASS JsonArray    NO-UNDO.
   DEFINE PROTECTED VARIABLE aoProCommandSubscription AS CLASS ProCommandSubscription NO-UNDO.
   DEFINE PROTECTED VARIABLE aoService                AS CLASS IService     NO-UNDO.
   
   DEFINE PUBLIC PROPERTY ServiceObject  AS CLASS JsonObject   NO-UNDO
      GET.
      PROTECTED SET.
   
   DEFINE PROTECTED VARIABLE acServiceID              AS CHARACTER          NO-UNDO.

   CONSTRUCTOR ServiceObject
      ( ioPCS  AS CLASS ProCommandSubscription,
        icType AS CHARACTER ):
      aoProCommandSubscription = ioPCS.
      ServiceObject = NEW JsonObject().
      ServiceObject:Add("serviceID", SUBSTITUTE("&1_&2", ioPCS:aiProCommandId, icType)).
      mFetchData().
   END CONSTRUCTOR.

   DESTRUCTOR ServiceObject():
      IF VALID-OBJECT(ServiceObject)
      THEN DELETE OBJECT ServiceObject.

      IF VALID-OBJECT(aoCharacteristicArray)
      THEN DELETE OBJECT aoCharacteristicArray.
      
      IF VALID-OBJECT(aoService)
      THEN DELETE OBJECT aoService.

      aoProCommandSubscription = ?.
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
      loJsonObject:Add("phoneNumber", aoProCommandSubscription:ahOrder::CLI).

      RETURN loJsonObject.

   END METHOD.

   METHOD PROTECTED ABSTRACT VOID mFetchData().

END CLASS.