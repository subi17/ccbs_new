 
 /*------------------------------------------------------------------------
    File        : ProCommandOrder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING Gwy.SAPC.OrderEnum.
USING Gwy.SAPC.Service.IService.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.ProCommandOrder INHERITS Gwy.SAPC.ProCommand: 

   DEFINE PUBLIC VARIABLE aoOrderEnum AS CLASS OrderEnum NO-UNDO.
   DEFINE PUBLIC VARIABLE acCLI       AS CHARACTER       NO-UNDO.
   DEFINE PUBLIC VARIABLE acCLIType   AS CHARACTER       NO-UNDO.
   DEFINE PUBLIC VARIABLE acICC       AS CHARACTER       NO-UNDO.
   DEFINE PUBLIC VARIABLE alPayType   AS LOGICAL         NO-UNDO.

   CONSTRUCTOR ProCommandOrder
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).

      mSetProCommandId().

   END CONSTRUCTOR.
   
   METHOD PROTECTED JsonObject mOrderObject
      (icOrderType AS CHARACTER):

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("orderID", STRING(aiProCommandId)).
      loJsonObject:Add("orderType", icOrderType).
      loJsonObject:Add("subscriptionID", STRING(MsRequest.MsSeq)).
      loJsonObject:Add("Sellchannel", "TMSB2C").
      loJsonObject:Add("Selldate", DATETIME-TZ(Func.Common:mTimeStamp2DateTime(MsRequest.ActStamp))).
      loJsonObject:Add("Seller", CAPS(multitenancy.TenantInformation:mGetEffectiveBrand())).
      loJsonObject:Add("createdBy", MsRequest.UserCode).
      loJsonObject:Add("createdDate", DATETIME-TZ(Func.Common:mTimeStamp2DateTime(MsRequest.CreStamp))).

      RETURN loJsonObject.

   END METHOD.

END CLASS.