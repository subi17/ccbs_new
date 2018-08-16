 
 /*------------------------------------------------------------------------
    File        : ProCommandBPM
    Purpose     : Contains information common for BPM messages
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Json.ObjectModel.JsonObject.
USING Gwy.SAPC.OrderEnum.
USING Gwy.SAPC.ProCommandOrder.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.ProCommandBPM INHERITS ProCommandOrder ABSTRACT:
   
   CONSTRUCTOR ProCommandBPM
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).
      acProCommandTarget = STRING(Gwy.SAPC.TargetEnum:BPM).

   END CONSTRUCTOR.

   METHOD PROTECTED VOID mCreateCommandLine
      ( icCustID   AS CHARACTER,
        icCategory AS CHARACTER ):

      DEFINE VARIABLE loJsonObject    AS CLASS JsonObject NO-UNDO.

      loJsonObject = mOrderObject(IF aoOrderEnum EQ OrderEnum:SUBSCRIPTION_TERMINATE
                                  THEN "Disconnection"
                                  ELSE "InOrder").

      loJsonObject:Add("Client", mClientObject(icCustID,
                                               icCategory)).

      aoService = NEW Gwy.SAPC.Service.CreateOrder.UnderRoot(THIS-OBJECT).

      aoService:mCreateServices(loJsonObject).

      FINALLY:
         IF VALID-OBJECT(loJsonObject)
         THEN aoCommandLine = loJsonObject.
      END FINALLY.

   END METHOD.

   METHOD PROTECTED ABSTRACT JsonObject mContactObject().

   METHOD PROTECTED JsonObject mClientObject
      ( icCustID   AS CHARACTER,
        icCategory AS CHARACTER ):

      DEFINE VARIABLE loJsonObject    AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loContactObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("clientID", icCustID).
      loJsonObject:Add("Type", mGetSegment(icCategory)).

      loContactObject = mContactObject().
      IF VALID-OBJECT(loContactObject)
      THEN loJsonObject:Add("Contact", loContactObject).

      RETURN loJsonObject.

   END METHOD.

   METHOD PRIVATE CHARACTER mGetSegment
      ( icCategory AS CHARACTER ):

      FOR
         FIRST CustCat FIELDS (Brand Category Segment) NO-LOCK WHERE
            CustCat.Brand    = "1" AND
            CustCat.Category = icCategory:
         RETURN CustCat.Segment.
      END.

      RETURN "".

   END METHOD.

END CLASS.