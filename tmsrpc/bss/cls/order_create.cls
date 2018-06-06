/*
   {
     "contractid": "123456",
     "channel": "customer-care",
     "salesman": "user123",
     "created-at": "2018-04-01",
     "agreement-customer": {
       "custnum": 1234567
     },
     "items": [
       {
         "type": "acc",
         "product-offering-id": "ACC0",
         "product-id": "ACC",
         "attributes": {
           "memo": {
             "title": "Agreement customer change",
             "content": "Customer request"
           },
           "subscription-id": 12345,
           "execution-date": "2018-04-01",
           "custnum": 74387482
         }
       }
     ]
   }
*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW. 

CLASS bss.cls.order_create INHERITS profcgi.RPC.JSON.Response:

   METHOD PUBLIC OVERRIDE VOID mHandle
      ( ioJsonObject AS CLASS JsonObject ):

      multitenancy.TenantInformation:mSetEffectiveBrand(ioJsonObject:GetCharacter("brand")).

      DEFINE VARIABLE loOrder AS CLASS bss.cls.Order NO-UNDO.

      loOrder = NEW bss.cls.Order().
      loOrder:mCreateData(ioJsonObject).

      DO TRANSACTION ON ERROR UNDO, THROW:
         loOrder:mStoreData().
      END.

      mSendResult(loOrder:mResult()).

      FINALLY:
         IF VALID-OBJECT(loOrder)
         THEN DELETE OBJECT loOrder.
      END FINALLY.
      
   END METHOD.

END CLASS.
