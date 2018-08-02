USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW. 

CLASS bss.cls.order_create INHERITS profcgi.RPC.JSON.Response:

   METHOD PUBLIC OVERRIDE VOID mHandle
      ( ioJsonObject AS CLASS JsonObject ):

      DEFINE VARIABLE loOrder AS CLASS om.Order NO-UNDO.

      multitenancy.TenantInformation:mSetEffectiveBrand(ioJsonObject:GetCharacter("brand")).

      loOrder = NEW om.Order().

      loOrder:mCreateData(ioJsonObject).

      DO TRANSACTION 
      ON ERROR UNDO, THROW:
         loOrder:mStoreData().
      END.

      mSendResult(loOrder:mResult()).

      FINALLY:
         IF VALID-OBJECT(loOrder) THEN 
            DELETE OBJECT loOrder.
      END FINALLY.
      
   END METHOD.

END CLASS.
