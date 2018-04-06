USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.lib.Logger.

BLOCK-LEVEL ON ERROR UNDO, THROW. 

CLASS bss.cls.order_create INHERITS profcgi.RPC.JSON.Response:

   METHOD PUBLIC OVERRIDE VOID mHandle
      ( ioJsonObject AS CLASS JsonObject ):

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.
      
      loJsonObject = NEW JsonObject().
      
      loJsonObject:Add("Text", "Hello world!").
      loJsonObject:Add("Number", 1).
      loJsonObject:Add("Request", ioJsonObject).
      
      mSendResult(loJsonObject).
      
   END METHOD.
      
   METHOD PUBLIC OVERRIDE VOID mHandle
      ( ioJsonArray AS CLASS JsonArray ):

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loJsonArray  AS CLASS JsonArray  NO-UNDO.
      DEFINE VARIABLE lii          AS INTEGER NO-UNDO.
      
      loJsonArray = NEW JsonArray().
    
      DO lii = 1 TO ioJsonArray:Length:
      
         loJsonObject = NEW JsonObject().
         loJsonObject:Add("Text", SUBSTITUTE("Hello world &1!", lii)).
         loJsonObject:Add("Number", lii).
         loJsonObject:Add("Request", ioJsonArray:GetJsonText(lii)).
         loJsonArray:Add(loJsonObject).

      END.
      
      mSendResult(loJsonArray).

   END METHOD.

   METHOD PUBLIC OVERRIDE VOID mHandle():
      
      UNDO, THROW NEW ParamError("order_create needs parameters").

   END METHOD.

END CLASS.
