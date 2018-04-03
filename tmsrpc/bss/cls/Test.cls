USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.

BLOCK-LEVEL ON ERROR UNDO, THROW. 

CLASS bss.cls.Test INHERITS profcgi.RPC.JSON.Response:

   METHOD PUBLIC OVERRIDE VOID mHandle
      ( ioJsonObject AS CLASS JsonObject ):

      UNDO, THROW NEW InternalError("JsonObject mHandle method is not implemented").

   END METHOD.
      
   METHOD PUBLIC OVERRIDE VOID mHandle
      ( ioJsonArray AS CLASS JsonArray ):

      UNDO, THROW NEW InternalError("JsonArray mHandle method is not implemented").

   END METHOD.

   METHOD PUBLIC OVERRIDE VOID mHandle():
      
      UNDO, THROW NEW InternalError("mHandle method having no parameter is not implemented").

   END METHOD.

END CLASS.
