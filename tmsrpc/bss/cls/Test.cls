USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.lib.Logger.

BLOCK-LEVEL ON ERROR UNDO, THROW. 

CLASS bss.cls.Test INHERITS profcgi.RPC.JSON.Response:

   METHOD PUBLIC OVERRIDE VOID mHandle
      ( ioJsonObject AS CLASS JsonObject ):

      Logger:writeLog(1, "Called mHandle method using JsonObject parameter", "DEBUG").

      UNDO, THROW NEW InternalError("JsonObject mHandle method is not implemented").

   END METHOD.
      
   METHOD PUBLIC OVERRIDE VOID mHandle
      ( ioJsonArray AS CLASS JsonArray ):

      Logger:writeLog(1, "Called mHandle method using JsonArray parameter", "DEBUG").

      UNDO, THROW NEW InternalError("JsonArray mHandle method is not implemented").

   END METHOD.

   METHOD PUBLIC OVERRIDE VOID mHandle():
      
      Logger:writeLog(1, "Called mHandle method using no parameter", "DEBUG").

      UNDO, THROW NEW InternalError("mHandle method having no parameter is not implemented").

   END METHOD.

END CLASS.
