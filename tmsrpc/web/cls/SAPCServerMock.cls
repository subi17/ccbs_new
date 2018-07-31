 
/*------------------------------------------------------------------------
   File        : SAPCServerMock
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : tlamminmaki
   Created     : Wed Jun 20 10:51:56 EEST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Net.HTTP.StatusCodeEnum.
USING profcgi.REST.Request.
USING profcgi.REST.Response.
USING profcgi.lib.Logger.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS web.cls.SAPCServerMock INHERITS profcgi.REST.DefaultWebHandler: 
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/

   METHOD PROTECTED OVERRIDE VOID HandleGet
      ( Req  AS CLASS Request,
        Resp AS CLASS Response ):

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("result", "OK").

      /* Note! Resp will delete the object stored to the Entity eventually
               AND the object is needed after this method has finished.
         => It is NOT allowed to delete the object stored to Resp:Entity
            in this method */
      Resp:Entity = loJsonObject.

      /* Please remember to store the StatusCode for the Resp!!!
         => Otherwise there will be Internal Server Error returned */
      Resp:StatusCode = 200.

   END METHOD.

   METHOD PROTECTED OVERRIDE VOID HandlePost
      ( Req  AS CLASS Request,
        Resp AS CLASS Response ):
      
      HandleGet(Req, Resp).

   END METHOD.

   METHOD PROTECTED OVERRIDE VOID HandlePatch
      ( Req  AS CLASS Request,
        Resp AS CLASS Response ):

      HandleGet(Req, Resp).

   END METHOD.

END CLASS.
