 
/*------------------------------------------------------------------------
   File        : DefaultWebHandler
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

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS web.cls.ProMessageNotify INHERITS profcgi.REST.DefaultWebHandler: 
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/

   METHOD PROTECTED OVERRIDE VOID HandleGet
      ( Req  AS CLASS Request,
        Resp AS CLASS Response ):
      
      Logger:writeLog(1, SUBSTITUTE("Server software: &1", Req:ServerSoftware), "DEBUG").
      Logger:writeLog(1, SUBSTITUTE("In the URI a promessage id was '&1'",
                                    Req:GetPathParameter("messageid")), "DEBUG").
      Resp:StatusCode = 200.

   END METHOD.

   METHOD PROTECTED OVERRIDE VOID HandlePost
      ( Req  AS CLASS Request,
        Resp AS CLASS Response ):
      HandleNotAllowedMethod(Req, Resp).    
   END METHOD.

   METHOD PROTECTED OVERRIDE VOID HandlePatch
      ( Req  AS CLASS Request,
        Resp AS CLASS Response ):
      HandleNotAllowedMethod(Req, Resp).    
   END METHOD.

END CLASS.