 
 /*------------------------------------------------------------------------
    File        : ParamRESTClient
    Purpose     : Can take following parameters from TMSParam table:
                  Scheme, Host, Port, UserName, PassWord, Domain,
                  ReqTimeOut, RetryPause, NumRetries, ClientName and
                  ClientVersion
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed May 16 16:18:54 EEST 2018
    Notes       : It should be possible to re-use the same ParamRESTClient
                  object (make multiple REST requests using the same object) 
       
    Usage:

    DO ON ERROR UNDO, THROW:
       objRESTClient = NEW ParamRESTClient("<tmsparamgroup>").
   
       objRESTClient:mSetURIPath = "uri/path".
       
       /* optional
          Normally it is not needed to specify content type and
          it is automatically defined based on the entity/body data */
       objRESTClient:mSetContentType("application/json").
       
       /* optional
          This is a content type charset and normally it is not needed
          to specify it.
          The default value for Json data is UTF-8 and for others
          this has empty by default (charset is implicitely defined) */    
       objRESTClient:mSetContentCharSet("UTF-8").
   
       /* optional
          By default for when there a json data is send by the client
          the accepted content type is also application/json.
          
          Other cases by default all content types are accepted. */    
       objRESTClient:mSetAcceptContentType("application/json").
   
       /* URI query (get needs this). The last parameter is set to YES value
          then the existing query will be deleted */
       objRESTClient:mAddURIQuery("name", "value", NO).
      
       /* URI query without a value */
       objRESTClient:mAddURIQuery("name2", NO).
   
       objRESTClient:mPOST(<JsonObject/JsonArray>).
   
       /* objRESTClient:aoResultObject will contain the result data, if any */
    
       CATCH loRESTError AS Gwy.RESTError:
          /* Error handling will be here... */
          /* NOTE: The errors automatically are logged to the client log */
       END CATCH.
       
       FINALLY:
          IF VALID-OBJECT(objRESTClient)
          THEN DELETE OBJECT objRESTClient.
       END FINALLY.
    END.

  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.ParamRESTClient INHERITS Gwy.RESTClient: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   CONSTRUCTOR PUBLIC ParamRESTClient
      ( icTMSParamGroup AS CHARACTER ):

      /* The super class constructor having no parameters
         is implicitly called at this point... */

      mProcessHTTPClientParams(icTMSParamGroup).
      mProcessHTTPRequestParams(icTMSParamGroup).

   END CONSTRUCTOR.

   METHOD PRIVATE VOID mProcessHTTPClientParams
      (icTMSParamGroup AS CHARACTER):

      DEFINE VARIABLE lcc AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lde AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE lii AS INTEGER   NO-UNDO.
      DEFINE VARIABLE loRESTError AS CLASS Gwy.RESTError NO-UNDO.

      DO ON ERROR UNDO, THROW:
         lcc = Syst.Parameters:getc("ClientName", icTMSParamGroup).
         CATCH loAppError AS Progress.Lang.AppError:
            lcc = ?.
         END CATCH.
      END.
      IF lcc NE ?
      THEN mSetNameVersion(lcc,
                           Syst.Parameters:getc("ClientVersion",icTMSParamGroup)).

      DO ON ERROR UNDO, THROW:
         lde = Syst.Parameters:getde("ReqTimeOut", icTMSParamGroup).
         CATCH loAppError AS Progress.Lang.AppError:
            lde = ?.
         END CATCH.
      END.
      IF lde >= 0 THEN mSetTimeout(lde).

      DO ON ERROR UNDO, THROW:
         lde = Syst.Parameters:getde("RetryPause", icTMSParamGroup).
         CATCH loAppError AS Progress.Lang.AppError:
            lde = ?.
         END CATCH.
      END.
      IF lde >= 0 THEN mSetRetryPause(lde).

      DO ON ERROR UNDO, THROW:
         lii = Syst.Parameters:geti("NumRetries", icTMSParamGroup).
         CATCH loAppError AS Progress.Lang.AppError:
            lii = ?.
         END CATCH.
      END.
      IF lii >= 0 THEN mSetNumRetries(lii).
      
      CATCH loAppError AS Progress.Lang.AppError :
         loRESTError = NEW Gwy.RESTError(loAppError:ReturnValue).
         DO lii = 1 TO loAppError:NumMessages:
            loRESTError:AddMessage(loAppError:GetMessage(lii), loAppError:GetMessageNum(lii)).
         END.
         UNDO, THROW loRESTError.
      END CATCH.

   END METHOD.

   METHOD PRIVATE VOID mProcessHTTPRequestParams
      (icTMSParamGroup AS CHARACTER):

      DEFINE VARIABLE lcUserName AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcPassWord AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lii        AS INTEGER   NO-UNDO.
      DEFINE VARIABLE loRESTError AS CLASS Gwy.RESTError NO-UNDO.
      
      DO ON ERROR UNDO, THROW:
         lcUserName = Syst.Parameters:getc("UserName", icTMSParamGroup).
         CATCH loAppError AS Progress.Lang.AppError :
            lcUserName = "".
         END CATCH.
      END.

      DO ON ERROR UNDO, THROW:
         lcPassWord = Syst.Parameters:getc("PassWord",icTMSParamGroup).
         CATCH loAppError AS Progress.Lang.AppError :
            lcPassWord = "".
         END CATCH.
      END.

      /* An error will be thrown if domain is not defined */
      IF lcUserName > ""
      THEN mSetCredentials(Syst.Parameters:getc("Domain",icTMSParamGroup),
                           lcUserName,
                           lcPassWord).

      /* An error will be thrown if any of the parameter
         is not available or it is incorrect type */
      mSetURI(Syst.Parameters:getc("Scheme", icTMSParamGroup),
              Syst.Parameters:getc("Host", icTMSParamGroup),
              Syst.Parameters:geti("Port", icTMSParamGroup)).

      CATCH loAppError AS Progress.Lang.AppError :
         loRESTError = NEW Gwy.RESTError(loAppError:ReturnValue).
         DO lii = 1 TO loAppError:NumMessages:
            loRESTError:AddMessage(loAppError:GetMessage(lii), loAppError:GetMessageNum(lii)).
         END.
         UNDO, THROW loRESTError.
      END CATCH.

   END METHOD.

END CLASS.