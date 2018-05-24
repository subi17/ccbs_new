 
 /*------------------------------------------------------------------------
    File        : RESTClient
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed May 16 16:18:54 EEST 2018
    Notes       : It should be possible to re-use the same RESTClient
                  object (make multiple REST requests using the same object) 
       
    Usage:

    DO ON ERROR UNDO, THROW:
       objRESTClient = NEW RESTClient().
   
       /* optional */
       objRESTClient:mSetCredentials("domain", "username", "password").
   
       objRESTClient:mSetURI("http",
                             "host",
                             12345). /* port */
   
       objRESTClient:mSetURIPath("uri/path").
       
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
    
       CATCH loError AS Progress.Lang.Error:
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
USING OpenEdge.Net.HTTP.Credentials.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING OpenEdge.Net.HTTP.RequestBuilder.
USING OpenEdge.Net.HTTP.StatusCodeEnum.
USING OpenEdge.Net.HTTP.DefaultRequestBuilder.
USING OpenEdge.Net.HTTP.MethodEnum.
USING OpenEdge.Net.URI.
USING OpenEdge.Net.UriSchemeEnum.
USING Progress.Json.ObjectModel.JsonConstruct.
USING Gwy.RESTHTTPClient.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.RESTClient: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY aoRESTHTTPClient    AS CLASS RESTHTTPClient NO-UNDO
      GET.
      PRIVATE SET.

   DEFINE PRIVATE VARIABLE aoURI               AS CLASS URI            NO-UNDO.
   DEFINE PRIVATE VARIABLE aoRequest           AS CLASS IHttpRequest   NO-UNDO.

   DEFINE PRIVATE VARIABLE aoCredentials       AS CLASS Credentials    NO-UNDO.
   
   DEFINE PRIVATE VARIABLE acURIScheme         AS CHARACTER            NO-UNDO. 
   DEFINE PRIVATE VARIABLE acURIHost           AS CHARACTER            NO-UNDO. 
   DEFINE PRIVATE VARIABLE aiURIPort           AS INTEGER              NO-UNDO. 
   DEFINE PRIVATE VARIABLE acURIFragment       AS CHARACTER            NO-UNDO. 
   DEFINE PRIVATE VARIABLE acURIPath           AS CHARACTER            NO-UNDO.
   DEFINE PRIVATE VARIABLE alURIHasQuery       AS LOGICAL              NO-UNDO. 

   DEFINE PRIVATE VARIABLE acContentType       AS CHARACTER            NO-UNDO.
   DEFINE PRIVATE VARIABLE acContentCharSet    AS CHARACTER            NO-UNDO. 

   DEFINE PRIVATE VARIABLE acAcceptContentType AS CHARACTER            NO-UNDO.

   DEFINE PUBLIC PROPERTY aoResultObject AS CLASS Progress.Lang.Object   NO-UNDO
      GET.
      PRIVATE SET.

   CONSTRUCTOR PUBLIC RESTClient():

      aoRESTHTTPClient = NEW RESTHTTPClient().
   
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC RESTClient ( ):

      IF VALID-OBJECT(aoResultObject)
      THEN DELETE OBJECT aoResultObject.

      IF VALID-OBJECT(aoURI)
      THEN DELETE OBJECT aoURI.

      IF VALID-OBJECT(aoCredentials)
      THEN DELETE OBJECT aoCredentials.

      IF VALID-OBJECT(aoRESTHTTPClient)
      THEN DELETE OBJECT aoRESTHTTPClient.

   END DESTRUCTOR.

   /* By default if the content type was application/json also
      then accepted content type is application/json.
      
      Every other case the all content types are accepted by default.
   */ 
   METHOD PUBLIC VOID mSetAcceptContentType
      ( icAcceptContentType AS CHARACTER ):

      acAcceptContentType = icAcceptContentType.

   END METHOD.

   METHOD PUBLIC VOID mSetCredentials
      (icDomain   AS CHARACTER,
       icUserName AS CHARACTER,
       icPassWord AS CHARACTER):

      IF VALID-OBJECT(aoCredentials)
      THEN DELETE OBJECT aoCredentials.
      
      aoCredentials = NEW Credentials(icDomain, icUserName,icPassWord).      

   END METHOD.

   METHOD PUBLIC VOID mSetURI
      ( icScheme     AS CHARACTER,
        icHost       AS CHARACTER,
        iiPort       AS INTEGER ):

      IF VALID-OBJECT(aoURI)
      THEN DELETE OBJECT aoURI.

      DEFINE VARIABLE loUriSchemeEnum AS CLASS UriSchemeEnum NO-UNDO.
      
      /* This throws an error if the scheme is not valid */
      loUriSchemeEnum = UriSchemeEnum:GetEnum(icScheme).

      ASSIGN
         aoURI         = NEW URI(STRING(loUriSchemeEnum), icHost, iiPort)
         acURIScheme   = STRING(loUriSchemeEnum)
         acURIHost     = icHost
         aiURIPort     = iiPort
         alURIHasQuery = FALSE.

   END METHOD.

   /* It is possible to override the content type using this method.
      Normally the default one should be a good one.
      Note: The charset value for the content type is set using
            method mSetCharSet */
   METHOD PUBLIC VOID mSetContentType
      ( icContentType AS CHARACTER ):

      acContentType = icContentType.
      
   END METHOD.

   /* It is possible to use specific charset value for the content type
      using this method.
      
      The default value for Json data is UTF-8 and for others the
      charset value is not used (it is empty) by default.
   */
   METHOD PUBLIC VOID mSetContentCharSet
      ( icCharSet AS CHARACTER ):

      acContentCharSet = icCharSet.
      
   END METHOD.

   METHOD PUBLIC VOID mSetURIPath
      ( icURIPath AS CHARACTER ):

      mURICheck("mSetURIPath").
      
      ASSIGN
         acURIPath   = icURIPath
         aoURI:Path = icURIPath.

   END METHOD.

   METHOD PUBLIC VOID mSetURIFragment
      ( icURIFragment AS CHARACTER ):

      mURICheck("mSetURIPath").
      
      ASSIGN
         acURIFragment   = icURIFragment
         aoURI:Fragment = icURIFragment.

   END METHOD.

   METHOD PRIVATE VOID mURICheck
      (icMethodName AS CHARACTER):

      IF NOT VALID-OBJECT(aoURI)
      THEN UNDO, THROW NEW Progress.Lang.AppError
               (SUBSTITUTE("Called &1 but the URI is not initialized " +
                "(mSetURI method does that)", icMethodName)).      
      
   END METHOD.

   /* Note that there can't be multiple items having the same name */
   METHOD PUBLIC VOID mAddURIQuery
      ( icName       AS CHARACTER,
        ilClearQuery AS LOGICAL ):

      mAddURIQuery(icName, ?, ilClearQuery).

   END METHOD.

   /* Note that there can't be multiple items having the same name */
   METHOD PUBLIC VOID mAddURIQuery
      ( icName       AS CHARACTER,
        icValue      AS CHARACTER,
        ilClearQuery AS LOGICAL ):

      mURICheck("mAddURIQuery").

      /* Delete the old URI object and initialize a new one using
         the same values as before */
      IF ilClearQuery AND alURIHasQuery
      THEN DO:
         mSetURI(acURIScheme, acURIHost, aiURIPort).
         mSetURIPath(acURIPath).
         mSetURIFragment(acURIFragment).
      END.

      aoURI:AddQuery(icName, icValue).
      alURIHasQuery = TRUE.

   END METHOD.

   METHOD PRIVATE DefaultRequestBuilder mDoRequestBuilder
      (ioMethodEnum AS CLASS MethodEnum):
   
      IF NOT VALID-OBJECT(aoURI)
      THEN UNDO, THROW NEW Progress.Lang.AppError
               ("Attempting to do a request but the URI is not defined").
      
      RETURN CAST(RequestBuilder:Build(STRING(ioMethodEnum), aoURI),
                  DefaultRequestBuilder).

   END METHOD.

   METHOD PRIVATE VOID mRESTResponse
      ( ioRequest AS CLASS IHttpRequest ):

      DEFINE VARIABLE loResponse AS CLASS IHttpResponse NO-UNDO.
      ASSIGN
         loResponse     = aoRESTHTTPClient:mExecute(ioRequest)
         aoResultObject = loResponse:Entity.

      Class.Logger:writeLog(2,
                            SUBSTITUTE("&1 - &2",
                                       loResponse:StatusCode,
                                       loResponse:StatusReason),
                            "INFO").

      IF VALID-OBJECT(aoResultObject)
      THEN DO:
         Class.Logger:writeLog(4, "Response data:", "DEBUG").
         Class.Logger:writeLog(4, aoResultObject, "DEBUG").
      END.

      CASE loResponse:StatusCode:
         WHEN INTEGER(StatusCodeEnum:OK) THEN.
         OTHERWISE UNDO, THROW NEW Progress.Lang.AppError
                        (SUBSTITUTE("'&1 - &2' is not supported",
                                    loResponse:StatusCode,
                                    loResponse:StatusReason)).
      END CASE.

      FINALLY:
         IF VALID-OBJECT(loResponse)
         THEN DELETE OBJECT loResponse.
      END FINALLY.

   END METHOD.

   METHOD PRIVATE VOID mRESTRequest
      ( ioMethodEnum   AS CLASS MethodEnum,
        ioObjectEntity AS CLASS Progress.Lang.Object ):

      Class.Logger:writeLog(2,
                            SUBSTITUTE("&1 REST call",
                                       STRING(ioMethodEnum)),
                            "INFO").

      Class.Logger:writeLog(3,
                            SUBSTITUTE("Base URI: &1",
                                       aoURI:BaseURI),
                            "INFO").

      Class.Logger:writeLog(2,
                            SUBSTITUTE("Relative URI: &1",
                                       aoURI:RelativeURI),
                            "INFO").

      IF VALID-OBJECT(ioObjectEntity)
      THEN DO:
         Class.Logger:writeLog(4, "Request data:", "DEBUG").
         Class.Logger:writeLog(4, ioObjectEntity, "DEBUG").
      END.

      DEFINE VARIABLE loRequest               AS CLASS IHttpRequest          NO-UNDO.
      DEFINE VARIABLE loDefaultRequestBuilder AS CLASS DefaultRequestBuilder NO-UNDO.
      DEFINE VARIABLE lcContentType           AS CHARACTER                   NO-UNDO.
      DEFINE VARIABLE lcAcceptContentType     AS CHARACTER                   NO-UNDO.
      DEFINE VARIABLE lii                     AS INTEGER                     NO-UNDO.

      IF VALID-OBJECT(aoResultObject)
      THEN DELETE OBJECT aoResultObject.

      loDefaultRequestBuilder = mDoRequestBuilder(ioMethodEnum).

      IF VALID-OBJECT(aoCredentials)
      THEN loDefaultRequestBuilder:UsingCredentials(aoCredentials).

      IF VALID-OBJECT(ioObjectEntity)
      THEN DO:
         IF acContentType > ""
         THEN DO:
            IF acContentCharSet > ""
            THEN lcContentType = SUBSTITUTE("&1;charset=&2",
                                            acContentType,
                                            acContentCharSet).
            ELSE lcContentType = acContentType.
            loDefaultRequestBuilder:WithData(ioObjectEntity, lcContentType).
         END.
         ELSE IF TYPE-OF(ioObjectEntity, JsonConstruct)
         THEN DO:
            lcContentType = "application/json".
            IF acContentCharSet > ""
            THEN lcContentType = lcContentType + ";charset=UTF-8".
            
            loDefaultRequestBuilder:WithData(ioObjectEntity, lcContentType).
            
            IF acAcceptContentType EQ ""
            THEN loDefaultRequestBuilder:AcceptJson().
         END.
         ELSE loDefaultRequestBuilder:WithData(ioObjectEntity).
      END.

      IF acAcceptContentType > ""
      THEN loDefaultRequestBuilder:AcceptContentType(acAcceptContentType).

      loRequest = loDefaultRequestBuilder:Request.

      mRESTResponse(loRequest).

      CATCH loAppError AS Progress.Lang.AppError:

         IF loAppError:ReturnValue > ""
         THEN Class.Logger:writeLog(1, loAppError:ReturnValue, "ERROR").

         UNDO, THROW loAppError.
         
      END CATCH.

      CATCH loProError AS Progress.Lang.ProError:

         DO lii = 1 TO loProError:NumMessages:
            Class.Logger:writeLog(1, loProError:GetMessage(lii), "ERROR").
         END.
      
         UNDO, THROW loProError.
      
      END CATCH.              

      FINALLY:
         IF VALID-OBJECT(loRequest)
         THEN DELETE OBJECT loRequest.
         IF VALID-OBJECT(loDefaultRequestBuilder)
         THEN DELETE OBJECT loDefaultRequestBuilder.
      END FINALLY.

   END METHOD.

   /* Note: HEAD doesn't support message body/entity */
   METHOD PUBLIC VOID mHEAD():
      mRESTRequest(MethodEnum:HEAD, ?).
   END METHOD.

   /* Note: TRACE doesn't support message body/entity */
   METHOD PUBLIC VOID mTRACE():
      mRESTRequest(MethodEnum:TRACE, ?).
   END METHOD.

   /* Note: GET doesn't support message body/entity (in here at least) */
   METHOD PUBLIC VOID mGET():

      mRESTRequest(MethodEnum:GET, ?).

   END METHOD.

   /* Note: DELETE doesn't support message body/entity (in here at least) */
   METHOD PUBLIC VOID mDELETE():

      mRESTRequest(MethodEnum:DELETE, ?).

   END METHOD.

   METHOD PUBLIC VOID mPOST():

      mRESTRequest(MethodEnum:POST, ?).

   END METHOD.

   METHOD PUBLIC VOID mPOST
      (ioObjectEntity AS CLASS Progress.Lang.Object):

      mRESTRequest(MethodEnum:POST, ioObjectEntity).

   END METHOD.

   METHOD PUBLIC VOID mPUT():

      mRESTRequest(MethodEnum:PUT, ?).

   END METHOD.

   METHOD PUBLIC VOID mPUT
      (ioObjectEntity AS CLASS Progress.Lang.Object):

      mRESTRequest(MethodEnum:PUT, ioObjectEntity).

   END METHOD.
   
   METHOD PUBLIC VOID mPATCH():

      mRESTRequest(MethodEnum:PATCH, ?).

   END METHOD.

   METHOD PUBLIC VOID mPATCH
      (ioObjectEntity AS CLASS Progress.Lang.Object):

      mRESTRequest(MethodEnum:PATCH, ioObjectEntity).

   END METHOD.

   METHOD PUBLIC VOID mOPTIONS():

      mRESTRequest(MethodEnum:OPTIONS, ?).

   END METHOD.

   METHOD PUBLIC VOID mOPTIONS
      (ioObjectEntity AS CLASS Progress.Lang.Object):

      mRESTRequest(MethodEnum:OPTIONS, ioObjectEntity).

   END METHOD.

END CLASS.