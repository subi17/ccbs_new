 
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
       objRESTClient:mSetTimeout(5.0).

       /* optional */
       objRESTClient:mSetRetryPause(2.0).

       /* optional */
       objRESTClient:mSetNumRetries(3).

       /* optional */
       objRESTClient:mSetNameVersion("Fancy-client", "0.33").

       /* optional */
       objRESTClient:mSetLibrary(<IHttpClientLibrary>).

       /* optional */
       objRESTClient:mSetProxy(<URI>).

       /* optional */
       objRESTClient:mSetProxy("http://some.proxy").

       /* optional */
       objRESTClient:mSetCookie(<ICookieJar>).

       /* optional */
       objRESTClient:mSetCookie().

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
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.URI.
USING OpenEdge.Net.UriSchemeEnum.
USING Progress.Json.ObjectModel.JsonConstruct.
USING Gwy.RESTError.

&GLOBAL-DEFINE PROGRESS_VERSION 11.6

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.RESTClient: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

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

   /* HTTP client */
   DEFINE PRIVATE VARIABLE aoHTTPClient    AS CLASS IHttpClient        NO-UNDO.

   /* HTTP client related variables */
   DEFINE PRIVATE VARIABLE alClientChanged AS LOGICAL   INITIAL TRUE   NO-UNDO.
   DEFINE PRIVATE VARIABLE adeTimeOut      AS DECIMAL   INITIAL ?      NO-UNDO.
   DEFINE PRIVATE VARIABLE adeRetryPause   AS DECIMAL   INITIAL ?      NO-UNDO.
   DEFINE PRIVATE VARIABLE aiNumRetries    AS INTEGER   INITIAL ?      NO-UNDO.
   DEFINE PRIVATE VARIABLE acClientName    AS CHARACTER INITIAL ?      NO-UNDO.
   DEFINE PRIVATE VARIABLE acClientVersion AS CHARACTER INITIAL ?      NO-UNDO.
   DEFINE PRIVATE VARIABLE aoLibrary       AS CLASS IHttpClientLibrary NO-UNDO.
   DEFINE PRIVATE VARIABLE aoCookie        AS CLASS ICookieJar         NO-UNDO.
   DEFINE PRIVATE VARIABLE alEnableCookie  AS LOGICAL   INITIAL FALSE  NO-UNDO.
   DEFINE PRIVATE VARIABLE aoProxy         AS CLASS URI                NO-UNDO.
   /* Seems that this is not available in Progress 11.6 */
   /*DEFINE PRIVATE VARIABLE aoLogger      AS CLASS ILogWriter         NO-UNDO.*/


   DEFINE PUBLIC PROPERTY aoResultObject AS CLASS Progress.Lang.Object   NO-UNDO
      GET.
      PRIVATE SET.

   CONSTRUCTOR PUBLIC RESTClient():
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC RESTClient ( ):

      IF VALID-OBJECT(aoResultObject)
      THEN DELETE OBJECT aoResultObject.

      IF VALID-OBJECT(aoURI)
      THEN DELETE OBJECT aoURI.

      IF VALID-OBJECT(aoCredentials)
      THEN DELETE OBJECT aoCredentials.

      IF VALID-OBJECT(aoHTTPClient)
      THEN DELETE OBJECT aoHTTPClient.

   END DESTRUCTOR.


   METHOD PUBLIC VOID mSetTimeout
      ( ideValue AS DECIMAL ): /* A zero or positive value */

      IF adeTimeOut EQ ideValue
      THEN RETURN.

      ASSIGN
         adeTimeOut      = ideValue
         alClientChanged = TRUE.

   END METHOD.

   METHOD PUBLIC VOID mSetRetryPause
      ( ideValue AS DECIMAL ): /* A zero or positive value */

      IF adeRetryPause EQ ideValue
      THEN RETURN.

      ASSIGN
         adeRetryPause   = ideValue
         alClientChanged = TRUE.

   END METHOD.

   METHOD PUBLIC VOID mSetNumRetries
      ( iiValue AS INTEGER ): /* A zero or positive value */

      IF aiNumRetries EQ iiValue
      THEN RETURN.

      ASSIGN
         aiNumRetries    = iiValue
         alClientChanged = TRUE.

   END METHOD.

   /* Sets the rest client name and version
      Default name: 'OpenEdge-HttpClient'
      Default version: '0.4.0'
   */
   METHOD PUBLIC VOID mSetNameVersion
      ( icName AS CHARACTER,
        icVersion AS CHARACTER ):

      IF acClientName EQ icName AND
         acClientVersion EQ icVersion
      THEN RETURN.

      ASSIGN
         acClientName    = icName
         acClientVersion = icVersion
         alClientChanged = TRUE.

   END METHOD.

   /* It is probably not a good idea to call this.
      The default library should work ok (will be created using a method
      BuildLibrary in a class OpenEdge.Net.HTTP.ClientBuilder) */
   METHOD PUBLIC VOID mSetLibrary
      ( ioLibrary AS CLASS IHttpClientLibrary ):

      IF aoLibrary EQ ioLibrary
      THEN RETURN.

      ASSIGN
         aoLibrary       = ioLibrary
         alClientChanged = TRUE.

   END METHOD.

/*
   METHOD PUBLIC VOID mSetLogger
      ( ioLogger AS CLASS ILogWriter ):

      IF aoLogger EQ ioLogger
      THEN RETURN.

      ASSIGN
         aoLogger         = ioLogger
         alClientChanged  = TRUE.

   END METHOD.
*/

   METHOD PUBLIC VOID mSetProxy
      ( ioProxyURI AS CLASS URI ):

      IF aoProxy EQ ioProxyURI
      THEN RETURN.

      ASSIGN
         aoProxy         = ioProxyURI
         alClientChanged = TRUE.

   END METHOD.

   METHOD PUBLIC VOID mSetProxy
      ( icProxyURI AS CHARACTER ):
      mSetProxy(URI:Parse(icProxyURI)).
   END METHOD.

   METHOD PUBLIC VOID mSetCookie
      ( ioCookieJar AS CLASS ICookieJar ):

      IF aoCookie EQ ioCookieJar
      THEN RETURN.

      ASSIGN
         aoCookie        = ioCookieJar
         alEnableCookie = TRUE
         alClientChanged = TRUE.

   END METHOD.

   METHOD PUBLIC VOID mSetCookie():

      ASSIGN
         aoCookie        = ?
         alEnableCookie = TRUE
         alClientChanged = TRUE.

   END METHOD.

   METHOD PRIVATE VOID mInitHTTPClient():

      /* Delete the old one if it exists */
      IF VALID-OBJECT(aoHTTPClient)
      THEN DELETE OBJECT aoHTTPClient.

      DEFINE VARIABLE loClientBuilder AS CLASS DefaultHttpClientBuilder NO-UNDO.

      loClientBuilder = CAST(ClientBuilder:Build(), DefaultHttpClientBuilder).

      IF adeTimeOut NE ?
      THEN loClientBuilder:SetRequestTimeout(adeTimeOut).

      IF adeRetryPause NE ?
      THEN loClientBuilder:SetRetryPause(adeRetryPause).

      IF aiNumRetries NE ?
      THEN loClientBuilder:SetNumRetries(aiNumRetries).

      IF acClientName NE ? OR acClientVersion NE ?
      THEN loClientBuilder:Named(IF acClientName EQ ?
                                 THEN 'OpenEdge-HttpClient' ELSE acClientName,
                                 IF acClientVersion EQ ?
                                 THEN '0.4.0' ELSE acClientVersion).

      IF VALID-OBJECT(aoLibrary)
      THEN loClientBuilder:UsingLibrary(aoLibrary).

      IF VALID-OBJECT(aoCookie)
      THEN loClientBuilder:KeepCookies(aoCookie).
      &IF {&PROGRESS_VERSION} > 11.5 &THEN
      ELSE IF alEnableCookie
      THEN loClientBuilder:KeepCookies().
      &ENDIF

      IF VALID-OBJECT(aoProxy)
      THEN loClientBuilder:ViaProxy(aoProxy).

      /*
      IF VALID-OBJECT(aoLogger)
      THEN loClientBuilder:UsingLogger(aoLogger).
      */

      aoHTTPClient = loClientBuilder:Client.

      FINALLY:
         IF VALID-OBJECT(loClientBuilder)
         THEN DELETE OBJECT loClientBuilder.
         alClientChanged = FALSE.
      END FINALLY.

   END METHOD.

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

      &IF {&PROGRESS_VERSION} > 11.5 &THEN
      DEFINE VARIABLE loUriSchemeEnum AS CLASS UriSchemeEnum NO-UNDO.
      
      /* This throws an error if the scheme is not valid */
      loUriSchemeEnum = UriSchemeEnum:GetEnum(icScheme).

      ASSIGN
         aoURI         = NEW URI(STRING(loUriSchemeEnum), icHost, iiPort)
         acURIScheme   = STRING(loUriSchemeEnum)
         acURIHost     = icHost
         aiURIPort     = iiPort
         alURIHasQuery = FALSE.

      &ELSE
      ASSIGN
         aoURI         = NEW URI(icScheme, icHost, iiPort)
         acURIScheme   = icScheme
         acURIHost     = icHost
         aiURIPort     = iiPort
         alURIHasQuery = FALSE.
      &ENDIF

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

      mURICheck("mSetURIFragment").
      
      ASSIGN
         acURIFragment   = icURIFragment
         aoURI:Fragment = icURIFragment.

   END METHOD.

   METHOD PRIVATE VOID mURICheck
      (icMethodName AS CHARACTER):

      IF NOT VALID-OBJECT(aoURI)
      THEN UNDO, THROW NEW RESTError(SUBSTITUTE("mSetURI must be called " +
                                                "before calling method &1",
                                                icMethodName)).

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
      ( ioMethodEnum AS CLASS MethodEnum ):
   
      IF NOT VALID-OBJECT(aoURI)
      THEN UNDO, THROW NEW RESTError
               (SUBSTITUTE("mSetURI must be called before HTTP &1",
                           STRING(ioMethodEnum))).
      
      RETURN CAST(RequestBuilder:Build(STRING(ioMethodEnum), aoURI),
                  DefaultRequestBuilder).

   END METHOD.

   METHOD PRIVATE VOID mRESTResponse
      ( ioRequest AS CLASS IHttpRequest ):

      DEFINE VARIABLE lcStatusReason AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcResult       AS LONGCHAR  NO-UNDO.

      DEFINE VARIABLE loResponse AS CLASS IHttpResponse NO-UNDO.
      ASSIGN
         loResponse     = aoHTTPClient:Execute(ioRequest)
         aoResultObject = loResponse:Entity
         lcStatusReason = loResponse:StatusReason.

      IF NOT lcStatusReason > ""
      THEN lcStatusReason = StatusCodeHelper:GetMessage(loResponse:StatusCode).

      IF loResponse:StatusCode >= 400
      THEN UNDO, THROW NEW RESTError(loResponse:StatusCode, lcStatusReason, aoResultObject).

      Class.Logger:writeLog(2,
                            SUBSTITUTE("&1 &2",
                                       loResponse:StatusCode,
                                       lcStatusReason),
                            "INFO").

      CASE loResponse:StatusCode:
         WHEN INTEGER(StatusCodeEnum:OK)
         THEN.
         OTHERWISE
            Class.Logger:writeLog(1,
                                  SUBSTITUTE("There is no special logic for " +
                                             "the HTTP status code &1",
                                             loResponse:StatusCode),
                                  "WARNING").
      END.

      IF VALID-OBJECT(aoResultObject)
      THEN DO:
         Class.Logger:writeLog(4, "Response data:", "DEBUG").
         Class.Logger:writeLog(4, aoResultObject, "DEBUG").
      END.

      CATCH loHTTPError AS HttpRequestError:
         UNDO, THROW NEW RESTError(loHTTPError).
      END CATCH.

      FINALLY:
         IF VALID-OBJECT(loResponse)
         THEN DELETE OBJECT loResponse.
      END FINALLY.

   END METHOD.

   METHOD PRIVATE VOID mRESTRequest
      ( ioMethodEnum   AS CLASS MethodEnum,
        ioObjectEntity AS CLASS Progress.Lang.Object ):

      IF alClientChanged OR NOT VALID-OBJECT(aoHTTPClient)
      THEN mInitHTTPClient().

      Class.Logger:writeLog(2,
                            SUBSTITUTE("&1 REST call at URI &2",
                                       STRING(ioMethodEnum), aoURI:ToString()),
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
      DEFINE VARIABLE lcReturnValue           AS CHARACTER                   NO-UNDO.
      DEFINE VARIABLE loRESTError             AS CLASS Gwy.RESTError         NO-UNDO.

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

      CATCH loError AS Progress.Lang.Error:

         IF TYPE-OF(loError, RESTError)
         THEN UNDO, THROW CAST(loError, RESTError).

         IF TYPE-OF(loError, Progress.Lang.AppError)
         THEN lcReturnValue = CAST(loError, Progress.Lang.AppError):ReturnValue.

         loRESTError = NEW RESTError(lcReturnValue).

         DO lii = 1 TO loError:NumMessages:
            loRESTError:AddMessage(loError:GetMessage(lii), loError:GetMessageNum(lii)).
         END.

         UNDO, THROW loRESTError.

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