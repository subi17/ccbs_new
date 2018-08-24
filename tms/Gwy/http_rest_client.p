/*------------------------------------------------------------------------
    File        : http_rest_client.p
    Purpose     : Returns rest api response in json object
    Author(s)   : srinivas 
    Created     : Thu Jan 18 09:17:32 IST 2018
    Notes       : * This is the equivalent of curl -X GET -v http://localhost:16680/oemanager/applications/ -u tomcat:tomcat
  ----------------------------------------------------------------------*/
block-level ON ERROR UNDO, THROW.

USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.URI.
USING Progress.Json.ObjectModel.JsonObject.
USING OpenEdge.Net.HTTP.IHttpClientLibrary.
USING OpenEdge.Net.HTTP.Lib.ClientLibraryBuilder.


DEFINE INPUT PARAMETER icAction        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER icHost          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER iiport          AS INTEGER    NO-UNDO.
DEFINE INPUT PARAMETER icAuthType      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER icRealm         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER icUserId        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER icpassword      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER icUriPath       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER icUriQuery      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER icUriQueryVal   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER icHeaderName    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER icHeaderValue   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER ioRequestJson   AS JsonObject NO-UNDO.
DEFINE OUTPUT PARAMETER oiStatusCode   AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER ocStatusReason AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER ioJson         AS JsonObject NO-UNDO.

/* ***************************  Main Block  *************************** */
DEFINE VARIABLE oLib     AS IHttpClientLibrary NO-UNDO.
DEFINE VARIABLE oClient  AS IHttpClient   NO-UNDO.
DEFINE VARIABLE oUri     AS URI           NO-UNDO.
DEFINE VARIABLE oReq     AS IHttpRequest  NO-UNDO.
DEFINE VARIABLE oResp    AS IHttpResponse NO-UNDO.
DEFINE VARIABLE oCreds   AS Credentials   NO-UNDO.
DEFINE VARIABLE lcUserId AS CHARACTER     NO-UNDO.


oClient = ClientBuilder:Build():Client.

IF icUserId NE "" THEN DO: 
    IF icRealm NE "" THEN
        oCreds = new Credentials(icRealm, icUserId, icpassword).
    ELSE
        oCreds = new Credentials('', icUserId, icpassword).
END.

oUri = NEW URI('http', icHost, iiport).
oUri:Path = icUriPath. 

IF icUriQuery NE "" then 
    oUri:AddQuery(icUriQuery, icUriQueryVal).

CASE icAction:
    WHEN STRING(MethodEnum:GET) THEN
    DO:
        IF icAuthType = STRING(AuthenticationMethodEnum:Basic) AND 
           VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Get(oUri)
                    :UsingBasicAuthentication(oCreds)
                    :Request.
        ELSE IF icAuthType EQ "" AND
                VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Get(oUri)
                    :UsingCredentials(oCreds)
                    :Request.    
        ELSE      
            oReq = RequestBuilder:Get(oUri)
                        :Request.    

        oResp = oClient:EXECUTE(oReq).

        CASE oResp:StatusCode:
            WHEN 200 THEN 
            DO:
                IF TYPE-OF(oResp:Entity, JsonObject) THEN
                    ASSIGN ioJson = cast(oResp:Entity, JsonObject).
            END.
            OTHERWISE
                UNDO, THROW NEW Progress.Lang.AppError(oResp:StatusReason, 1).
        END CASE.
    END. /*  MethodEnum:GET */
    WHEN STRING(MethodEnum:PATCH) THEN 
    DO:
        IF icAuthType = STRING(AuthenticationMethodEnum:Basic) AND 
           VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Patch(oUri, ioRequestJson)
                    :UsingBasicAuthentication(oCreds)
                    :ContentType('application/json')
                    :AcceptJson()
                    :Request.
        ELSE IF icAuthType EQ "" AND
                VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Patch(oUri, ioRequestJson)
                    :UsingCredentials(oCreds)
                    :ContentType('application/json')
                    :AcceptJson()
                    :Request. 
        ELSE IF icAuthType EQ "" AND 
                icHeaderValue NE "" THEN DO:
            oLib = ClientLibraryBuilder:Build()
                     :sslVerifyHost(NO)
                     :Library.  
            oClient = ClientBuilder:Build()
                       :UsingLibrary(oLib)
                       :Client.
            oUri = NEW URI('https', ichost, iiport).
            oUri:Path = icUripath.
            
            IF icUriQuery NE "" THEN
               oUri:AddQuery(icUriQuery, icUriQueryVal).
               
            oReq = RequestBuilder:Patch(oUri, ioRequestJson)
                        :ContentType('application/json')
                        :AddHeader(icHeaderName,icHeaderValue)
                        :AcceptJson()
                        :Request.
                        
        END.          
        ELSE 
            oReq = RequestBuilder:Patch(oUri, ioRequestJson)
                        :ContentType('application/json')
                        :AcceptJson()
                        :Request.    

        oResp = oClient:Execute(oReq).

        IF VALID-OBJECT(oResp) THEN 
        DO:
            IF TYPE-OF(oResp:Entity, JsonObject) THEN
                ASSIGN ioJson         = CAST(oResp:Entity, JsonObject)
                       oiStatusCode   = oResp:StatusCode
                       ocStatusReason = oResp:StatusReason.
            ELSE 
                ASSIGN oiStatusCode   = oResp:StatusCode
                       ocStatusReason = oResp:StatusReason.
        END.
        ELSE 
            UNDO, THROW NEW Progress.Lang.AppError("API Call Status Code '" + STRING(oResp:StatusCode) + "'", 1).
    END. /* MethodEnum:PATCH */
    WHEN STRING(MethodEnum:PUT) THEN
    DO:
        IF icAuthType EQ STRING(AuthenticationMethodEnum:Basic) AND 
           VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:PUT(oUri, ioRequestJson)
                    :UsingBasicAuthentication(oCreds)
                    :ContentType('application/json;charset=UTF-8')
                    :AcceptJson()
                    :Request.
        ELSE IF icAuthType EQ "" AND
                VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:PUT(oUri, ioRequestJson)
                    :UsingCredentials(oCreds)
                    :ContentType('application/json;charset=UTF-8')
                    :AcceptJson()
                    :Request.    
        ELSE      
            oReq = RequestBuilder:PUT(oUri, ioRequestJson)
                        :ContentType('application/json;charset=UTF-8')
                        :AcceptJson()
                        :Request.    

        oResp = oClient:EXECUTE(oReq).

        IF VALID-OBJECT(oResp) THEN 
        DO:
            IF TYPE-OF(oResp:Entity, JsonObject) THEN
                ASSIGN ioJson = CAST(oResp:Entity, JsonObject).
        END.
        ELSE 
            UNDO, THROW NEW Progress.Lang.AppError("API Call Status Code '" + STRING(oResp:StatusCode) + "'", 1).
    END. /* MethodEnum:PUT */
    WHEN STRING(MethodEnum:POST) THEN 
    DO:
        IF icAuthType EQ STRING(AuthenticationMethodEnum:Basic) AND 
           VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Post(oUri, ioRequestJson)
                    :UsingBasicAuthentication(oCreds)
                    :ContentType('application/json')
                    :AcceptJson()
                    :Request.
        ELSE IF icAuthType EQ "" AND
                VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Post(oUri, ioRequestJson)
                    :UsingCredentials(oCreds)
                    :ContentType('application/json')
                    :AcceptJson()
                    :Request.    
        ELSE      
            oReq = RequestBuilder:Post(oUri, ioRequestJson)
                        :ContentType('application/json')
                        :AcceptJson()
                        :Request.    

        oResp = oClient:EXECUTE(oReq).

        IF VALID-OBJECT(oResp) THEN 
        DO:
            IF TYPE-OF(oResp:Entity, JsonObject) THEN
                ASSIGN ioJson = CAST(oResp:Entity, JsonObject).
        END.
        ELSE 
            UNDO, THROW NEW Progress.Lang.AppError("API Call Status Code '" + STRING(oResp:StatusCode) + "'", 1).
    END. /* MethodEnum:POST */
    OTHERWISE
        UNDO, THROW NEW Progress.Lang.AppError('Action not supported', 1).
END CASE.

CATCH oError AS Progress.Lang.Error :
    UNDO, THROW oError.
END CATCH.

FINALLY:
    IF VALID-OBJECT(oClient) THEN DELETE OBJECT oClient.
    IF VALID-OBJECT(oUri)    THEN DELETE OBJECT oUri.
    IF VALID-OBJECT(oCreds)  THEN DELETE OBJECT oCreds.
    IF VALID-OBJECT(oReq)    THEN DELETE OBJECT oReq.
    IF VALID-OBJECT(oResp)   THEN DELETE OBJECT oResp.
END FINALLY.
