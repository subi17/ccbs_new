/*------------------------------------------------------------------------
    File        : http_rest_client.p
    Purpose     : Returns rest api response in json object
    Author(s)   : srinivas 
    Created     : Thu Jan 18 09:17:32 IST 2018
    Notes       : * This is the equivalent of curl -X GET -v http://localhost:16680/oemanager/applications/ -u tomcat:tomcat
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using OpenEdge.Net.HTTP.*.
using OpenEdge.Net.URI.
using Progress.Json.ObjectModel.JsonObject.

define input  parameter icAction      as character  no-undo.
define input  parameter icHost        as character  no-undo.
define input  parameter iiport        as integer    no-undo.
define input  parameter icAuthType    as character  no-undo.
define input  parameter icRealm       as character  no-undo.
define input  parameter icUserId      as character  no-undo.
define input  parameter icpassword    as character  no-undo.
define input  parameter icUriPath     as character  no-undo.
define input  parameter icUriQuery    as character  no-undo.
define input  parameter icUriQueryVal as character  no-undo.
define input  parameter ioRequestJson as JsonObject no-undo.
define output parameter ioJson        as JsonObject no-undo.

/* ***************************  Main Block  *************************** */
define variable oClient as IHttpClient no-undo.
define variable oUri as URI no-undo.
define variable oReq as IHttpRequest no-undo.
define variable oResp as IHttpResponse no-undo.
define variable oCreds as Credentials no-undo.
define variable lcUserId as character no-undo.

oClient = ClientBuilder:Build():Client.

if icUserId <> "" then do: 
    IF icRealm <> "" THEN
        oCreds = new Credentials(icRealm, icUserId, icpassword).
    ELSE
        oCreds = new Credentials('', icUserId, icpassword).
end.

oUri = new URI('http', icHost, iiport).
oUri:Path = icUriPath. 
if icUriQuery <> "" then 
    oUri:AddQuery(icUriQuery, icUriQueryVal).

case icAction:
    when STRING(MethodEnum:GET) then 
    do:
        IF icAuthType = STRING(AuthenticationMethodEnum:Basic) AND 
           VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Get(oUri)
                    :UsingBasicAuthentication(oCreds)
                    :Request.
        else if icAuthType = "" and
                VALID-OBJECT(oCreds) then 
            oReq = RequestBuilder:Get(oUri)
                    :UsingCredentials(oCreds)
                    :Request.    
        else      
            oReq = RequestBuilder:Get(oUri)
                        :Request.    

        oResp = oClient:Execute(oReq).

        case oResp:StatusCode:
            when 200 then 
            do:
                if type-of(oResp:Entity, JsonObject) then
                    assign ioJson = cast(oResp:Entity, JsonObject).
            end.
            otherwise
                undo, throw new Progress.Lang.AppError(oResp:StatusReason, 1).
        end case.
    end. /*  MethodEnum:GET */
    WHEN STRING(MethodEnum:PATCH) THEN 
    do:
        IF icAuthType = STRING(AuthenticationMethodEnum:Basic) AND 
           VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Patch(oUri, ioRequestJson)
                    :UsingBasicAuthentication(oCreds)
                    :ContentType('application/json')
                    :AcceptJson()
                    :Request.
        ELSE IF icAuthType = "" AND
                VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Patch(oUri, ioRequestJson)
                    :UsingCredentials(oCreds)
                    :ContentType('application/json')
                    :AcceptJson()
                    :Request.    
        ELSE 
            oReq = RequestBuilder:Patch(oUri, ioRequestJson)
                        :ContentType('application/json')
                        :AcceptJson()
                        :Request.    

        oResp = oClient:Execute(oReq).

        IF VALID-OBJECT(oResp) THEN 
        DO:
            IF TYPE-OF(oResp:Entity, JsonObject) THEN
                ASSIGN ioJson = CAST(oResp:Entity, JsonObject).
        END.
        ELSE 
            UNDO, THROW NEW Progress.Lang.AppError("API Call Status Code '" + STRING(oResp:StatusCode) + "'", 1).
    end. /* MethodEnum:PATCH */
    when STRING(MethodEnum:PUT) then 
    do:
        IF icAuthType = STRING(AuthenticationMethodEnum:Basic) AND 
           VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Put(oUri, ioRequestJson)
                    :UsingBasicAuthentication(oCreds)
                    :ContentType('application/json;charset=UTF-8')
                    :AcceptJson()
                    :Request.
        ELSE IF icAuthType = "" AND
                VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Put(oUri, ioRequestJson)
                    :UsingCredentials(oCreds)
                    :ContentType('application/json;charset=UTF-8')
                    :AcceptJson()
                    :Request.    
        else      
            oReq = RequestBuilder:Put(oUri, ioRequestJson)
                        :ContentType('application/json;charset=UTF-8')
                        :AcceptJson()
                        :Request.    

        oResp = oClient:Execute(oReq).

        IF VALID-OBJECT(oResp) THEN 
        DO:
            if type-of(oResp:Entity, JsonObject) then
                assign ioJson = cast(oResp:Entity, JsonObject).
        END.
        ELSE 
            undo, throw new Progress.Lang.AppError("API Call Status Code '" + STRING(oResp:StatusCode) + "'", 1).
    end. /* MethodEnum:PUT */
    when STRING(MethodEnum:POST) then 
    do:
        IF icAuthType = STRING(AuthenticationMethodEnum:Basic) AND 
           VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Post(oUri, ioRequestJson)
                    :UsingBasicAuthentication(oCreds)
                    :ContentType('application/json')
                    :AcceptJson()
                    :Request.
        ELSE IF icAuthType = "" AND
                VALID-OBJECT(oCreds) THEN
            oReq = RequestBuilder:Post(oUri, ioRequestJson)
                    :UsingCredentials(oCreds)
                    :ContentType('application/json')
                    :AcceptJson()
                    :Request.    
        else      
            oReq = RequestBuilder:Post(oUri, ioRequestJson)
                        :ContentType('application/json')
                        :AcceptJson()
                        :Request.    

        oResp = oClient:Execute(oReq).

        IF VALID-OBJECT(oResp) THEN 
        DO:
            if type-of(oResp:Entity, JsonObject) then
                assign ioJson = cast(oResp:Entity, JsonObject).
        END.
        ELSE 
            undo, throw new Progress.Lang.AppError("API Call Status Code '" + STRING(oResp:StatusCode) + "'", 1).
    end. /* MethodEnum:POST */
    otherwise
        undo, throw new Progress.Lang.AppError('Action not supported', 1).
end case.

catch oError as Progress.Lang.Error :
    undo, throw oError.
end catch.

FINALLY:
    IF VALID-OBJECT(oClient) THEN DELETE OBJECT oClient.
    IF VALID-OBJECT(oUri) THEN DELETE OBJECT oUri.
    IF VALID-OBJECT(oCreds) THEN DELETE OBJECT oCreds.
    IF VALID-OBJECT(oReq) THEN DELETE OBJECT oReq.
    IF VALID-OBJECT(oResp) THEN DELETE OBJECT oResp.
END FINALLY.
