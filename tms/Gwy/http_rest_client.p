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

if icUserId <> "" then 
    oCreds = new Credentials('', icUserId, icpassword).

oUri = new URI('http', icHost, iiport).
oUri:Path = icUriPath. 
if icUriQuery <> "" then 
    oUri:AddQuery(icUriQuery, icUriQueryVal).

case icAction:
    when 'get' then 
    do:
        if lcUserId <> "" then 
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
    end.
    when 'post' then 
    do:
        if lcUserId <> "" then 
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

        case oResp:StatusCode:
            when 200 then 
            do:
                if type-of(oResp:Entity, JsonObject) then
                    assign ioJson = cast(oResp:Entity, JsonObject).
            end.
            otherwise
                undo, throw new Progress.Lang.AppError(oResp:StatusReason, 1).
        end case.
    end.
    otherwise
        undo, throw new Progress.Lang.AppError('Action not supported', 1).
end case.

catch oError as Progress.Lang.Error :
    undo, throw oError.
end catch.
