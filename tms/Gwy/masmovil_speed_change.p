
USING Progress.Json.ObjectModel.*.

{Syst/tmsconst.i}
{Func/log.i}
{Func/memo.i}
{Func/cparam2.i}

DEF INPUT PARAM piOrderId       AS INT  NO-UNDO.
DEF INPUT PARAM pcDownloadSpeed AS CHAR NO-UNDO.
DEF INPUT PARAM pcUploadSpeed   AS CHAR NO-UNDO.

DEF VAR lcFixedNumber AS CHAR NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcResultCode AS CHAR NO-UNDO. 
DEF VAR lcResultDesc AS CHAR NO-UNDO. 
DEF VAR lcHost         AS CHAR       NO-UNDO.
DEF VAR liPort         AS INT        NO-UNDO.
DEF VAR lcUserId       AS CHAR       NO-UNDO.
DEF VAR lcpassword     AS CHAR       NO-UNDO.
DEF VAR lcUriPath      AS CHAR       NO-UNDO.
DEF VAR lcUriQuery     AS CHAR       NO-UNDO.
DEF VAR lcUriQueryVal  AS CHAR       NO-UNDO.
DEF VAR loRequestJson  AS JsonObject NO-UNDO.
DEF VAR loJson         AS JsonObject NO-UNDO.

FUNCTION fGetRequestJson AS JsonObject
    ():
    
    DEF VAR lobjServiceArray           AS JsonArray  NO-UNDO.
    DEF VAR lobjCharacteristicArray    AS JsonArray  NO-UNDO.
    DEF VAR lServiceObject             AS JsonObject NO-UNDO.
    DEF VAR lCharacteristicDataObject1 AS JsonObject NO-UNDO.
    DEF VAR lCharacteristicDataObject2 AS JsonObject NO-UNDO.
    DEF VAR lOutputJsonObject          AS JsonObject NO-UNDO.

    DEF VAR loJson         AS JsonObject NO-UNDO.

    lobjCharacteristicArray = new jsonarray().
 
    lCharacteristicDataObject1 = new jsonobject().
    lCharacteristicDataObject1:Add('name', 'DownloadSpeed').
    lCharacteristicDataObject1:Add('value', pcDownloadSpeed).

    lCharacteristicDataObject2 = new jsonobject().
    lCharacteristicDataObject2:Add('name', 'UploadSpeed').
    ldataarrayobj1:Add('value', pcUploadSpeed).

    lobjCharacteristicArray:add(lCharacteristicDataObject1).
    lobjCharacteristicArray:add(lCharacteristicDataObject2).

    lobjServiceArray = NEW jsonarray().

    lServiceObject = NEW jsonobject().
    lServiceObject:add('Characteristics', lobjCharacteristicArray).
    lServiceObject:add('type', 'FTTH').
    lobjServiceArray:add(lServiceObject).

    lOutputJsonObject = NEW jsonobject().
    lOutputJsonObject:ADD("orderID", "Y" + STRING(piOrderId)).
    lOutputJsonObject:ADD("Services" , lobjServiceArray).
    
    RETURN lOutputJsonObject.

END FUNCTION.

ASSIGN
    lcHost        = fCParam("Masmovil", "Host")   
    liPort        = fIParam("Masmovil", "Port")     
    lcUriPath     = fCParam("Masmovil", "UriPath")   
    lcUriQuery    = fCParam("Masmovil", "UriQuery")  
    lcUriQueryVal = fCParam("Masmovil", "UriQueryValue").

loRequestJson = fGetRequestJson().

RUN Gwy/http_rest_client("post"    ,
                         lcHost    ,
                         liPort    ,     
                         ""        ,
                         ""        ,
                         lcUriPath ,
                         lcUriQuery,
                         lcUriQueryVal,
                         loRequestJson,
                         OUTPUT loJson).

ASSIGN  
    lcResultCode = loJson:GetCharacter(INPUT "codigo"      AS CHARACTER)
    lcResultDesc = loJson:GetCharacter(INPUT "descripcion" AS CHARACTER).

IF lcResultCode EQ "FTTH0000" THEN DO:
   RETURN "".
ELSE 
   RETURN SUBST("ErrorCode: &1 ", (IF lcResultDesc > "" THEN lcResultDesc ELSE '')).

FINALLY:

END.
