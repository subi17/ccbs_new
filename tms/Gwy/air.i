{commali.i}
{cparam2.i}
{xmlfunction.i}
{airnodes.i}

DEFINE TEMP-TABLE ttUCIP NO-UNDO
   FIELD ttName   AS CHARACTER
   FIELD ttFormat AS CHARACTER
   FIELD ttValue  AS CHARACTER.

DEFINE VARIABLE lcTCPModule  AS CHARACTER NO-UNDO INITIAL "tcpgwy" . 

FIND FIRST TMSParam where
           TMSParam.Brand      = gcBrand AND 
           TMSParam.ParamCode  =  "TCPModule" NO-LOCK NO-ERROR.
IF AVAIL TMSParam THEN 
         lcTCPModule = TMSParam.CharVal.

PROCEDURE pPrePaidPlatform:

   DEFINE INPUT PARAMETER pcMethod AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lhSAXWriter  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lmXML        AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE lcRoot       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcStruct     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcURL        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lhField      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhTable      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lcTable      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcFormat     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcXML        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcHTTPHeader AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcReturn     AS CHARACTER NO-UNDO.
   ASSIGN
      lcRoot   = "methodCall"
      lcStruct = "params,param,value,struct"
      /*lcURL    = "-H 217.168.3.233 -S 10010"*/.
   
   lcUrl = fGetAIRNode().

   CREATE SAX-WRITER lhSAXWriter. 
   
   lhSAXWriter:FORMATTED = FALSE. 
   
   lhSAXWriter:SET-OUTPUT-DESTINATION("memptr",lmXML).
   
   lhSAXWriter:START-DOCUMENT().
   lhSAXWriter:START-ELEMENT(lcRoot).

   lhSAXWriter:WRITE-DATA-ELEMENT("methodName",pcMethod).

   fRPCStruct("Start",lcStruct,lhSAXWriter).

   FOR EACH ttUCIP NO-LOCK: 

      ASSIGN
         lhTable = BUFFER ttUCIP:HANDLE
         lcTable = lhTable:NAME.

      lhSAXWriter:START-ELEMENT("member").

      lhField = lhTable:BUFFER-FIELD(1).
      lhSAXWriter:WRITE-DATA-ELEMENT("name",STRING(lhField:BUFFER-VALUE)).
    
      lhSAXWriter:START-ELEMENT("value").
    
      lhField = lhTable:BUFFER-FIELD(2).
      lcFormat = lhField:BUFFER-VALUE.

      lhField = lhTable:BUFFER-FIELD(3).
      lhSAXWriter:WRITE-DATA-ELEMENT(lcFormat,STRING(lhField:BUFFER-VALUE)).
    
      lhSAXWriter:END-ELEMENT("value"). 
    
      lhSAXWriter:END-ELEMENT("member").

   END. 

   fRPCStruct("End",lcStruct,lhSAXWriter).

   lhSAXWriter:END-ELEMENT(lcRoot).

   lhSAXWriter:END-DOCUMENT().

   DELETE OBJECT lhSAXWriter.

   EMPTY TEMP-TABLE ttUCIP.

   lcXML = GET-STRING(lmXML,1).

   /* Basic yoigo:yoigo12 */
   lcHTTPHeader = "POST /Air HTTP/1.1~n"             +
                  "Content-Type: text/xml~n"         +
                  "Host: propus:10010~n"             +
                  "User-Agent: UGw Server/4.2/1.0~n" +
                  "Authorization: Basic eW9pZ286eW9pZ28xMgo=~n" + 
                  "Content-Length: " + STRING(LENGTH(lcXML)) + "~n~n".

   SET-SIZE(lmXML) = 0.

   RUN VALUE(lcTCPModule) (lcHTTPHeader + lcXML,lcURL,3,10,"</methodResponse>").
   /* RUN tcpgwy(lcHTTPHeader + lcXML,lcURL,3,2,"<"). */
   
   lcReturn = RETURN-VALUE.

   RETURN lcReturn.

END PROCEDURE.

