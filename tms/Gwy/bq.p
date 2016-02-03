{Func/xmlfunction.i}
{Func/mathfunction.i}
{Func/timestamp.i}

DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttUCIP NO-UNDO
   FIELD ttName   AS CHARACTER
   FIELD ttFormat AS CHARACTER
   FIELD ttValue  AS CHARACTER.

DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeBalance AS DECIMAL   NO-UNDO.

RUN pPrePaidRequest(pcCLI).

lcResponse = RETURN-VALUE.

ldeBalance = DECIMAL(fGetRPCNodeValue(lcResponse,"accountValue1")) NO-ERROR.

OUTPUT TO /tmp/x.log APPEND.
PUT UNFORMATTED pcCLI " " ldeBalance CHR(10) lcResponse CHR(10).
OUTPUT CLOSE.

RETURN STRING(ldeBalance).

PROCEDURE pPrePaidRequest:

   DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lcProc    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcRequest AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcMethod  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcReturn  AS CHARACTER NO-UNDO.

   lcRequest = "ENQ".

   CASE lcRequest:
      /* recharge */
      WHEN "RCG" THEN ASSIGN
         lcRequest = "RefillTRequest"
         lcProc    = "p" + lcRequest.
      /* Query */
      WHEN "ENQ" THEN ASSIGN
         lcRequest = "BalanceEnquiryTRequest"
         lcProc    = "p" + lcRequest.
      /* Cancellation */
      WHEN "ANT" THEN ASSIGN
         lcRequest = "AdjustmentTRequest"
         lcProc    = "p" + lcRequest.
   END.

   RUN VALUE(lcProc) (pcCLI).

   lcMethod = RETURN-VALUE.

   /* prodedure to run is in response */
   IF INDEX(THIS-PROCEDURE:INTERNAL-ENTRIES,"p" + lcMethod) > 0 THEN DO:
      RUN pPrePaidPlatform(lcMethod, pcCLI).
      lcReturn =  RETURN-VALUE.
   END.
   ELSE
      lcReturn = lcMethod.

   RETURN lcReturn.

END PROCEDURE.

PROCEDURE pPrePaidPlatform:

   DEFINE INPUT PARAMETER pcMethod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcCLI    AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lhSAXWriter  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE llOK         AS LOGICAL   NO-UNDO. 
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
      lcURL    = "-H 217.168.3.233 -S 10010".

   CREATE SAX-WRITER lhSAXWriter. 
   
   lhSAXWriter:FORMATTED = FALSE. 
   
   llOK = lhSAXWriter:SET-OUTPUT-DESTINATION("memptr",lmXML).
   
   llOK = lhSAXWriter:START-DOCUMENT().
   llOK = lhSAXWriter:START-ELEMENT(lcRoot).

   llOK = lhSAXWriter:WRITE-DATA-ELEMENT("methodName",pcMethod).

   fRPCStruct("Start",lcStruct,lhSAXWriter).

   FOR EACH ttUCIP NO-LOCK: 

      ASSIGN
         lhTable = BUFFER ttUCIP:HANDLE
         lcTable = lhTable:NAME.

      llOK = lhSAXWriter:START-ELEMENT("member").

      lhField = lhTable:BUFFER-FIELD(1).
      llOK = lhSAXWriter:WRITE-DATA-ELEMENT("name",STRING(lhField:BUFFER-VALUE)).
    
      llOK = lhSAXWriter:START-ELEMENT("value").
    
      lhField = lhTable:BUFFER-FIELD(2).
      lcFormat = lhField:BUFFER-VALUE.

      lhField = lhTable:BUFFER-FIELD(3).
      llOK = lhSAXWriter:WRITE-DATA-ELEMENT(lcFormat,STRING(lhField:BUFFER-VALUE)).
    
      llOK = lhSAXWriter:END-ELEMENT("value"). 
    
      llOK = lhSAXWriter:END-ELEMENT("member").

   END. 

   fRPCStruct("End",lcStruct,lhSAXWriter).

   llOK = lhSAXWriter:END-ELEMENT(lcRoot).

   llOK = lhSAXWriter:END-DOCUMENT().

   DELETE OBJECT lhSAXWriter.

   EMPTY TEMP-TABLE ttUCIP.

   lcXML = GET-STRING(lmXML,1).

   lcHTTPHeader = "POST /Air HTTP/1.1~n"             +
                  "Content-Type: text/xml~n"         +
                  "Host: propus:10010~n"             +
                  "User-Agent: UGw Server/2.0/1.0~n" +
                  "Content-Length: " + STRING(LENGTH(lcXML)) + "~n~n".

   SET-SIZE(lmXML) = 0.

   /* wait only 6 seconds for response */
   RUN tg(lcHTTPHeader + lcXML,lcURL,3,2,"<").
   
   lcReturn = RETURN-VALUE.

   RETURN lcReturn.

END PROCEDURE.

PROCEDURE pHeader:

   DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.

   DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.

   DO liLoop = 1 TO 5:
      
      CREATE ttUCIP.
      
      CASE liLoop:
         WHEN 1 THEN ASSIGN
            ttUCIP.ttName   = "originNodeType"
            ttUCIP.ttValue  = "AIR"
            ttUCIP.ttFormat = "string".
         WHEN 2 THEN ASSIGN
            ttUCIP.ttName   = "originHostName"
            ttUCIP.ttValue  = "propus"
            ttUCIP.ttFormat = "string".
         WHEN 3 THEN ASSIGN
            ttUCIP.ttName   = "originTransactionID"
            ttUCIP.ttValue  = STRING(NEXT-VALUE(PrePaidReq),"999999999")
            ttUCIP.ttFormat = "string".
         WHEN 4 THEN ASSIGN
            ttUCIP.ttValue  = fISO860(fMakeTS())
            ttUCIP.ttName   = "originTimeStamp"
            ttUCIP.ttFormat = "dateTime.iso8601".
         WHEN 5 THEN ASSIGN
            ttUCIP.ttName   = "subscriberNumber"
            ttUCIP.ttValue  = pcCLI
            ttUCIP.ttFormat = "string".

      END.

   END.

END PROCEDURE.

PROCEDURE pBalanceEnquiryTRequest:

   DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.

   RUN pHeader(pcCLI).

   RETURN "BalanceEnquiryTRequest".

END PROCEDURE.
