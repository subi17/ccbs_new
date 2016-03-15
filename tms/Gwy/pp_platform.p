{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/xmlfunction.i}
{Gwy/airnodes.i}

DEFINE INPUT PARAMETER pcBrand AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piPPReq AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTCPModule  AS CHARACTER NO-UNDO INITIAL "Gwy/tcpgwy.p" . 

FIND FIRST TMSParam where
           TMSParam.Brand      = gcBrand AND 
           TMSParam.ParamCode  =  "TCPModule" NO-LOCK NO-ERROR.
IF AVAIL TMSParam THEN 
         lcTCPModule = TMSParam.CharVal.

FIND FIRST PrePaidRequest WHERE
           PrePaidRequest.Brand     = pcBrand AND
           PrePaidRequest.PPRequest = piPPReq
NO-LOCK NO-ERROR.

DEFINE TEMP-TABLE ttUCIP NO-UNDO
   FIELD ttName   AS CHARACTER
   FIELD ttFormat AS CHARACTER
   FIELD ttValue  AS CHARACTER.

DEFINE VARIABLE lcRoot     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMethod   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStruct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProc     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcURL      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRequest  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcHTTPHdr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPPStatus AS INTEGER   NO-UNDO.

ASSIGN
   lcProc                  = "p" + PrePaidRequest.Request
   lcRoot                  = "methodCall"
   lcStruct                = "params,param,value,struct".
   
lcUrl = fGetAIRNode().

IF LOOKUP(lcProc,THIS-PROCEDURE:INTERNAL-ENTRIES) > 0 THEN DO:

   RUN VALUE(lcProc).

   lcMethod = RETURN-VALUE.

   RUN pCreateXML(lcMethod,        
                  OUTPUT lcRequest, 
                  OUTPUT lcResponse).
   liPPStatus = 2.               
END.

ELSE ASSIGN
   lcResponse = "Invalid request type"
   liPPStatus = 3.

DO TRANSACTION:

   FIND FIRST PrePaidRequest WHERE
              PrePaidRequest.Brand     = pcBrand AND
              PrePaidRequest.PPRequest = piPPReq
   EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   IF LOCKED(PrePaidRequest) THEN DO:

      FIND FIRST PrePaidRequest WHERE
                 PrePaidRequest.Brand     = pcBrand AND
                 PrePaidRequest.PPRequest = piPPReq
      NO-LOCK NO-ERROR.

      OUTPUT TO /scratch/nagios/tms/ivr/ivr_pp_locked.txt APPEND.
      PUT UNFORMATTED "New TopUp: " fTS2HMS(fMakeTS()) " " PrePaidRequest.CLI CHR(10) lcResponse CHR(10).
      OUTPUT CLOSE.

   END.

   ELSE DO:

      ASSIGN 
         PrePaidRequest.Response = lcResponse
         PrePaidRequest.CommLine = lcRequest
         PrePaidRequest.PPStatus = liPPStatus.
   END.

END.

PROCEDURE pCreateXML:

   DEFINE INPUT  PARAMETER pcMethod   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER ocRequest  AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER ocResponse AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lhSAXWriter  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE llOK         AS LOGICAL   NO-UNDO. 
   DEFINE VARIABLE lcFormat     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lhField      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhTable      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lcTable      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lmXML        AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE lcXML        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcHTTPheader AS CHARACTER NO-UNDO.
   
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

   IF pcMethod EQ "Refill" THEN
   lcHTTPHeader = "POST /Air HTTP/1.1~n"             +
                  "Content-Type: text/xml~n"         +
                  "Host: propus:10010~n"             +
                  "User-Agent: UGw Server/4.2/1.0~n" +
                  "Authorization: Basic eW9pZ286eW9pZ28xMgo=~n" + 
                  "Content-Length: " + STRING(LENGTH(lcXML)) + "~n~n".
   ELSE
   lcHTTPHeader = "POST /Air HTTP/1.1~n"             +
                  "Content-Type: text/xml~n"         +
                  "Host: propus:10010~n"             +
                  "User-Agent: UGw Server/2.0/1.0~n" +
                  "Content-Length: " + STRING(LENGTH(lcXML)) + "~n~n".

   SET-SIZE(lmXML) = 0.

   ocRequest = lcXML. 

   RUN VALUE(lcTCPModule) (lcHTTPHeader + lcXML,lcURL,30,2,"<").

   ocResponse = RETURN-VALUE.

END PROCEDURE.

PROCEDURE pHeader:

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
            ttUCIP.ttValue  = PrePaidRequest.PPReqPrefix + 
                              STRING(PrePaidRequest.PPRequest,"999999999")
            ttUCIP.ttFormat = "string".
         WHEN 4 THEN ASSIGN
            ttUCIP.ttName            = "originTimeStamp"
            ttUCIP.ttValue           = fISO860(PrePaidRequest.TSRequest)
            ttUCIP.ttFormat          = "dateTime.iso8601".
         WHEN 5 THEN ASSIGN
            ttUCIP.ttName   = "subscriberNumber"
            ttUCIP.ttValue  = PrePaidRequest.CLI
            ttUCIP.ttFormat = "string".
      END.

   END.

END PROCEDURE.

PROCEDURE pAdjustmentTRequest:

   DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.

   RUN pHeader.

   DO liLoop = 1 TO 2:
      
      CREATE ttUCIP.
      
      CASE liLoop:
         WHEN 1 THEN ASSIGN
            ttUCIP.ttName   = "transactionCurrency"
            ttUCIP.ttValue  = "EUR"
            ttUCIP.ttFormat = "string".
         WHEN 2 THEN ASSIGN
            ttUCIP.ttName   = "adjustmentAmount"
            ttUCIP.ttValue  = STRING(INT(PrePaidRequest.TopUpAmt))
            ttUCIP.ttFormat = "string".
      END.

   END.

   /* Topup for prepaid subscriptions */
   IF PrePaidRequest.PPReqPrefix = "970" THEN DO:
      CREATE ttUCIP.
      ASSIGN ttUCIP.ttName   = "messageCapabilityFlag"
             ttUCIP.ttValue  = "00100000"
             ttUCIP.ttFormat = "string".
   END. /* IF PrePaidRequest.PPReqPrefix = "970" THEN DO: */
   
   RETURN "AdjustmentTRequest".

END PROCEDURE.

PROCEDURE pGetAccountDetailsTRequest:

   RUN pHeader.

   ASSIGN
      ttUCIP.ttName   = "messageCapabilityFlag"
      ttUCIP.ttValue  = "01100000"
      ttUCIP.ttFormat = "string".

   RETURN "GetAccountDetailsTRequest".
   
END PROCEDURE.

PROCEDURE pStandardVoucherRefillTRequest:

   DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.

   RUN pHeader.

   DO liLoop = 1 TO 2:
      
      CREATE ttUCIP.
      
      CASE liLoop:
         WHEN 1 THEN ASSIGN
            ttUCIP.ttName   = "activationNumber"
            ttUCIP.ttValue  = "686372095655"
            ttUCIP.ttFormat = "string".
         WHEN 2 THEN ASSIGN
            ttUCIP.ttName   = "messageCapabilityFlag"
            ttUCIP.ttValue  = "00000000"
            ttUCIP.ttFormat = "string".

      END.

   END.
   
   RETURN "StandardVoucherRefillTRequest".

END PROCEDURE.

PROCEDURE pRefillTRequest:

   DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.
   DEF VAR ldeTime AS DEC NO-UNDO. 
   DEF VAR lcPaymentProfileID AS CHAR NO-UNDO. 

   RUN pHeader.

   /* TODO: PaymentProfileID = 4 not supported yet */
   /*
   ldeTime = fMakeTS().

   FIND FIRST MSOwner WHERE
              MSOwner.CLI = PrepaidRequest.CLI AND
              MSOwner.TSBegin <= ldeTime AND
              MSOwner.TSEnd >= ldeTime NO-LOCK NO-ERROR.

   /* Top-Ups for TARJ5 must be done with correct PaymentProfileID to ensure
      promotional price activation works correctly in prepaid platform. */
   IF AVAIL MSOwner AND MSOwner.CLIType = "TARJ5" THEN lcPaymentProfileID = "4".
   ELSE lcPaymentProfileID = "1".
   */
   lcPaymentProfileID = "1".

   DO liLoop = 1 TO 3:
      
      CREATE ttUCIP.
      
      CASE liLoop:
         WHEN 1 THEN ASSIGN
            ttUCIP.ttName   = "transactionAmount"
            ttUCIP.ttValue  = STRING(INT(PrePaidRequest.TopUpAmt))
            ttUCIP.ttFormat = "string".
         WHEN 2 THEN ASSIGN
            ttUCIP.ttName   = "transactionCurrency"
            ttUCIP.ttValue  = "EUR"
            ttUCIP.ttFormat = "string".
         WHEN 3 THEN ASSIGN
            ttUCIP.ttName   = "refillProfileID"
            ttUCIP.ttValue  = lcPaymentProfileID
            ttUCIP.ttFormat = "string".
      END.

   END.
   
   RETURN "Refill".

END PROCEDURE.

PROCEDURE pBalanceEnquiryTRequest:

   RUN pHeader.

   RETURN "BalanceEnquiryTRequest".

END PROCEDURE.
