{Syst/commali.i} 
{Func/xmlfunction.i}
{Func/mathfunction.i}
{Func/msreqfunc.i}
{Func/timestamp.i}

DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,'angetenar,alpheratz,sadachbia,yanai') = 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
   lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.

DEFINE INPUT PARAMETER iiRequest AS INT             NO-UNDO.

DEF VAR llACC AS LOG  NO-UNDO.

DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResp     AS CHARACTER NO-UNDO.

RUN pCreditCheckRequest(iiREquest).

lcResponse = RETURN-VALUE.

IF lcResponse = "A" THEN fReqStatus(2,lcResp).
ELSE DO:
   fReqError(lcResponse).

   IF llACC THEN
      RUN Mm/acc_sendsms(MsRequest.OrigRequest,
                      MsRequest.CustNum,
                      "Rejected",
                      "HT:309").
END.                      

PROCEDURE pCreditCheckRequest:

   DEFINE INPUT PARAMETER piRequest AS INT  NO-UNDO.

   DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO.  

   RUN pHeader(pirequest).

   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE. 

   lcReturn = "A".
   RETURN lcReturn.

END PROCEDURE.

PROCEDURE pHeader:

   DEFINE INPUT PARAMETER iiMSRequest AS INT NO-UNDO.
   DEF BUFFER bOrigRequest FOR MsRequest.
   
   FIND FIRST MSrequest WHERE 
              MSRequest.Brand     = gcBrand    AND
              MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.
   
   llACC = FALSE.
   
   IF MsRequest.OrigRequest > 0 THEN DO:            
   
      FIND FIRST bOrigRequest WHERE 
         bOrigRequest.MsRequest = MsRequest.OrigRequest NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bOrigRequest THEN 
         RETURN "ERROR:Original request missing".
         
      IF bOrigRequest.ReqType = 10 THEN llACC = TRUE.
   END.
  
   IF NOT llACC THEN DO:
      FIND FIRST Customer WHERE 
                 Customer.CustNum = MSRequest.CustNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Customer THEN RETURN "ERROR:Customer missing". 
   END. 

   RETURN "".
   
END PROCEDURE.

