&IF "{&barr_request_i}" NE "YES"
&THEN

&GLOBAL-DEFINE barr_request_i YES

{Func/fmakemsreq.i}

FUNCTION fCreateBP RETURNS LOGICAL
(INPUT icPackage AS CHARACTER,
 INPUT iiInvCust AS INTEGER,
 INPUT icCli     AS CHARACTER,
 INPUT iiMsSeq   AS INTEGER,
 INPUT icCLB     AS CHARACTER,
 INPUT iiMandReq AS INTEGER,
 INPUT icSource  AS CHARACTER,
 INPUT icCreator AS CHARACTER,
 INPUT idActStamp AS DECIMAL,
 INPUT icSMSText  AS CHAR,
 INPUT icPrevBarr AS CHAR,
 OUTPUT oiCrReqId AS INTEGER):
   
   fCreateRequest(35,idActStamp,icCreator,FALSE,FALSE).

    
   ASSIGN bCreaReq.ReqCParam1  = icPackage
          bCreaReq.CustNum     = iiInvCust
          bCreaReq.Cli         = icCli
          bCreaReq.MsSeq       = iiMsSeq
          bCreaReq.ReqCParam2  = icCLB
          bCreaReq.ReqCParam4  = icPrevBarr 
          /* another request must be completed before this request */
          bCreaReq.ReqIParam2  = iiMandReq
          bCreaReq.ReqSource   = icSource 
          oiCrReqId            = bCreaReq.MsRequest.

   /* Need to send SMS (fraud tool) */
   IF icSMSText > "" THEN ASSIGN bCreaReq.SMSText   = icSMSText
                                 bCreaReq.SendSMS   = 1.
   RELEASE bCreaReq.
   
   RETURN TRUE.

END FUNCTION.

&ENDIF