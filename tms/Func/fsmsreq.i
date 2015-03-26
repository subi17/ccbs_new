/* fsmsreq.i         12.06.08/aam 

   create a request for sms message 
*/
   
{commali.i}
{fcreatereq.i}

FUNCTION fSMSRequest RETURNS INTEGER
   (INPUT  iiMsSeq      AS INT,    /* subscription         */
    INPUT  iiCreditType AS INT,    /* message type         */
    INPUT  icMsgSource  AS CHAR,   /* source of message text */
    INPUT  icMessage    AS CHAR,   /* message or keyvalue to get it  */
    INPUT  idActStamp   AS DEC,    /* when request should be handled */
    INPUT  icSource     AS CHAR,   /* where created */
    INPUT  icCreator    AS CHAR,   /* who made the request */
    INPUT  icSenderNbr  AS CHAR,   /* sms sender number */
    OUTPUT ocResult     AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF BUFFER bSMSOwner FOR MsOwner.

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   fCreateRequest(30,
                  idActStamp,
                  icCreator,
                  FALSE,    /* CreateFees */
                  TRUE).    /* sms */

   IF NOT AVAILABLE bReqSub THEN DO:
      FIND FIRST bSMSOwner WHERE bSMSOwner.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bSMSOwner THEN DO:
         DELETE bCreaReq.
         ocResult = "Subscription not found".
         RETURN 0.
      END.

      ASSIGN 
          bCreaReq.MsSeq      = bSMSOwner.MsSeq
          bCreaReq.CLI        = bSMSOwner.CLI
          bCreaReq.CustNum    = bSMSOwner.CustNum.
   END.
   
   ASSIGN bCreaReq.ReqSource   = icSource
          bCreaReq.ReqIParam1  = iiCreditType
          bCreaReq.ReqCParam1  = icMsgSource
          bCreaReq.ReqCParam3  = icSenderNbr
          liReqCreated         = bCreaReq.MsRequest.

   /* icMsgSource:
      - 'free' = icMessage contains a ready text
      - empty or 'invtext' = get the text from InvText using icMessage as
        InvText.KeyValue
   */
   IF icMsgSource = "free" 
   THEN bCreaReq.ReqCParam2 = icMessage.
   ELSE bCreaReq.SMSText    = icMessage.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

