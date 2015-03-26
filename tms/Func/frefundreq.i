/* frefundreq.i         30.08.07/aam 

   create a request for refund payment 
*/
   
{commali.i}
{fcreatereq.i}

FUNCTION fRefundRequest RETURNS INTEGER
   (INPUT  iiCustNum     AS INT,  
    INPUT  iiInvNum      AS INT,
    INPUT  iiVoucher     AS INT,    /* payment that is to be refunded */
    INPUT  iiRefundType  AS INT,    /* 1=to adv.payment, 2=refund payment */
    INPUT  idRefundAmt   AS DEC,    /* amount */
    INPUT  iiPaymType    AS INT,    /* 1=money,2=overp.,3=adv.paym */
    INPUT  icCustBank    AS CHAR,   /* customer's bank account */
    INPUT  iiCustBal     AS INT,    /* posted to customer refund balance */
    INPUT  icReason      AS CHAR,   /* reason for refunding */
    INPUT  idActStamp    AS DEC,    /* when request should be handled */
    INPUT  ilSendSMS     AS LOG,
    INPUT  icCreator     AS CHAR,
    OUTPUT ocResult      AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.

   ocResult = fChkRequest(iiCustNum,
                          23,
                          STRING(iiVoucher),
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   fCreateRequest(23,
                  idActStamp,
                  icCreator,
                  FALSE,      /* fees */
                  ilSendSMS).

   ASSIGN 
      bCreaReq.CustNum    = iiCustNum
      bCreaReq.ReqIParam1 = iiInvNum
      bCreaReq.ReqIParam2 = iiVoucher
      bCreaReq.ReqIParam3 = iiPaymType
      bCreaReq.ReqIParam4 = iiCustBal
      bCreaReq.ReqDParam1 = idRefundAmt
      bCreaReq.ReqDParam2 = iiRefundType
      bCreaReq.ReqCParam1 = icReason
      bCreaReq.ReqCParam2 = icCustBank
      liReqCreated        = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

