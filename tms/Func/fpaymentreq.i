/* fpayment.i         11.09.07/aam 

   create a request for a payment 
*/
   
{fcreatereq.i}

/* caller can just give the amount in idPaymAmt, and let the request take 
   care of posting rules and accounts (fPaymentRequest)
   or
   give all posting amounts and their respective accounts in ":" separated
   lists in icPostings and icAccounts (fPaymentWithPostingsRequest)
*/

FUNCTION fPaymentWithPostingsRequest RETURNS INTEGER
   (INPUT  iiCustNum     AS INT,  
    INPUT  iiPaymType    AS INT,    /* payment type */
    INPUT  icPaymSrc     AS CHAR,   /* payment source */
    INPUT  idtPaymDate   AS DATE,   /* payment date (not posting date) */
    INPUT  iiInvNum      AS INT,
    INPUT  idPaymAmt     AS DEC,    /* amount (1. posting) */
    INPUT  icPostings    AS CHAR,   /* postings, not mandatory */
    INPUT  icAccounts    AS CHAR,   /* accounts, not mandatory */
    INPUT  icMemo        AS CHAR,   /* memo text */
    INPUT  idActStamp    AS DEC,    /* posting time */
    INPUT  iiControl     AS INT,    /* does payment need manual control */
    INPUT  iiInterest    AS INT,    /* can interest be calculated */
    INPUT  icCreator     AS CHAR,
    OUTPUT ocResult      AS CHAR):

   DEF VAR liReqCreated AS INT NO-UNDO.

   /* no need to check pending requests or other request types, one customer
      and/or invoice can have as many payments as desired */

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   fCreateRequest(31,
                  idActStamp,
                  icCreator,
                  FALSE,      /* fees */
                  FALSE).     /* sms */
                
   ASSIGN 
      bCreaReq.CustNum     = iiCustNum
      bCreaReq.ReqIParam1  = iiInvNum
      bCreaReq.ReqIParam2  = iiPaymType
      bCreaReq.ReqIParam3  = iiControl
      bCreaReq.ReqIParam4  = iiInterest
      bCreaReq.ReqDParam1  = idPaymAmt
      bCreaReq.ReqDtParam1 = idtPaymDate
      bCreaReq.ReqCParam1  = icPaymSrc
      bCreaReq.ReqCParam2  = icPostings
      bCreaReq.ReqCParam3  = icAccounts
      bCreaReq.ReqCParam4  = icMemo
      liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

/* simpler creation, not so much parameters to adjust */
FUNCTION fPaymentRequest RETURNS INTEGER
   (INPUT  iiCustNum     AS INT,  
    INPUT  iiPaymType    AS INT,    /* payment type */
    INPUT  icPaymSrc     AS CHAR,   /* payment source */
    INPUT  idtPaymDate   AS DATE,   /* payment date (not posting date) */
    INPUT  iiInvNum      AS INT,
    INPUT  idPaymAmt     AS DEC,    /* amount (1. posting) */
    INPUT  iiDebitAcc    AS INT,    /* debit account  */
    INPUT  icMemo        AS CHAR,   /* memo text */
    INPUT  idActStamp    AS DEC,    /* posting time */
    INPUT  icCreator     AS CHAR,
    OUTPUT ocResult      AS CHAR):

   RETURN fPaymentWithPostingsRequest(iiCustNum,
                                      iiPaymType,
                                      icPaymSrc,
                                      idtPaymDate,
                                      iiInvNum,
                                      idPaymAmt,
                                      "",
                                      IF iiDebitAcc > 0
                                      THEN STRING(iiDebitAcc)
                                      ELSE "",
                                      icMemo,
                                      idActStamp,
                                      0,
                                      1,
                                      icCreator,
                                      OUTPUT ocResult).

END FUNCTION.


