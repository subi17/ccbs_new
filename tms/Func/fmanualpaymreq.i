/* fmanualpaymreq.i      05.11.07/aam 

   create a request for a manual payment 
*/
   
{Syst/commali.i}
{Func/fcreatereq.i}

FUNCTION fManualPaymentRequest RETURNS INTEGER
   (INPUT  iiCustNum     AS INT,  
    INPUT  icCLI         AS CHAR,
    INPUT  iiInvNum      AS INT,
    INPUT  iiManualType  AS INT,    /* type of manual payment */
    INPUT  idAmt         AS DEC,    /* paid amount */
    INPUT  idtPaymDate   AS DATE,
    INPUT  icMemo        AS CHAR,   
    INPUT  idActStamp    AS DEC,    /* when request should be handled */
    INPUT  icCreator     AS CHAR,
    OUTPUT ocResult      AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.

   /* another request already pending */
   IF CAN-FIND(FIRST MsRequest USE-INDEX CustNum WHERE
                     MsRequest.Brand      = gcBrand           AND
                     MsRequest.ReqType    = 34                AND
                     MsRequest.CustNum    = Customer.CustNum  AND
                     LOOKUP(STRING(MsRequest.ReqStatus),"0,1,3") > 0)
   THEN DO:
      ocResult = "Customer has another manual payment request pending".
      RETURN 0.
   END. 

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   fCreateRequest(34,
                  idActStamp,
                  icCreator,
                  FALSE,      /* fees */
                  FALSE).     /* sms */

   ASSIGN 
      bCreaReq.CustNum     = iiCustNum
      bCreaReq.ReqIParam1  = iiInvNum
      bCreaReq.ReqIParam2  = iiManualType
      bCreaReq.ReqDParam1  = idAmt
      bCreaReq.ReqDtParam1 = idtPaymDate
      bCreaReq.ReqCParam1  = icCLI
      bCreaReq.ReqCParam2  = icMemo
      liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

