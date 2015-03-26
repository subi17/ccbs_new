/* fmpricereq.i         21.09.07/aam 

   create a price change request 
*/

{commali.i}   
{fcreatereq.i}

FUNCTION fFeeModelPriceRequest RETURNS INTEGER
   (INPUT  icBillCode    AS CHAR,   /* billing item */
    INPUT  iiPayType     AS INT,    /* 0=both, 1=prepaid, 2=postpaid */
    INPUT  iiMSISDNType  AS INT,    /* 0=both, 1=mnp, 2=new */
    INPUT  idNewPrice    AS DEC,    /* price */
    INPUT  idtValidFrom  AS DATE,   /* begin date for new price */
    INPUT  idActStamp    AS DEC,    /* when request is to be handled */
    INPUT  icCreator     AS CHAR,
    OUTPUT ocResult      AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.

   /* this request concerns basic configuration data, not an individual
      customer or mobsub, therefore general functions for validations
      cannot be used */
   FOR EACH MsRequest NO-LOCK USE-INDEX ReqCParam1 WHERE
            MsRequest.MsSeq      = 0          AND
            MsRequest.ReqType    = 27         AND
            MsRequest.ReqCParam1 = icBillCode AND
            MsRequest.ReqStatus  = 0:

      CASE iiPayType:
      WHEN 0 THEN ocResult = "P".
      WHEN 1 THEN DO:
         IF MsRequest.ReqIParam1 <= 1 THEN ocResult = "P".
      END.
      WHEN 2 THEN DO:
         IF MsRequest.ReqIParam1 = 0 OR
            MsRequest.ReqIParam1 = 2 THEN ocResult = "P".
      END.
      END CASE.
      
      CASE iiMSISDNType:
      WHEN 0 THEN ocResult = "M".
      WHEN 1 THEN DO:
         IF MsRequest.ReqIParam2 <= 1 THEN ocResult = "M".
      END.
      WHEN 2 THEN DO:
         IF MsRequest.ReqIParam2 = 0 OR
            MsRequest.ReqIParam2 = 2 THEN ocResult = "M".
      END.
      END CASE.
        
   END.

   IF ocResult > "" THEN DO:
      ocResult = "An active request already exists with given parameters".
      RETURN 0.
   END.

   /* set activation time by default to tomorrow if not given */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMake2Dt(TODAY + 1,1).

   fCreateRequest(27,
                  idActStamp,
                  icCreator,
                  FALSE,      /* fees */
                  FALSE).     /* sms */ 
   ASSIGN 
      bCreaReq.ReqCParam1  = icBillCode
      bCreaReq.ReqDParam1  = idNewPrice
      bCreaReq.ReqDtParam1 = idtValidFrom
      bCreaReq.ReqIParam1  = iiPayType
      bCreaReq.ReqIParam2  = iiMSISDNType
      liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

