/* ----------------------------------------------------------------------
  MODULE .......: dextra.i 
  TASK .........: common dextra api functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 26.03.10
  Version ......: xfera
----------------------------------------------------------------------- */
{commali.i}
{fgettxt.i}
{date.i}
{fmakesms.i}
{tmsconst.i}
{fcreatereq.i}

FUNCTION fSendDextraSMS RETURNS LOGICAL
(iiOrderId AS INTEGER,
 iiLOStatusId AS INTEGER,
 iiCourierID AS INTEGER):
      
   DEFINE VARIABLE lcSMSText AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldReqStamp AS DECIMAL NO-UNDO.
   DEFINE VARIABLE lcNumber AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLanguage AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liCASeq AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcSMSToken AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liNameMaxLength AS INTEGER NO-UNDO. 

   DEF BUFFER Order FOR Order.
   
   FIND FIRST Order NO-LOCK WHERE
              Order.Brand   = gcBrand   AND
              Order.OrderID = iiOrderId NO-ERROR.
    
   IF NOT AVAIL Order THEN RETURN FALSE.
   
   IF iiCourierID NE {&COURIER_ID_CORREOS} AND
      iiCourierID NE {&COURIER_ID_SEUR} AND
      iiCourierID NE {&COURIER_ID_ASM} THEN RETURN FALSE.

   CASE iiLOStatusId:
      WHEN 6 THEN DO: 
         IF Order.DeliverySecure EQ 1 THEN
            lcSMSToken = "LogisticDelivery_SECUR".
         ELSE lcSMSToken = "LogisticDelivery_ALL".
      END.   
      WHEN 9 THEN DO:
         IF iiCourierID = {&COURIER_ID_CORREOS} THEN
            lcSMSToken = "LogisticIncident_CORREOS".
         ELSE IF iiCourierID = {&COURIER_ID_SEUR} THEN
            lcSMSToken = "LogisticIncident_SEUR".
         ELSE lcSMSToken = "LogisticIncident_ASM".
      END.
      WHEN 10 THEN DO:
         IF iiCourierID = {&COURIER_ID_CORREOS} THEN
            lcSMSToken = "LogisticNotDeliv_CORREOS".
         ELSE IF iiCourierID = {&COURIER_ID_SEUR} THEN
            lcSMSToken = "LogisticNotDeliv_SEUR".
         ELSE lcSMSToken = "LogisticNotDeliv_ASM".
      END.
      OTHERWISE RETURN FALSE.
   END.

   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = gcBrand AND
              OrderCustomer.Orderid = iiOrderId AND
              OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.

   IF Order.MNPStatus > 0 THEN
      lcNumber = Order.CLI.
   ELSE IF OrderCustomer.MobileNumber BEGINS "6" THEN
      lcNumber = OrderCustomer.MobileNumber.
   ELSE IF OrderCustomer.FixedNumber BEGINS "6" THEN
      lcNumber = OrderCustomer.FixedNumber.

   IF lcNumber EQ "" THEN RETURN FALSE.

   liLanguage = INT(OrderCustomer.Language) NO-ERROR.
   lcSMSText = fGetSMSTxt(lcSMSToken,
                          TODAY,
                          liLanguage,
                          OUTPUT ldReqStamp).

   IF lcSMSText = "" THEN RETURN FALSE.
      
   liNameMaxLength = MAXIMUM(160 - LENGTH(lcSMSText) + 5,0).
   lcSMSText = REPLACE(lcSMSText,"#NAME",
                       SUBSTRING(OrderCustomer.FirstName, 1, liNameMaxLength)).

   liCASeq = fMakeSchedSMS2(OrderCustomer.CustNum,
                 lcNumber,
                 43,
                 lcSMSText,
                 ldReqStamp,
                 "622",
                 "").

   RETURN (liCASeq > 0).

END FUNCTION. 

FUNCTION fLogisticsRequest RETURNS INTEGER
    (INPUT  iiMSSeq       AS INT,    /* MSSeq */
     INPUT  iiOrderId     AS INT,
     INPUT  icAction      AS CHAR,
     INPUT  idActStamp    AS DEC,    /* when request should be handled */
     INPUT  icReqSource   AS CHAR,
     OUTPUT ocResult      AS CHAR).

   DEF VAR liRequest AS INT  NO-UNDO.
   DEF BUFFER bOrder FOR Order.

   FIND bOrder NO-LOCK WHERE
        bOrder.Brand = gcBrand AND
        bOrder.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL bOrder THEN DO:
      ocResult = "ERROR: Order not found".
      RETURN 0.
   END.
   
   ocResult = fChkRequest(iiMsSeq,
                          {&REQTYPE_LOGISTICS},
                          "",
                          "").
    
   IF ocResult > "" THEN RETURN 0.
   
   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   fCreateRequest({&REQTYPE_LOGISTICS},
                  idActStamp,
                  "",
                  FALSE,
                  FALSE). /* send sms */

   ASSIGN 
      liRequest         = bCreaReq.MsRequest
      bCreaReq.MsSeq    = iiMsSeq
      bCreaReq.CLI      = bOrder.CLI
      bCreaReq.ReqIparam1  = iiOrderId
      bCreaReq.ReqCparam1  = icAction
      bCreaReq.ReqSource   = icReqSource.

   RELEASE bCreaReq.
   
   RETURN liRequest.
     
END FUNCTION.

