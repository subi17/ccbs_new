/* ----------------------------------------------------------------------
  MODULE .......: duplicate_invoice.i
  TASK .........: duplicate invoice request functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 14.12.09
  Version ......: xfera
----------------------------------------------------------------------- */
{fcreatereq.i}
{tmsconst.i}

DEF BUFFER bInvoiceDup FOR Invoice.

FUNCTION fDuplicateInvoiceValidate RETURNS LOGICAL
   (BUFFER ibDupInvoice FOR Invoice,
    OUTPUT ocResult AS CHARACTER):

   ocResult = "".

   IF ibDupInvoice.InvType NE {&INV_TYPE_NORMAL} THEN
      ocResult = "Wrong invoice type".
   
   ELSE IF ibDupInvoice.PrintState NE {&INV_PRINTSTATE_PRINTHOUSE} THEN
      ocResult = "Invoice is not delivered".
   
   ELSE IF ibDupInvoice.InvCfg[1] = TRUE THEN
      ocResult = "Invoice printing is denied".
   
   ELSE IF ibDupInvoice.DueDate + 15 > TODAY THEN
      ocResult = "Incorrect invoice due date".
       
   ELSE IF NOT CAN-FIND(Customer NO-LOCK WHERE 
                        Customer.CustNum = ibDupInvoice.Custnum) THEN
      ocResult = "Customer was not found".
   
   ELSE IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
      MsRequest.Brand   = gcBrand AND
      MsRequest.ReqType = {&REQTYPE_DUPLICATE_INVOICE} AND
      MsRequest.CustNum = ibDupInvoice.CustNum AND
      MsRequest.ReqIParam1 = ibDupInvoice.InvNum AND
      LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0) THEN 
      ocResult = {&MSG_ONG_REQUEST}.

   RETURN ocResult EQ "". 

END FUNCTION. 

FUNCTION fDuplicateInvoiceRequest RETURNS INTEGER
   (INPUT  iiInvNum      AS INT,
    INPUT  idActStamp    AS DEC,    /* when request should be handled */
    INPUT  icCreator     AS CHAR,
    INPUT  icSource      AS CHAR,
    OUTPUT ocResult      AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF VAR liDay AS INTEGER NO-UNDO.
   DEF VAR liMonth AS INTEGER NO-UNDO.
   DEF VAR liYear AS INTEGER NO-UNDO.

   FIND bInvoiceDup WHERE
        bInvoiceDup.InvNum = iiInvNum NO-LOCK NO-ERROR.

   IF NOT AVAIL bInvoiceDup THEN DO:
      ocResult = "Invoice not found".
      RETURN 0.
   END.
         
   IF NOT fDuplicateInvoiceValidate(
      BUFFER bInvoiceDup,
      OUTPUT ocResult) THEN RETURN 0.

   /* set activation time to next DOC1 run moment */
   IF idActStamp = 0 OR idActStamp = ? THEN DO:

      ASSIGN
         liDay = DAY(TODAY)
         liMonth = MONTH(TODAY)
         liYear = YEAR(TODAY).
      
      /* set handling time to next possible 10th or 22th day of the month */
      IF liDay < 10 THEN liDay = 10.
      ELSE IF liDay >= 10 AND liDay < 22 THEN liDay = 22.
      ELSE DO:
         IF liMonth = 12 THEN ASSIGN
            liMonth = 1 
            liYear = liYear + 1.
         ELSE liMonth = liMonth + 1.
         liDay = 10.
      END.
   
      idActStamp = fHMS2TS(DATE(liMonth,liDay,liYear),"00:00:00").
   END.

   fCreateRequest({&REQTYPE_DUPLICATE_INVOICE},
                  idActStamp,
                  icCreator,
                  FALSE,      /* fees */
                  FALSE).    /* send sms */

   ASSIGN 
      bCreaReq.CustNum    = bInvoiceDup.Custnum
      bCreaReq.ReqIParam1 = bInvoiceDup.InvNum
      bCreaReq.ReqCParam1 = bInvoiceDup.ExtInvId 
      bCreaReq.ReqSource  = icSource
      liReqCreated        = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

