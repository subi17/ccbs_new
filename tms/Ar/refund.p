/* ----------------------------------------------------------------------------
  MODULE .......: refund.p
  FUNCTION .....: refund payment handling
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 04.09.07
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{msreqfunc.i}

{finvbal.i}
{faccper.i}
{fcustbal.i}
{fcustcnt.i}
{eventval.i} 
{fhdrtext.i}
{fbankdata.i}
{fpaymentreq.i}

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

DEF BUFFER bSubRequest FOR MsRequest.


FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 23 THEN RETURN "ERROR".

CASE MsRequest.ReqStat:
WHEN 0 OR 
WHEN 15 THEN RUN pRefund.
WHEN 17 THEN RUN pCreateSubRequests.
WHEN 8  THEN RUN pDone.
END CASE.

/* refund file is created in refundfileb.p (status 16) */

RETURN RETURN-VALUE.


PROCEDURE pRefund:

   DEF VAR ldActStamp    AS DEC  NO-UNDO. 
   DEF VAR lcError       AS CHAR NO-UNDO. 
   DEF VAR ldBalance     AS DEC  NO-UNDO.
   DEF VAR liReqStat     AS INT  NO-UNDO.
   DEF VAR liDDCancel    AS INT  NO-UNDO.
   DEF VAR liFileDate    AS INT  NO-UNDO.
   DEF VAR liMonth       AS INT  NO-UNDO.
   DEF VAR liYear        AS INT  NO-UNDO.

   liReqStat = MsRequest.ReqStat.
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   /* customer must have a valid bank account */
   IF LENGTH(Customer.BankAcc) < 24 OR 
      NOT fCheckBankAcc(Customer.BankAcc) THEN DO:
      fReqError("Invalid bank account on customer").
      RETURN.
   END.

   /* in 1. phase mark request to status 19 for manual approval, 
      move to adv.payment can be done straight away */
   IF liReqStat = 0 AND MsRequest.ReqDParam2 = 2 THEN DO:
   
      /* limit for manual approval */
      ldReqAmt = fCParamDE("RefundApprovalLimit").

      IF MsRequest.ReqDParam1 >= ldReqAmt THEN DO:
         fReqStatus(19,"").
         RETURN.
      END.   
   END.

   IF MsRequest.ReqIParam1 > 0 THEN DO:
      FIND Invoice WHERE Invoice.InvNum = MsRequest.ReqIParam1 
         NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE Invoice THEN DO:
         fReqError("Invoice not found").
         RETURN.
      END.
 
      /* refund cannot be processed before the dd cancel time has passed */
      IF Invoice.ChargeType = 2 AND Invoice.DDState = 1 THEN DO:
         liDDCancel = fCParamI("DDCancelDays").
         IF liDDCancel = ? THEN liDDCancel = 0.
      
         IF Invoice.DueDate + liDDCancel >= TODAY THEN DO:
            /* better to mark to error status than back to 0/15 */
            fReqError("DD can still be cancelled").
            RETURN.
         END.
      END.
   
   END.
   
   /* advance payment can be handled straight away */
   IF MsRequest.ReqDParam2 = 1 THEN DO:
      fReqStatus(17,"").
   END.
   
   /* if returned to customer via bank then move to file creation queue */
   ELSE IF MsRequest.ReqDParam2 = 2 THEN DO:
   
      FIND CURRENT MsRequest EXCLUSIVE-LOCK.
         
      /* set payment date, if not set manually */
      IF MsRequest.ReqDtParam1 = ? OR 
         MsRequest.ReqDtParam1 < TODAY + 1
      THEN DO:

         /* next possible file date */
         ASSIGN 
            liFileDate = 0
            lcReqChar  = fCParamC("RefundFileDays").
         IF lcReqChar = ? OR lcReqChar = "" THEN 
            lcReqChar = STRING(DAY(TODAY) + 1). 
         
         DO liReqCnt = 1 TO NUM-ENTRIES(lcReqChar):
            IF INTEGER(ENTRY(liReqCnt,lcReqChar)) > DAY(TODAY) THEN DO:
               liFileDate = INTEGER(ENTRY(liReqCnt,lcReqChar)).
               LEAVE.
            END.
         END.

         IF liFileDate = 0 THEN liFileDate = INTEGER(ENTRY(1,lcReqChar)).  
       
         ASSIGN 
            liMonth = MONTH(TODAY) + INTEGER(liFileDate <= DAY(TODAY))
            liYear  = YEAR(TODAY).
         
         IF liMonth = 13 THEN ASSIGN
            liMonth = 1
            liYear  = liYear + 1.
      
         IF (liFileDate > 28 AND liMonth = 2) OR 
            (liFileDate = 31 AND liMonth < 12) 
         THEN MsRequest.ReqDtParam1 = DATE(liMonth + 1,1,liYear) - 1.   
         ELSE MsRequest.ReqDtParam1 = DATE(liMonth,liFileDate,liYear).
      END.
      
      /* wait for bank file creation */
      fReqStatus(16,"").
   END.

END PROCEDURE.

PROCEDURE pCreateSubRequests:

   DEF VAR ldtAccDate   AS DATE NO-UNDO.
   DEF VAR liTime       AS INT  NO-UNDO.
   DEF VAR liPaymType   AS INT  NO-UNDO.

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   /* always date payments to current day (voucher sequences stay in order) */
   ldtAccDate = TODAY.

   /* check period */
   IF fPeriodLocked(ldtAccDate,FALSE) THEN DO:
      fReqError("Period for " + STRING(ldtAccDate,"99.99.99") + 
                " is locked.").
      RETURN.
   END.

   CASE MsRequest.ReqDParam2:
   /* move to adv.payment balance */
   WHEN 1.0 THEN ASSIGN
      liPaymType   = 4.
   /* refund payment */
   WHEN 2.0 THEN ASSIGN 
      liPaymType   = 6. 
   OTHERWISE 
      liPaymType  = 0.
   END CASE. 
      
   IF liPaymType = 0 THEN DO:
      fReqError("Invalid refund type").
      RETURN.
   END.
    
   /* bank file has been created, payment (sub)request can now be created */
   liReqCnt = 
      fPaymentWithPostingsRequest(MsRequest.CustNum,
                                  liPaymType,
                                  "RFUND",
                                  ldtAccDate,
                                  MsRequest.ReqIParam1,  /* invoice */
                                  MsRequest.ReqDParam1,  /* amount */
                                  "",                 /* postings */
                                  "",                 /* accounts */
                                  "Refund request " +
                                     STRING(MsRequest.MsRequest), /* memo */
                                  ?,                  /* handling time */
                                  INTEGER(MsRequest.ReqDParam2 = 2),
                                      /* control needed for money returns */
                                  0,                  /* interest */
                                  "",                 /* creator */
                                  OUTPUT lcReqChar).

   IF liReqCnt = 0 THEN DO:
      fReqError("Payment request creation failed: " + lcReqChar). 
   END.
   ELSE DO:

      /* link main request to subrequest */
      FIND bSubRequest WHERE bSubRequest.MsRequest = liReqCnt EXCLUSIVE-LOCK.
      ASSIGN 
         bSubRequest.OrigRequest = MsRequest.MsRequest
         bSubRequest.Mandatory   = 1.
      RELEASE bSubRequest.      
     
      fReqStatus(7,"").
   END.

END PROCEDURE.

PROCEDURE pDone:

   /* check that all subrequests have been handled */
   CASE fGetSubRequestState(MsRequest.MsRequest):
    
   WHEN 2 THEN DO:
      /* request handled succesfully */
      fReqStatus(2,""). 
   END.
   
   WHEN 3 OR WHEN 4 THEN DO:
      fReqError("Subrequest has failed").
   END.

   OTHERWISE DO:
      /* back to waiting mode */
      fReqStatus(7,"").
   END.

   END CASE.
   
END PROCEDURE.


