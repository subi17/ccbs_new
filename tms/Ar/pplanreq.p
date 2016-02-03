/*-----------------------------------------------------------------------------
  MODULE .......: pplanreq.p
  FUNCTION .....: handle requests for payment plans
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 16.03.06
  CHANGED.. ....: 10.05.06/aam check decimal handling for batch amounts
  Version ......: M15
  -------------------------------------------------------------------------- */


{Func/msreqfunc.i}
{Func/finvbal.i}
{Func/fpaymplan.i}

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

DEF VAR liValid   AS INT  NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN RETURN "ERROR".

lcReqType = "plreq".

CASE MsRequest.ReqCParam1:
WHEN "duedate"  OR 
WHEN "partpaym" THEN RUN pSingleInvoice.
WHEN "paymplan" THEN RUN pPaymentPlan.
OTHERWISE RETURN "ERROR".
END CASE.

RETURN RETURN-VALUE.


/* change invoice's due date (1) or 
   divide invoice into part payment batches (2) */
PROCEDURE pSingleInvoice:

   DEF VAR ldtFromDate AS DATE NO-UNDO.
   DEF VAR ldtToDate   AS DATE NO-UNDO.
   DEF VAR liPlanID    AS INT  NO-UNDO.
   DEF VAR liAddDays   AS INT  NO-UNDO. 
   DEF VAR lcAmounts   AS CHAR NO-UNDO. 
   DEF VAR liPlanType  AS INT  NO-UNDO.
   DEF VAR liBatchQty  AS INT  NO-UNDO.
    
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   FIND Invoice WHERE Invoice.InvNum = MsRequest.ReqIParam1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Invoice THEN DO:
      fReqError("Invoice not found").
   END.

   /* latest x days from original due date */
   liAddDays = fCParamI("PPDueDateLimit").
   IF liAddDays = ? THEN liAddDays = 0.

   ldtFromDate = DATE(ENTRY(1,MsRequest.ReqCParam2,";")) NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN DO:
      fReqError("Invalid new due date").
      RETURN.
   END.
 
   IF MsRequest.ReqCParam1 = "partpaym" THEN DO:
      ASSIGN ldtToDate  = DATE(ENTRY(3,MsRequest.ReqCParam2,";"))
             lcAmounts  = ENTRY(2,MsRequest.ReqCParam2,";") + ";" +
                          ENTRY(4,MsRequest.ReqCParam2,";")
             liPlanType = 2
             liBatchQty = 2
             NO-ERROR.
   
      /* decimal handling */
      IF SESSION:NUMERIC-FORMAT = "american"
      THEN lcAmounts = REPLACE(lcAmounts,",",".").
      ELSE lcAmounts = REPLACE(lcAmounts,".",",").
      
      IF ERROR-STATUS:ERROR THEN DO:
         fReqError("Invalid new due date").
         RETURN.
      END.
   END.

   ELSE ASSIGN ldtToDate  = ldtFromDate
               lcAmounts  = ""
               liPlanType = 1
               liBatchQty = 1.

   /* check that invoice is valid for payment plan */
   liValid = fValidForPaymPlan(Invoice.InvNum,
                               liPlanType, 
                               liAddDays).

   IF liValid > 0 THEN DO:

      lcReqChar = fValidationError(liValid).

      fReqError(lcReqChar).
      RETURN.
   END. 
 
   /* create plan */
   RUN pplancre (Invoice.CustNum,  /* request may be made by agr.cust */
                 Invoice.InvNum,
                 0,
                 liBatchQty,
                 lcAmounts,
                 ldtFromDate,
                 ldtToDate,
                 liPlanType, 
                 OUTPUT liPlanID,
                 OUTPUT lcReqChar).
                             
   IF lcReqChar > "" OR liPlanID = 0 THEN DO:
      fReqError("Plan creation failed; " + lcReqChar).
      RETURN.
   END. 

   fSplitTS(MsRequest.CreStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).

   /* mark plan as accepted */
   FIND PaymPlan WHERE PaymPlan.PPlanID = liPlanID EXCLUSIVE-LOCK.
   ASSIGN PaymPlan.PPStatus  = 3
          PaymPlan.MsRequest = MsRequest.MsRequest
          PaymPlan.Orderer   = MsRequest.CustNum.
 
   /* print a letter, if request was made after original due date */
   IF ldtActDate > Invoice.DueDate THEN DO:
      RUN prinpplan(liPlanID,
                    MsRequest.CustNum,
                    OUTPUT lcReqChar).

      IF lcReqChar > "" THEN DO:
         fReqLog("Payment plan " + STRING(liPlanID) + 
                 " letter print failed: " + lcReqChar).
         /* mark plan as not accepted */
         PaymPlan.PPStatus = 1.        
      END. 
   
   END.

   /* fee for activating a payment plan */
   IF MsRequest.CreateFees THEN DO:
   
      RUN creasfee (Invoice.CustNum,
                    0,
                    TODAY,
                    "PaymPlan",
                    "Activate",
                    1,
                    ?,
                    "",             /* memo */
                    FALSE,          /* no messages to screen */
                    katun,
                    "",
                    0,
                    "",
                    "",
                    OUTPUT lcReqChar).
   END.
    
   /* mark payment plan id to request */
   FIND CURRENT MsRequest EXCLUSIVE-LOCK.
   MsRequest.ReqIParam2 = liPlanID.
   
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.

/* payment plan, multiple invoices */
PROCEDURE pPaymentPlan:

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   fReqError("Handling not yet implemented").
   RETURN.
   
   /* request handled succesfully */   
/*   fReqStatus(2,""). */
 
END PROCEDURE.


