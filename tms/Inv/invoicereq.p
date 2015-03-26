/*-----------------------------------------------------------------------------
  MODULE .......: invoicereq.p
  FUNCTION .....: handle requests for invoices
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 12.04.07
  CHANGED.. ....: 10.09.07/aam ChargeType in ReqIParam2
  Version ......: Yoigo
  -------------------------------------------------------------------------- */

{commali.i}
{msreqfunc.i}
{billrund.i NEW}

DEF INPUT  PARAMETER iiRequest AS INT NO-UNDO.

DEF VAR liValid     AS INT    NO-UNDO.
DEF VAR hInvRun     AS HANDLE NO-UNDO.
DEF VAR ldActStamp  AS DEC    NO-UNDO.
DEF VAR liHandled   AS INT    NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR
   MsRequest.ReqType NE 20
THEN RETURN "ERROR:UNKNOWN REQUEST".

ASSIGN
   lcReqType  = "InvReq"
   ldActStamp = MsRequest.ActStamp.

RUN lamupers PERSISTENT SET hInvRun.


/* exceptionally handle all type 20 requests at the same time, 
   because lamupers and rerate must otherwise be loaded for each 
   request separately (a bunch of requests are created with same
   activation stamp each day) */
FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand     = gcBrand AND
         MsRequest.ReqType   = 20      AND
         MsRequest.ReqStat   = 0       AND
         MsRequest.ActStamp <= ldActStamp:
         
   DO TRANS:
      RUN pODInvoice.
   END.
   
   liHandled = liHandled + 1.
   
   /* don't try to do all at once, give time to other requests also */
   IF liHandled > 200 THEN LEAVE.
END.


/* update invoice nbr sequence */
RUN pUpdInvGroup in hInvRun.

/* clear persistent procedures */
RUN pCleanMem IN hInvRun.

IF VALID-HANDLE(hInvRun) THEN DELETE PROCEDURE hInvRun.
 


/* on demand invoice */
PROCEDURE pODInvoice:

   DEF VAR ldtFromDate AS DATE   NO-UNDO.
   DEF VAR ldtToDate   AS DATE   NO-UNDO.
   DEF VAR liFeePeriod AS INT    NO-UNDO.
   DEF VAR ldtInvDate  AS DATE   NO-UNDO.
   DEF VAR liInvNum    AS INT    NO-UNDO. 
   DEF VAR liInvCust   AS INT    NO-UNDO.
    
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   FIND InvGroup OF Customer NO-LOCK NO-ERROR.
   IF NOT AVAILABLE InvGroup THEN DO:
      fReqError("Invoice group not found").
      RETURN.
   END.
   
   FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN DO:
      FIND FIRST MsOwner USE-INDEX MsSeq WHERE
                 MsOwner.MsSeq   = MsRequest.MsSeq AND
                 MsOwner.InvCust = Customer.CustNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MsOwner THEN DO:           
         fReqError("Subscription not found").
         RETURN.
      END.

      liInvCust = MsOwner.InvCust.
   END.
   ELSE liInvCust = MobSub.InvCust.
   
   IF liInvCust NE CUstomer.CustNum THEN DO:
      fReqError("Conflict in invoice customer data").
      RETURN.
   END.

   EMPTY TEMP-TABLE ttInvCust.
   
   CREATE ttInvCust.
   ASSIGN 
      ttInvCust.CustNr  = Customer.CustNum
      ttInvCust.MinInv  = InvGroup.MinInvAmt
      ttInvCust.CallQty = 0
      ttInvCust.LowVal  = FALSE.
    
   /* invoice is created for all events up till today
      there is a theoretical chance that new tickets are added to this months
      invseq while invoice is being created (after cdrs have been collected
      but invseq has not yet been marked as billed), but it shouldn't be
      too much of a risk  */
   ASSIGN 
      ldtInvDate  = TODAY
      ldtFromDate = DATE(MONTH(TODAY),1,YEAR(TODAY))
      ldtToDate   = IF MONTH(TODAY) = 12 
                    THEN DATE(12,31,YEAR(TODAY))
                    ELSE DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1
      liFeePeriod = YEAR(TODAY) * 100 + MONTH(TODAY).
      
   RUN pCreateODInv in hInvRun(MsRequest.MsSeq,
                               ldtInvDate,   
                               ldtFromDate,
                               ldtToDate,
                               liFeePeriod,
                               0,            /* extratime */
                               TRUE,         /* rerate    */
                               TRUE,         /* double    */
                               OUTPUT liInvNum).

   FIND Invoice WHERE Invoice.InvNum = liInvNum NO-LOCK NO-ERROR.
   
   IF liInvNum = 0 OR NOT AVAILABLE Invoice OR 
      Invoice.CustNum NE MsRequest.CustNum
   THEN DO:
      fReqError("Invoice was not created").
      RETURN.
   END.

   /* mark invoice data to request */
   FIND CURRENT MsRequest EXCLUSIVE-LOCK.
   ASSIGN 
      MsRequest.ReqIParam1 = Invoice.InvNum
      MsRequest.ReqCParam1 = Invoice.ExtInvID
      MsRequest.ReqDParam1 = Invoice.InvAmt.

   /* chargetype changed */
   IF MsRequest.ReqIParam2 > 0 THEN DO:
      FIND CURRENT Invoice EXCLUSIVE-LOCK.
      Invoice.ChargeType = MsRequest.ReqIParam2.
      RELEASE Invoice.
   END.
   ELSE MsRequest.ReqIParam2 = Invoice.ChargeType.
 
   /* fee for creating an invoice */
   IF MsRequest.CreateFees THEN DO:
   
      RUN creasfee (MsRequest.CustNum,
                    0,
                    TODAY,
                    "ODInvoice",
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
 
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.



