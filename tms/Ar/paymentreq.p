/* ----------------------------------------------------------------------------
  MODULE .......: paymentreq.p
  FUNCTION .....: payment handling
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 10.09.07
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Func/msreqfunc.i}

{Func/faccper.i}
{Func/finvpayment.i}
{Func/fpaymconfig.i}
{Func/ftaxdata.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}
END.


DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 31 THEN RETURN "ERROR".

CASE MsRequest.ReqStat:
WHEN 0 OR WHEN 15 THEN RUN pPayment.
END CASE.

fCleanEventObjects().

RETURN RETURN-VALUE.


PROCEDURE pPayment:

   DEF VAR ldtAccDate AS DATE NO-UNDO.
   DEF VAR ldActStamp    AS DEC  NO-UNDO. 
   DEF VAR lcError       AS CHAR NO-UNDO. 
   DEF VAR ldBalance     AS DEC  NO-UNDO.
   DEF VAR liReqStat     AS INT  NO-UNDO.
   DEF VAR lcCLI         AS CHAR NO-UNDO.
   DEF VAR liPaymType    AS INT  NO-UNDO.
   DEF VAR liAccount     AS INT  NO-UNDO EXTENT 10.
   DEF VAR ldPosting     AS DEC  NO-UNDO EXTENT 10.
   DEF VAR liVoucher     AS INT  NO-UNDO.
   DEF VAR lcPaymSrc     AS CHAR NO-UNDO.


   liReqStat = MsRequest.ReqStat.
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   /* if so marked then in 1. phase mark request to status 19 for manual
      approval (status 15 means approved) */
   IF liReqStat = 0 AND MsRequest.ReqIParam3 = 1 THEN DO:
      fReqStatus(19,"").
      RETURN.
   END.

   lcCLI = "".
   
   IF MsRequest.ReqIParam1 > 0 THEN DO:
      FIND Invoice WHERE Invoice.InvNum = MsRequest.ReqIParam1 
         NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE Invoice THEN DO:
         fReqError("Invoice not found").
         RETURN.
      END.

      IF Invoice.CustNum NE Customer.CustNum THEN DO:
         fReqError("Conflict in invoice customer").
         RETURN.
      END.
   END.
   
   /* always date payments to current day in order to maintain voucher
      sequence in correct order */
   ldtAccDate = TODAY.

   /* check period */
   IF fPeriodLocked(ldtAccDate,FALSE) THEN DO:
      fReqError("Period for " + STRING(ldtAccDate,"99.99.99") + 
                " is locked.").
      RETURN.
   END.

   /* payment type is mandatory */
   IF NOT CAN-FIND(TMSCodes WHERE   
                   TMSCodes.TableName = "Payment"  AND
                   TMSCodes.FieldName = "PaymType" AND
                   TMSCodes.CodeValue = STRING(MsRequest.ReqIParam2))
   THEN DO:
      fReqError("Invalid payment type").
      RETURN.
   END.

   /* default posting */
   ASSIGN 
      ldPosting[1] = MsRequest.ReqDParam1
      ldPosting[2] = -1 * MsRequest.ReqDParam1.
     
   /* if postings and accounts have been given then use them */
   IF MsRequest.ReqCParam2 > "" THEN 
   DO liReqCnt = 1 TO NUM-ENTRIES(MsRequest.ReqCParam2,":"):
      IF liReqCnt > 10 THEN DO:
         fReqError("More than 10 postings given").
         RETURN.
      END.
       
      /* amounts are without decimals (avoid possible conflict with ./,) */
      ldReqAmt = DECIMAL(ENTRY(liReqCnt,MsRequest.ReqCParam2,":")) / 100
                 NO-ERROR.

      IF ERROR-STATUS:ERROR THEN DO:
         fReqError("Invalid posting amount").
         RETURN.
      END.
      
      IF ldReqAmt NE 0 THEN 
         ldPosting[liReqCnt] = ldReqAmt.
   END.
   
   IF MsRequest.ReqCParam3 > "" THEN 
   DO liReqCnt = 1 TO NUM-ENTRIES(MsRequest.ReqCParam3,":"):
      IF liReqCnt > 10 THEN DO:
         fReqError("More than 10 accounts given").
         RETURN.
      END.
      
      liAccount[liReqCnt] = INTEGER(ENTRY(liReqCnt,MsRequest.ReqCParam3,":")) 
                            NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         fReqError("Invalid account").
         RETURN.
      END.
   END.

   /* payment source may contain also a key to a source table */
   lcPaymSrc = ENTRY(1,MsRequest.ReqCParam1,"/").
   
   /* if accounts have not been given then get default accounts according
      to payment type/source configuration */
   IF liAccount[1] = 0 AND liAccount[2] = 0 THEN DO:
   
      lcReqChar = fRegionTaxZone(IF MsRequest.ReqIParam1 > 0 AND 
                                    Invoice.Region > ""
                                 THEN Invoice.Region   
                                 ELSE Customer.Region).
       
      fGetPaymentAccounts(MsRequest.ReqIParam2,  /* payment type */
                          lcPaymSrc,
                          ldtAccDate,
                          lcReqChar,  /* tax zone */
                          OUTPUT liAccount).
   END.
   
   IF liAccount[2] = 0 AND LOOKUP(STRING(MsRequest.ReqIParam2),"3,4,6") = 0
   THEN DO:
      /* payment is targeted to an invoice */
      IF MsRequest.ReqIParam1 > 0 THEN liAccount[2] = Invoice.ArAccNum.
   END.
   
   /* atleast two accounts are required */
   DO liReqCnt = 1 TO 2:
      FIND Account WHERE 
           Account.Brand  = gcBrand AND
           Account.AccNum = liAccount[liReqCnt] NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Account THEN DO:
         fReqError("Accounts have not been defined").
         RETURN.
      END.
   END.

   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
      lhInvoice = BUFFER Invoice:HANDLE.
      RUN StarEventInitialize(lhInvoice).
   END.

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).
    
   /* payment source */
   IF lcPaymSrc = "" THEN lcPaymSrc = "REQ".
   
   RUN createpaym (Customer.CustNum,
                   MsRequest.ReqIParam1,               /* invoice */
                   lcCLI,
                   ldtAccDate,                         /* posting date */
                   MsRequest.ReqDtParam1,              /* payment date */
                   ldPosting,                          /* amounts */
                   liAccount,                          /* accounts */
                   lcPaymSrc,                          /* source */
                   MsRequest.ReqIParam2,               /* payment type */
                   "R" + STRING(MsRequest.MsRequest),  /* reference */
                   MsRequest.ReqCParam4,               /* memo */
                   OUTPUT liVoucher).

   IF liVoucher = 0 THEN DO:
      fReqError("Payment creation failed: " + RETURN-VALUE). 
   END.

   ELSE DO:

      /* if invoice was paid then update balance, status and others */
      IF MsRequest.ReqIParam1 > 0 THEN DO:
   
         /* ar amount */
         ldReqAmt = 0.
         FIND Payment WHERE Payment.Voucher = liVoucher NO-LOCK.
         DO liReqCnt = 1 TO 10:
            IF Payment.AccType[liReqCnt] = 1 THEN 
               ldReqAmt = ldReqAmt + Payment.Posting[liReqCnt].
         END.
            
         IF ldReqAmt NE 0 THEN DO:
         
            FIND InvGroup where 
                 InvGroup.Brand    = gcBrand AND 
                 InvGroup.InvGroup = Customer.InvGroup NO-LOCK.

            fInvoicePaymentUpdate(BUFFER Invoice,
                                  Payment.PaymDate,
                                  ldReqAmt,
                                  liVoucher,
                                  (MsRequest.ReqIParam4 = 1), /* interest */
                                  InvGroup.UpdCustBal, /* customer balance */
                                  TRUE).               /* payment behaviour */
         END.
      END.

      /* payment source may contain also a key to a source table */
      IF NUM-ENTRIES(MsRequest.ReqCParam1,"/") > 1 THEN DO:
      
         lcReqChar = ENTRY(2,MsRequest.ReqCParam1,"/").
         
         CASE ENTRY(1,lcReqChar,":"):
         
         WHEN "UP" THEN DO:
            FIND UnregPaym WHERE 
                 UnregPaym.UrSeq = INTEGER(ENTRY(2,lcReqChar,":"))
            EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE UnregPaym THEN DO:

                FIND Payment WHERE Payment.Voucher = liVoucher EXCLUSIVE-LOCK.
                ASSIGN 
                  Payment.PaymArc = UnregPaym.Archive
                  Payment.BankAcc = UnregPaym.BankAcc.

                UnregPaym.Booked = UnregPaym.Booked + Payment.PaymAmt.
                
                IF UnregPaym.Booked = UnregPaym.PaidAmt THEN 
                   UnregPaym.State  = 1.

                CREATE UnregLog.
                ASSIGN
                   UnregLog.UrSeq   = UnregPaym.UrSeq
                   UnregLog.AccDate = ldtAccDate  
                   UnregLog.Voucher = Payment.Voucher
                   UnregLog.Amount  = Payment.PaymAmt
                   UnregLog.CustNum = Payment.CustNum.
                CASE Payment.PaymType:
                WHEN 0 THEN DO:
                   IF Payment.InvNum > 0 
                   THEN UnregLog.CustBal = "Inv".
                   ELSE UnregLog.CustBal = "OP".
                END.
                WHEN 2 THEN UnregLog.CustBal = "CL".
                WHEN 4 THEN UnregLog.CustBal = "AP".
                END CASE.
            END.
         END.
         
         END CASE.
         
      END.
         
      /* handled succesfully */
      fReqStatus(2,""). 
   END.

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice). 
   
END PROCEDURE.

