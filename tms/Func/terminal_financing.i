/* ----------------------------------------------------------------------
  MODULE .......: terminal_financing.i 
  TASK .........: Terminal financing related functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 10.06.13
  Version ......: Yoigo
----------------------------------------------------------------------- */
&IF "{&TERMINAL_FINANCING_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE TERMINAL_FINANCING_I YES

{commali.i}
{tmsconst.i}
{date.i}
{fcreatereq.i}

FUNCTION fValidateBankFileRequest RETURNS LOG (
   INPUT  icBankCode AS CHAR,
   INPUT  iiReqType  AS INT,
   OUTPUT ocError AS CHAR,
   OUTPUT odaLastDump AS DATE):
   
   DEF VAR litime AS INT NO-UNDO. 

   IF LOOKUP(icBankCode,{&TF_BANK_CODES}) EQ 0 THEN
      RETURN ocError = SUBST("Incorrect bank code: &1", icBankCode).
   
   IF CAN-FIND(
      FIRST MsRequest NO-LOCK WHERE
            MsRequest.Brand = gcBrand AND
            MsRequest.ReqType = iiReqType AND
            MsRequest.ReqCparam1 = icBankCode AND
      LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0) THEN DO:
      ocError = "Ongoing request".
      RETURN FALSE.
   END.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand = gcBrand AND
            MsRequest.ReqType = iiReqType AND
            MsRequest.ReqCparam1 = icBankCode
      BY ActStamp DESC:

      IF MsRequest.ReqStatus EQ {&REQUEST_STATUS_CANCELLED} THEN NEXT. 

      fSplitTS(MsRequest.Actstamp, output odaLastDump, output litime).

      IF odaLastDump >= TODAY THEN DO:
         ocError = "Already done".
         RETURN FALSE.
      END.
      
      LEAVE.
   END.

   /* temporary code, initialize first request with a correct from date */
   IF odaLastDump EQ ? THEN
      odaLastDump = 6/1/2014.

   RETURN TRUE.
   
END FUNCTION. /* FUNCTION fValidateBankFileRequest */

FUNCTION fCreateBankFileRequest RETURNS INTEGER
   (INPUT  icBankCode  AS CHAR,
    INPUT  iccreator   AS CHAR,
    INPUT  icsource    AS CHAR,
    OUTPUT ocresult    AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF VAR ldaLastDump AS DATE NO-UNDO. 

   IF NOT fValidateBankFileRequest(
      icBankCode,
      {&REQTYPE_TERMINAL_FINANCE_BANK_FILE},
      OUTPUT ocresult,
      OUTPUT ldaLastDump) THEN RETURN 0.

   fCreateRequest(({&REQTYPE_TERMINAL_FINANCE_BANK_FILE}),
                  fmakets(),
                  iccreator,
                  FALSE,      /* fees */
                  FALSE).    /* send sms */

   ASSIGN bCreaReq.ReqSource   = icsource
          bCreaReq.ReqDtParam1 = ldaLastDump
          bCreaReq.ReqDtParam2 = TODAY - 1
          bCreaReq.ReqCParam1  = icBankCode
          liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION. /* FUNCTION fSendeInvoiceRequest */
   

FUNCTION fOrderContainsFinancedTerminal RETURNS CHAR
   (INPUT iiOrderId  AS INT,
    INPUT icDCEvent AS CHAR):

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.
      
   FIND Order NO-LOCK WHERE
        Order.Brand  = gcBrand AND
        Order.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN
      RETURN {&TF_STATUS_YOIGO}.

   IF icDCEvent = "RVTERM12" THEN DO:

      IF CAN-FIND(FIRST SingleFee WHERE
                        SingleFee.Brand       = gcBrand AND
                        SingleFee.Custnum     = Order.CustNum AND
                        SingleFee.HostTable   = "MobSub" AND
                        SingleFee.KeyValue    = STRING(Order.MsSeq) AND
                        SingleFee.OrderId     = iiOrderId AND
                        LOOKUP(SingleFee.BillCode,
                               {&TF_BANK_RVTERM_BILLCODES}) > 0) THEN
         RETURN {&TF_STATUS_WAITING_SENDING}.
   END.
   ELSE IF icDCEvent BEGINS "PAYTERM" THEN DO:
   
      FOR FIRST OrderCustomer NO-LOCK WHERE
                OrderCustomer.Brand = gcBrand AND
                OrderCustomer.Order = iiOrderId AND
                OrderCustomer.RowType = 1:
      
         IF NOT OrderCustomer.Profession > "" THEN RETURN  {&TF_STATUS_YOIGO}.
         IF LOOKUP(OrderCustomer.CustIdType,"NIF,NIE") = 0 THEN
            RETURN {&TF_STATUS_YOIGO}.

         IF CAN-FIND(FIRST OfferItem NO-LOCK WHERE
                           OfferItem.Brand       = gcBrand        AND
                           OfferItem.Offer       = Order.Offer    AND
                           OfferItem.ItemType    = "PerContract"  AND
                           OfferItem.ItemKey BEGINS "PAYTERM"     AND
                           OfferItem.EndStamp   >= Order.CrStamp  AND
                           OfferItem.BeginStamp <= Order.CrStamp) THEN DO:
            RETURN (IF INDEX(Order.OrderChannel,"POS") > 0
                    THEN {&TF_STATUS_WAITING_SENDING}
                    ELSE {&TF_STATUS_HOLD_SENDING}).
         END.
      END.
   END.

   RETURN {&TF_STATUS_YOIGO}.
END.

FUNCTION fGetInstallmentOrderId RETURNS INT
   (INPUT iiMsRequest AS INT):

   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER bMsRequest FOR MsRequest.
   DEF BUFFER Order FOR Order.
   DEF BUFFER FixedFee FOR FixedFee.
   
   FIND MsRequest NO-LOCK WHERE
        MsRequest.MsRequest = iiMsRequest NO-ERROR.
   IF NOT AVAIL MsRequest THEN RETURN 0.
   
   IF LOOKUP(msrequest.reqcparam2,"act,recreate") = 0 THEN RETURN 0.

   CASE msrequest.reqsource:

      /* subscription creation */
      WHEN {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} then do:

         FIND FIRST order NO-LOCK where
                    order.msseq = msrequest.msseq and
                    order.ordertype < 2 NO-ERROR.
         IF NOT AVAIL order THEN RETURN 0.
         RETURN order.orderid.
      end.

      /* renewal */
      WHEN {&REQUEST_SOURCE_RENEWAL} then do:

         if msrequest.origrequest = 0 then return 0.

         find first bmsrequest NO-LOCK where
                    bmsrequest.msrequest = msrequest.origrequest and
                    bmsrequest.reqtype = 46 no-error.

         /*   msrequest.reqsource eq "4" or */
         if bmsrequest.reqiparam1 eq 0 then return 0.

         FIND FIRST order NO-LOCK where
                    order.brand = gcBrand and
                    order.orderid = bmsrequest.reqiparam1 NO-ERROR.
         IF AVAIL Order THEN RETURN order.orderid.
         RETURN 0.
      end.

      /* installment contract change */
      WHEN {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE} then do:
           
         IF msrequest.reqiparam3 > 0 THEN
            for first fixedfee NO-LOCK where
                      fixedfee.brand = gcBrand and
                      fixedfee.custnum = msrequest.custnum and
                      fixedfee.hosttable = "MobSub" and
                      fixedfee.keyvalue = string(msrequest.msseq) and
                      fixedfee.sourcetable = "DCCLI" and
                      fixedfee.sourcekey = string(msrequest.reqiparam3):
               return fixedfee.orderid.
            END.

         if msrequest.reqIparam2 > 0 then do:
               
            find bmsrequest NO-LOCK WHERE
                 bmsrequest.msrequest = msrequest.reqiparam2 and
                 bmsrequest.reqtype = 9 and
                 bmsrequest.reqsource = 
                 {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE} no-error.
            IF AVAIL bmsrequest and
                     bmsrequest.reqiparam3 > 0 then   
               for first fixedfee NO-LOCK where
                         fixedfee.brand = gcBrand and
                         fixedfee.custnum = bmsrequest.custnum and
                         fixedfee.hosttable = "MobSub" and
                         fixedfee.keyvalue = string(bmsrequest.msseq) and
                         fixedfee.sourcetable = "DCCLI" and
                         fixedfee.sourcekey = string(bmsrequest.reqiparam3):
               return fixedfee.orderid.
            END.
         END.
         ELSE RETURN 0.
      END.
      /*  not originating from order */
      OTHERWISE RETURN -1. /*  WHEN "4" OR WHEN "5" OR WHEN "6" */ 
      
   END CASE.

END.

/*  This create Request for bank files is related into YDR-2025
   - terminal finance cancellation report AND log file
   - terminal finance termination report AND log file
   these are for SABADELL AND UNOE banks monhtly based periods
*/
FUNCTION fCreateTFBankFileRequest RETURNS INTEGER
   (INPUT  icBankCode  AS CHAR,
    INPUT  iccreator   AS CHAR,
    INPUT  icsource    AS CHAR,
    OUTPUT ocresult    AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF VAR ldaLastDump AS DATE NO-UNDO. 

   IF NOT fValidateBankFileRequest(
      icBankCode,
      {&REQTYPE_TERMINAL_FINANCE_CAN_TER_BANK_FILE},
      OUTPUT ocresult,
      OUTPUT ldaLastDump) THEN RETURN 0.

   fCreateRequest(({&REQTYPE_TERMINAL_FINANCE_CAN_TER_BANK_FILE}),
                  fmakets(),
                  iccreator,
                  FALSE,      /* fees */
                  FALSE).    /* send sms */

   ASSIGN bCreaReq.ReqSource   = icsource
          bCreaReq.ReqCParam1  = icBankCode
          liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION. /* FUNCTION fSendeInvoiceRequest */
&ENDIF

