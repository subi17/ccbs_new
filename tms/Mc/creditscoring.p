DEFINE INPUT PARAMETER  piId     AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER  pcActionType  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER plOk          AS LOGICAL NO-UNDO. 
DEFINE OUTPUT PARAMETER piEmlCount    AS INTEGER NO-UNDO. 
DEFINE OUTPUT PARAMETER pcAnswerCodes AS CHARACTER NO-UNDO. 

/* output through cat. */

{fcgi_agent/xmlrpc/xmlrpc_client.i}
{Syst/commali.i}
{Func/timestamp.i}
{Func/date.i}
{Syst/tmsconst.i}
{Mm/fbundle.i}

/* Connection parameters from TMSParam */
DEFINE VARIABLE cConnURL AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cTimeOut AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iTimeOut  AS INTEGER NO-UNDO. 

/* Character values parsed from the response XML */
DEFINE VARIABLE cTrDsRsgDs   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cNempl       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSmDobERTC11 AS CHARACTER NO-UNDO. 

DEFINE VARIABLE cTrDsRsgDsElName AS CHARACTER INIT "TR-DS-RSG-DS" NO-UNDO. 
DEFINE VARIABLE cNemplElName       AS CHARACTER INIT "IN-10-NEMPL" NO-UNDO. 
DEFINE VARIABLE cSmDobERTC11ElName AS CHARACTER INIT "SM-DOB-ERCT11" NO-UNDO. 

DEF BUFFER bContactPerson FOR CustContact.
DEF BUFFER bDeliveryOrderCustomer FOR OrderCustomer.
DEF BUFFER bAdmOrderCustomer FOR OrderCustomer.

/* XML RPC parameters */
DEFINE VARIABLE pcFoundingDate  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCif        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcAddress    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRepId      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRepName    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRepFSurname AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRepSSurname AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRepTelphone AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRepEmail    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcDelName     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcDelZip      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcDeln        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcDelFSurname AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcDelSSurname AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcDelTelphone AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcDelMail     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRdEntidad   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRdSurcursal AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRdDc        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcNCuenta     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcZIP         AS CHARACTER NO-UNDO. 

DEF VAR lcSalesman            AS CHARACTER NO-UNDO.
DEF VAR lcSimOnly             AS CHARACTER NO-UNDO.
DEF VAR lcOfferId             AS CHARACTER NO-UNDO.
DEF VAR lcSubsType            AS CHARACTER NO-UNDO.
DEF VAR lcKindOfNumber        AS CHARACTER NO-UNDO.
DEF VAR lcOldOperator         AS CHARACTER NO-UNDO.
DEF VAR lcOldPaymType         AS CHARACTER NO-UNDO.
DEF VAR lcMNPMSISDN           AS CHARACTER NO-UNDO.
DEF VAR lcPayTerm             AS CHARACTER NO-UNDO.
DEF VAR lcTerm                AS CHARACTER NO-UNDO.
DEF VAR lcPaymentMethod       AS CHARACTER NO-UNDO.
DEF VAR lcCashResult          AS CHARACTER NO-UNDO.
DEF VAR ldePayInAdv           AS DECIMAL   NO-UNDO.
DEF VAR lcBundleCLITypes      AS CHARACTER NO-UNDO.
DEF VAR ldeResidualAmount     AS DECIMAL   NO-UNDO.
DEF VAR lcUPSCode             AS CHARACTER NO-UNDO.
DEF VAR lcSegment             AS CHARACTER NO-UNDO.
DEFINE VARIABLE piMobileDonorHolder AS INTEGER INITIAL ? NO-UNDO.
DEFINE VARIABLE piFixDonorHolder AS INTEGER INITIAL ? NO-UNDO.

/* Parameter and result struct indentifiers */
DEFINE VARIABLE gcParamStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcResult AS CHARACTER NO-UNDO. 

/* Result XML in the  LONGCHAR value for parsing the XML */
DEFINE VARIABLE lLongCh AS LONGCHAR NO-UNDO.

/* Document and element handles used when parsing the XML as DOM document */
DEFINE VARIABLE hDoc AS HANDLE NO-UNDO. 
DEFINE VARIABLE hRoot AS HANDLE NO-UNDO. 
DEFINE VARIABLE hRootChild AS HANDLE NO-UNDO. 

/* Variables used when calculating and iterating over the children of 
   the root element and recognizing errors during the parsing */
DEFINE VARIABLE iNumChildren AS INTEGER NO-UNDO. 
DEFINE VARIABLE iChild AS INTEGER NO-UNDO. 
DEFINE VARIABLE lError AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cRootChildName AS CHARACTER NO-UNDO. 

DEF VAR lcDataBundle AS CHAR NO-UNDO. 

FUNCTION fGetTMSParam RETURN CHARACTER (INPUT pcParamCode AS CHARACTER):
   FIND TMSParam WHERE Brand = "1" AND ParamGroup = "CreditScoring" AND
        ParamCode = pcParamCode NO-LOCK NO-ERROR.
   IF AVAILABLE TMSParam THEN
      RETURN TMSParam.Charval.
   ELSE 
      RETURN ?.
END.

FUNCTION fGetElementContent RETURN CHARACTER 
   (INPUT pcXMLData AS CHARACTER, INPUT pcElementName AS CHARACTER):
   DEFINE VARIABLE iElementBegin AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iElementEnd   AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iLength       AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cResult       AS CHARACTER NO-UNDO.
   
   cResult = "".
   /* Find element begin */
   iElementBegin = INDEX(pcXMLData, "<" + pcElementName + ">").
   IF iElementBegin = 0 THEN
   DO:
      iElementBegin = INDEX(pcXMLData, "&lt;" + pcElementName + "&gt;").
      IF iElementBegin = 0 THEN cResult = ?.
      iElementBegin = iElementBegin + LENGTH(pcElementName) + 8.
   END.
   ELSE
   DO:
      iElementBegin = iElementBegin + LENGTH(pcElementName) + 2.
   END.
     
   /* Find element end if begin was found */
   IF cResult <> ? THEN
   DO:
      iElementEnd   = INDEX(pcXMLData, "</" + pcElementName + ">").
      IF iElementEnd = 0 THEN
      DO:
         iElementEnd   = INDEX(pcXMLData, "&lt;/" + pcElementName + "&gt;").
         IF iElementEnd = 0 THEN cResult = ?.
      END.
   END.
   
   IF cResult <> ? THEN
      cResult =  SUBSTRING(pcXMLData, iElementBegin, iElementEnd - iElementBegin).
   RETURN cResult.
END.


plOK = TRUE.

cConnURL = fGetTMSParam("URL").
cTimeOut = fGetTMSParam("Timeout").
IF cConnURL = ? OR cConnURL = "" THEN ASSIGN
   plOk = FALSE
   pcAnswerCodes = "URL was not specified in TMSParam".
ELSE IF cTimeOut = ? OR cTimeOut = "" THEN ASSIGN
   plOK = FALSE
   pcAnswerCodes = "Timeout was not specified in TMSParam".
ELSE DO:
   iTimeOut = INTEGER(cTimeout) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
   DO:
      plOK = FALSE.
      pcAnswerCodes = "Timeout in TMSParam was not numeric".
   END.
END.

IF NOT plOK THEN RETURN.

/* Find parameter data from TMS tables */

IF pcActionType EQ "ORDER" THEN DO:

   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

   FIND FIRST Order WHERE
              Order.Brand = "1" AND
              Order.OrderId = piId NO-LOCK NO-ERROR.
   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = "1" AND
              OrderCustomer.OrderId = piId AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-LOCK NO-ERROR.
   IF AVAILABLE OrderCustomer AND AVAILABLE Order THEN
   DO:
      pcFoundingDate = fDateFmt(OrderCustomer.FoundationDate, "ddmmyyyy"). /* 3 */
      
      FIND FIRST OrderAccessory OF Order WHERE
                 OrderAccessory.TerminalType EQ ({&TERMINAL_TYPE_PHONE}) NO-LOCK NO-ERROR.

      ASSIGN pcCIF          = OrderCustomer.CustId                      /*  1 */
             pcZIP          = OrderCustomer.ZIP                         /*  2 */
             pcAddress       = OrderCustomer.Address                    /*  4 */
             pcRepId         = OrderCustomer.AuthCustId                 /*  5 */
             pcRepName       = OrderCustomer.FirstName                  /*  6 */
             pcRepFSurname   = OrderCustomer.SurName1                   /*  7 */
             pcRepSSurname   = OrderCustomer.SurName2                   /*  8 */
             pcRepTelphone   = OrderCustomer.MobileNumber              /*  9 */
             pcRepEmail      = OrderCustomer.Email                      /* 10 */ 
             lcSalesman    = Order.Salesman
             lcSimOnly     = STRING(AVAIL(OrderAccessory),"N/Y")
             lcOfferId     = Order.Offer
             lcSubsType    = Order.CLIType.
             lcKindOfNumber = "N".

      /* YPRO-25 Segment field WITH Customer Category */
      FIND FIRST CustCat NO-LOCK WHERE
                 CustCat.Brand    = gcBrand AND
                 CustCat.Category = OrderCustomer.Category NO-ERROR.
      IF AVAILABLE CustCat THEN
        lcSegment = CustCat.Segment.

      IF LOOKUP(Order.CLIType,lcBundleCLITypes) > 0 THEN DO:
         lcDataBundle = fGetDataBundleInOrderAction(Order.OrderID,
                                                    Order.CLIType).
         IF lcDataBundle > "" THEN lcSubsType = lcDataBundle.
      END.

      IF Order.MNPStatus > 0 THEN ASSIGN
         lcKindOfNumber = "P"
         lcOldOperator = Order.CurrOper
         lcMNPMSISDN   = Order.CLI
         lcOldPaymType = STRING(Order.OldPayType,"P/C").

      /* Bank related */
      ASSIGN
            pcRdEntidad     = SUBSTRING(OrderCustomer.BankCode, 5, 4)   /* 18 */
            pcRdSurcursal   = SUBSTRING(OrderCustomer.BankCode, 9, 4)   /* 19 */
            pcRdDc          = SUBSTRING(OrderCustomer.BankCode, 13, 2)   /* 20 */
            pcNCuenta      = SUBSTRING(OrderCustomer.BankCode, 15, 10). /* 21 */  

   END.

   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = "1" AND
              OrderCustomer.OrderId = piId AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY} NO-LOCK NO-ERROR.
   IF AVAILABLE OrderCustomer THEN
   DO:
      ASSIGN pcDelName       = OrderCustomer.Address                   /* 11 */ 
             pcDelZip        = OrderCustomer.ZIP                       /* 12 */
             lcUPSCode       = IF Order.DeliveryType = {&ORDER_DELTYPE_POS} AND
                                  OrderCustomer.KialaCode > ""
                               THEN "YOI_" + OrderCustomer.KialaCode
                               ELSE OrderCustomer.KialaCode.
   END. 

   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = "1" AND
              OrderCustomer.OrderId = piId AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT} NO-LOCK NO-ERROR.
   IF AVAILABLE OrderCustomer THEN
   DO:
      ASSIGN pcDeln          = OrderCustomer.FirstName                 /* 13 */
             pcDelFSurname   = OrderCustomer.SurName1                  /* 14 */
             pcDelSSurname   = OrderCustomer.SurName2                  /* 15 */
             pcDelTelphone   = OrderCustomer.MobileNumber              /* 16 */
             pcDelMail       = OrderCustomer.Email                     /* 17 */
             pcRepId         = OrderCustomer.AuthCustId /* is overwritten here if needed */
                                  WHEN OrderCustomer.AuthCustId > "".  /* 5  */
   END.

   ASSIGN
      piMobileDonorHolder =
         INTEGER(CAN-FIND(FIRST OrderCustomer NO-LOCK WHERE
                    OrderCustomer.Brand = "1" AND
                    OrderCustomer.OrderId = piId AND
                    OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}))
      piFixDonorHolder =
         INTEGER(CAN-FIND(FIRST OrderCustomer NO-LOCK WHERE
                    OrderCustomer.Brand = "1" AND
                    OrderCustomer.OrderId = piId AND
                    OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER})).
END.
/* YDR-323 STC */
ELSE IF LOOKUP(pcActionType, "NORMAL,RENEWAL_STC") > 0 THEN DO:
   
   IF pcActionType EQ "RENEWAL_STC" THEN DO:
      
      FIND FIRST Order WHERE 
                 Order.Brand = "1" AND
                 Order.OrderID = piId NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Order THEN DO:
         plOK = FALSE.
         pcAnswerCodes = "Order data not available".
         RETURN.
      END.

      FIND FIRST Customer WHERE 
                 Customer.Custnum = Order.Custnum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Customer THEN DO:
         plOK = FALSE.
         pcAnswerCodes = "Order customer data not available".
         RETURN.
      END.
      
      FIND FIRST bDeliveryOrderCustomer OF Order WHERE
                 bDeliveryOrderCustomer.RowType = 4 NO-LOCK NO-ERROR.

      FIND FIRST MsRequest WHERE
                 MsRequest.Msseq = Order.MsSeq AND
                 MsRequest.ReqType = 0 AND
                 MsRequest.ReqIParam2 = Order.OrderID 
      NO-LOCK NO-ERROR.
      IF NOT AVAIL MsRequest THEN DO:
         plOK = FALSE.
         pcAnswerCodes = "STC request not found".
         RETURN.
      END.
   END.
   ELSE DO:
      FIND FIRST Mobsub WHERE 
                 Mobsub.MsSeq = piId NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Mobsub THEN DO:
         plOK = FALSE.
         pcAnswerCodes = "Subscription data not available".
         RETURN.
      END.
      
      FIND FIRST Customer WHERE 
                 Customer.Custnum = Mobsub.Custnum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Customer THEN DO:
         plOK = FALSE.
         pcAnswerCodes = "Customer data not available".
         RETURN.
      END.
      
   END.
     
   ASSIGN pcCIF          = Customer.OrgId                    /*  1 */
          pcZIP          = Customer.ZIP                      /*  2 */
          pcFoundingDate = fDateFmt(Customer.FoundationDate, 
                                    "ddmmyyyy")              /* 3 */
          pcAddress       = Customer.Address                 /* 4 */
          pcRepId         = Customer.OrgId                /* 5 */
          pcRepName       = Customer.FirstName            /* 6 */
          pcRepFSurname   = Customer.CustName             /* 7 */
          pcRepSSurname   = Customer.SurName2             /* 8 */
          pcRepTelphone   = Customer.SMSNumber            /* 9 */
          pcRepEmail      = Customer.Email                /* 10 */ 
          pcDelZip        = (IF pcActionType EQ "RENEWAL_STC" AND
                             AVAIL bDeliveryOrderCustomer
                             THEN bDeliveryOrderCustomer.ZIP
                             ELSE Customer.ZIP) /* 12 */
          pcRdEntidad     = SUBSTRING(Customer.Bankacct, 5, 4)   /* 18 */
          pcRdSurcursal   = SUBSTRING(Customer.Bankacct, 9, 4)   /* 19 */
          pcRdDc          = SUBSTRING(Customer.Bankacct, 13, 2)   /* 20 */
          pcNCuenta       = SUBSTRING(Customer.Bankacct, 15, 10) /* 21 */  
          lcKindOfNumber  = "P"
          lcOldOperator   = "Yoigo" 
          lcOldPaymType   = "P"
          lcSalesman      = (IF pcActionType EQ "RENEWAL_STC"
                             THEN "Y300000000" ELSE "Y100000000")
          lcSimOnly       = (IF pcActionType EQ "RENEWAL_STC"
                             THEN "N" ELSE "Y")
          lcOfferId       = (IF pcActionType EQ "RENEWAL_STC"
                             THEN Order.Offer ELSE "")
          lcSubsType      = (IF pcActionType EQ "RENEWAL_STC" THEN
                                MsRequest.ReqCParam2
                             ELSE Mobsub.CliType).

   /* YPRO-25 Segment field WITH Customer Category */
   FIND FIRST CustCat NO-LOCK WHERE
              CustCat.Brand    = gcBrand AND
              CustCat.Category = Customer.Category NO-ERROR.
   IF AVAILABLE CustCat THEN
     lcSegment = CustCat.Segment.

   FIND FIRST bContactPerson WHERE
              bContactPerson.Brand = "1" AND
              bContactPerson.Custnum = Customer.Custnum AND
              bContactPerson.CustType EQ 5 NO-LOCK NO-ERROR.
   IF AVAIL bContactPerson THEN
      ASSIGN pcDelname       = bContactPerson.FirstName /* 13 */
             pcDelFSurname   = bContactPerson.Custname  /* 14 */
             pcDelSSurname   = bContactPerson.Surname2  /* 15 */
             pcDelTelPhone   = bContactPerson.SMSNumber /* 16 */
             pcDelMail       = bContactPerson.Email.    /* 17 */

END.
ELSE DO:
   plOK = FALSE.
   pcAnswerCodes = SUBST("Unknown credit scoring action: &1", pcActionType). 
   RETURN.
END.

IF LOOKUP(pcActionType,"ORDER,RENEWAL_STC") > 0 THEN DO:

   FIND FIRST Order WHERE
              Order.Brand = "1" AND
              Order.OrderID = piId NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Order THEN DO:
      plOK = FALSE.
      pcAnswerCodes = "Order data not available".
      RETURN.
   END.

   /* Find TERM and PayTerm contracts from offer */
   IF Order.Offer > "" THEN
      FOR FIRST Offer WHERE
                Offer.Brand = gcBrand AND
                Offer.Offer = Order.Offer NO-LOCK,
          EACH  OfferItem WHERE
                OfferItem.Brand       = gcBrand        AND
                OfferItem.Offer       = Offer.Offer    AND
                OfferItem.ItemType    = "PerContract"  AND
               (OfferItem.ItemKey BEGINS "PAYTERM" OR
                OfferItem.ItemKey BEGINS "TERM")       AND
                OfferItem.EndStamp   >= Order.CrStamp  AND
                OfferItem.BeginStamp <= Order.CrStamp NO-LOCK:
         IF OfferItem.ItemKey BEGINS "PAYTERM" THEN
            ASSIGN lcPayTerm = OfferItem.ItemKey
                   ldeResidualAmount = OfferItem.Amount.
         ELSE IF OfferItem.ItemKey BEGINS "TERM" THEN
            lcTerm = OfferItem.ItemKey.
      END. /* FOR FIRST Offer WHERE */

   /* Check Payment Method for direct channel */
   IF INDEX(Order.OrderChannel,"POS") = 0 THEN DO:
      FIND FIRST Orderpayment WHERE
                 OrderPayment.Brand   = gcBrand AND
                 OrderPayment.OrderId = Order.OrderId NO-LOCK NO-ERROR.
      IF AVAIL OrderPayment THEN DO:
         IF OrderPayment.Method = {&ORDERPAYMENT_M_POD} THEN 
            lcPaymentMethod = "on_delivery".
         ELSE IF OrderPayment.Method = {&ORDERPAYMENT_M_PAYPAL} THEN
            lcPaymentMethod = "paypal".
         ELSE lcPaymentMethod = "credit_card".
      END. /* IF AVAIL OrderPayment THEN DO: */

      /* Amount paid in direct channels */
      RUN Mc/cashfee.p(Order.OrderID,
                    2, /* action 2=just make a list of fees, don't create */
                    OUTPUT lcCashResult,
                    OUTPUT ldePayInAdv,
                    OUTPUT lcCashResult).
      ldePayInAdv = ROUND(ldePayInAdv,2).
   END. /* IF INDEX(Order.OrderChannel,"POS") = 0 THEN DO: */

END. /* IF LOOKUP(pcActionType,"ORDER,RENEWAL_STC") > 0 THEN DO: */

/* Do the call */
IF plOk THEN initialize(cConnURL, iTimeOut).

EMPTY TEMP-TABLE tt_param.


IF plOK THEN
DO:
   lcOldOperator =  SUBSTRING(lcOldOperator,1,16).

   /* Construct the parameter struct from the parameter values */
   gcParamStruct = add_struct("", ""). 
   add_string  (gcParamStruct, "cif"         , pcCif  ).
   add_string  (gcParamStruct, "zip"         , pcZip  ). 
   add_string  (gcParamStruct, "founding_date", pcFoundingDate ).  
   add_string  (gcParamStruct, "address"     , pcAddress  ).
   add_string  (gcParamStruct, "rep_id"      , pcRepId  ).
   add_string  (gcParamStruct, "rep_name"    , pcRepName  ).
   add_string  (gcParamStruct, "rep_fsurname", pcRepFSurname  ).
   add_string  (gcParamStruct, "rep_ssurname", pcRepSSurname   ).
   add_string  (gcParamStruct, "rep_email"   , pcRepEmail   ).
   add_string  (gcParamStruct, "del_zip"     , pcDelZip     ).
   add_string  (gcParamStruct, "del_n"       , pcDeln   ).
   add_string  (gcParamStruct, "del_name"    , pcDelName    ).
   add_string  (gcParamStruct, "del_fsurname", pcDelFSurname  ).
   add_string  (gcParamStruct, "del_ssurname", pcDelSSurname  ).
   add_string  (gcParamStruct, "del_telphone", pcDelTelphone  ).
   add_string  (gcParamStruct, "del_mail"    , pcDelMail  ).
   add_string  (gcParamStruct, "rd_entidad"  , pcRdEntidad   ).
   add_string  (gcParamStruct, "rd_sucursal" , pcRdSurcursal  ).
   add_string  (gcParamStruct, "rd_dc"       , pcRdDc  ).
   add_string  (gcParamStruct, "rd_ncuenta"  , pcNCuenta ).
   add_string  (gcParamStruct, "order_salesman_id", lcSalesMan).
   add_string  (gcParamStruct, "order_simonly_flag", lcSimOnly).
   add_string  (gcParamStruct, "order_offerid", lcOfferId).
   add_string  (gcParamStruct, "order_subscriptiontype", lcSubsType).
   add_string  (gcParamStruct, "order_kind_of_number", lcKindOfNumber).
   add_string  (gcParamStruct, "mnp_previous_operator", lcOldOperator).
   add_string  (gcParamStruct, "mnp_previous_payment_type", lcOldPaymType).
   add_string  (gcParamStruct, "mnp_msisdn", lcMNPMSISDN).
   add_string  (gcParamStruct, "term", lcTerm).
   add_string  (gcParamStruct, "payterm", lcPayTerm).
   add_double  (gcParamStruct, "payinadv", ldePayInAdv).
   add_string  (gcParamStruct, "payment_method", lcPaymentMethod).
   add_double  (gcParamStruct, "buyback_price", ldeResidualAmount).
   add_string  (gcParamStruct, "kiala_code", lcUPSCode).
   add_string  (gcParamStruct, "segment", lcSegment).

   IF piMobileDonorHolder NE ?
   THEN add_int  (gcParamStruct, "MobileDonorholder", piMobileDonorHolder).

   IF piFixDonorHolder NE ?
   THEN add_int  (gcParamStruct, "FixDonorholder", piFixDonorHolder).

   DEFINE VARIABLE lcResult AS LONGCHAR.

   /* RUN the XML RPC method */


   run_rpc_method_without_parse("CreditScoringPymes.do_scoring", 
                               TRUE, OUTPUT lcResult).

   gcResult = lcResult.


   IF gi_xmlrpc_error eq {&TRANSPORT_ERROR} THEN
   DO:
      plOk = FALSE.
      pcAnswerCodes = "Connection timeout occured with URL " + cConnURL + 
         " and timeout " + STRING(iTimeOut) + " seconds, error message : " +
         gc_xmlrpc_error.
   END.

   IF plOk THEN
   DO:
      /* Get the XML RPC result XML and complete it to be well formed XML by adding
         the root the element <root> */

      IF gcResult = ? OR TRIM(gcResult) = "?" OR TRIM(gcResult) = "" THEN
      DO:
         plOK = FALSE.
         pcAnswerCodes = "Invalid response " + gcResult.
      END.
      ELSE
      DO:

         cTrDsRsgDs = fGetElementContent(gcResult, cTrDsRsgDsElName).
         cNempl = fGetElementContent(gcResult, cNemplElName).
         cSmDobERTC11 = fGetElementContent(gcResult, cSmDobERTC11ElName).
         IF (cTrDsRsgDs eq ?) OR (cNempl eq ?) OR (cSmDobERTC11 eq ?) THEN
         DO:
            
            plOK = FALSE.
            DEFINE VARIABLE lcElements AS CHARACTER NO-UNDO. 
            lcElements = "".
            IF cTrDsRsgDs eq ? THEN lcElements = cTrDsRsgDsElName.
            IF cNempl eq ? THEN
            DO:
               IF lcElements eq "" THEN
                  lcElements = cNemplElName.
               ELSE
                  lcElements = lcElements + "," + cNemplElName.
            END. /* IF cNempl = ? */
            IF pcAnswerCodes eq ? THEN
            DO:
               IF lcElements eq "" THEN
                  lcElements = cSmDobERTC11ElName.
               ELSE
                  lcElements = lcElements + "," + cSmDobERTC11ElName.
            END. /* IF pcAnswerCodes = ? */

            pcAnswerCodes = "Invalid response: the response elements " + lcElements + 
                            " could not found in response : " + gcResult.
           
         END. /* IF cTrg... eq ? ... */
      END. /* IF gcResult = ? ... */
     
   END. /* IF plOk */
END.

IF plOk THEN
DO:
   piEmlCount = INTEGER(cNempl) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
   DO:
      plOK = FALSE.
      pcAnswerCodes = "Invalid response: Employment count was numeric".
   END.
   IF cTrDsRsgDs = "A" THEN
   DO:
      plOK = TRUE.
   END.
   ELSE 
   DO:
      plOK = FALSE.
      if cTrDsRsgDs <> "P" THEN
      DO:
         pcAnswerCodes = "Invalid response: the response error code was not P".
      END.
   END.

   IF pcAnswerCodes = "" THEN
   DO:
      pcAnswerCodes = cSmDobERTC11.
   END.
   ELSE
   DO:
      pcAnswerCodes = pcAnswerCodes + ", XML RPC answer codes: " + cSmDobERTC11.
   END.
END.

FINALLY:
   xmlrpc_finalize().
END.
