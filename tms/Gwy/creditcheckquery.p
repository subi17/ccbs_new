{Syst/commali.i} 
{Func/xmlfunction.i}
{Func/mathfunction.i}
{Func/msreqfunc.i}
{Syst/tmsconst.i}
{Mm/msagrcustchg.i}

DEFINE INPUT PARAMETER iiRequest AS INT             NO-UNDO.

DEF VAR lcType AS CHAR NO-UNDO.

DEFINE VARIABLE lcTCPModule  AS CHARACTER NO-UNDO INITIAL "Gwy/tcpgwy.p" . 
   
DEF BUFFER bOrigRequest FOR MsRequest.

FIND FIRST TMSParam where
           TMSParam.Brand      = Syst.Var:gcBrand AND 
           TMSParam.ParamCode  =  "TCPModule" NO-LOCK NO-ERROR.
IF AVAIL TMSParam THEN 
         lcTCPModule = TMSParam.CharVal.
   
FUNCTION fGetResponse RETURNS CHARACTER
  (INPUT pcXMLMsg AS CHARACTER,
   INPUT pcNode   AS CHARACTER):

   DEFINE VARIABLE liIdx1   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liIdx2   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO INIT "N/A".
   
   liIdx1 = INDEX(pcXMLMsg,pcNode).

   IF liIdx1 NE 0 THEN DO:
      lcReturn =  substring(pcxmlmsg,liidx1 +  length(pcnode),1).
   END.
   ELSE lcReturn = "Error: " + pcXMLMsg.

   RETURN lcReturn.

END FUNCTION.

DEFINE TEMP-TABLE ttCreditCheck NO-UNDO
   FIELD ttName   AS CHARACTER
   FIELD ttFormat AS CHARACTER
   FIELD ttValue  AS CHARACTER.

DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResp     AS CHARACTER NO-UNDO.
DEF VAR liEmployees AS INT NO-UNDO. 
DEF VAR lcAnswerCodes AS CHAR NO-UNDO. 
DEF VAR llOK AS LOG NO-UNDO. 
      
FIND FIRST MSrequest WHERE 
           MSRequest.Brand     = Syst.Var:gcBrand    AND
           MSRequest.MSRequest = iiRequest NO-LOCK NO-ERROR.
   
IF MsRequest.OrigRequest > 0 THEN DO:            

   FIND FIRST bOrigRequest WHERE 
              bOrigRequest.MsRequest = MsRequest.OrigRequest NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bOrigRequest THEN DO:
      fReqError("Original request missing").
      RETURN.
   END.
END.
      
FIND FIRST Customer WHERE 
           Customer.CustNum = MSRequest.CustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   fReqError("Customer not found").
   RETURN.
END.

/* Use Pymes credit scoring for company customer, YDR-323 */
IF Customer.CustIdType = "CIF" AND 
   (NOT AVAIL bOrigRequest OR
              bOrigRequest.ReqType NE 10) THEN DO:

   FIND FIRST MobSub WHERE 
              MobSub.MsSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN DO:
      fReqError("Subscription not found").
      RETURN.
   END.
   
   RUN Mc/creditscoring.p(
      MobSub.MsSeq,
      "NORMAL",
      OUTPUT llOk,
      OUTPUT liEmployees,
      OUTPUT lcAnswerCodes).

   IF llOk THEN fReqStatus(2,lcAnswerCodes).
   ELSE fReqError(lcAnswerCodes).
   
   RETURN.
END.
ELSE DO:
   
   RUN pCreditCheckRequest(iiREquest).

   lcResponse = RETURN-VALUE.

   lcresp = fGetResponse(lcResponse,"<methodResponse><params><param><value>")
            NO-ERROR.

   IF lcResp = "A" THEN fReqStatus(2,lcResp).
   ELSE DO:
      fReqError(lcResp).

      IF lcType = "ACC" THEN
         RUN Mm/acc_sendsms.p(MsRequest.OrigRequest,
                         MsRequest.CustNum,
                         "Rejected",
                         "HT:309").
   END.                      
END.

PROCEDURE pCreditCheckRequest:

   DEFINE INPUT PARAMETER piRequest AS INT  NO-UNDO.

   DEFINE VARIABLE lcMethod AS CHARACTER    NO-UNDO.
   DEFINE VARIABLE lcReturn AS CHARACTER    No-UNDO.

   lcMethod = "CreditScoring.do_scoring".
   
   RUN pHeader(pirequest).

   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
 
   /* prodedure to run is in response */
   IF INDEX(THIS-PROCEDURE:INTERNAL-ENTRIES,"pHeader") > 0 THEN DO:

      RUN pCreditScoring(piRequest,lcmethod).
      lcReturn =  RETURN-VALUE.
   END.
   ELSE
      lcReturn = lcMethod.

   RETURN lcReturn.

END PROCEDURE.

PROCEDURE pCreditScoring:

   DEFINE INPUT PARAMETER piMSRequest AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcMethod    AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lhSAXWriter  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE llOK         AS LOGICAL   NO-UNDO. 
   DEFINE VARIABLE lmXML        AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE lcRoot       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcStruct     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcURL        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lhField      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhTable      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lcTable      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcFormat     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcXML        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcHTTPHeader AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcReturn     AS CHARACTER NO-UNDO.
   DEF VAR lcAddress AS CHAR NO-UNDO. 

   ASSIGN
      lcRoot   = "methodCall"
      lcStruct = "params,param,value,struct" 
      lcURL = fCParam("URL","urlCreditCheck") 
      lcAddress = fCParam("URL","urlCreditCheckAddress").
     /* lcURL    = "-H 217.168.2.194 -S 8080"  . */

   CREATE SAX-WRITER lhSAXWriter. 
   
   lhSAXWriter:FORMATTED = FALSE. 
   
   llOK = lhSAXWriter:SET-OUTPUT-DESTINATION("memptr",lmXML).
   
   llOK = lhSAXWriter:START-DOCUMENT().
   llOK = lhSAXWriter:START-ELEMENT(lcRoot).

   llOK = lhSAXWriter:WRITE-DATA-ELEMENT("methodName",pcMethod).

   fRPCStruct("Start",lcStruct,lhSAXWriter).
   
   FOR EACH ttCreditCheck NO-LOCK: 

      ASSIGN
         lhTable = BUFFER ttCreditCheck:HANDLE
         lcTable = lhTable:NAME.

      llOK = lhSAXWriter:START-ELEMENT("member").
                                          
      lhField = lhTable:BUFFER-FIELD(1).  
      llOK = 
         lhSAXWriter:WRITE-DATA-ELEMENT("name",STRING(lhField:BUFFER-VALUE)).
      llOK = lhSAXWriter:START-ELEMENT("value").

      lhField = lhTable:BUFFER-FIELD(2).
      lcFormat = lhField:BUFFER-VALUE.

      lhField = lhTable:BUFFER-FIELD(3).
      llOK = 
        lhSAXWriter:WRITE-DATA-ELEMENT(lcFormat,STRING(lhField:BUFFER-VALUE)).

      llOK = lhSAXWriter:END-ELEMENT("value"). 

      llOK = lhSAXWriter:END-ELEMENT("member").
                
   END.
                
   fRPCStruct("End",lcStruct,lhSAXWriter).
     
   llOK = lhSAXWriter:END-ELEMENT(lcRoot).

   llOK = lhSAXWriter:END-DOCUMENT().

   DELETE OBJECT lhSAXWriter.

   EMPTY TEMP-TABLE ttCreditCheck.
   lcXML = GET-STRING(lmXML,1).

   lcHTTPHeader = SUBST("POST &1 HTTP/1.1~n"                +
                  "User-Agent: Frontier/5.1.2 (WinNT)~n" +
                  "Host: 145.247.16.66~n"                +
                  "Content-Type: text/xml~n"             +
                  "Content-Length: " + STRING(LENGTH(lcXML)) + "~n~n",
                  lcAddress). 

   SET-SIZE(lmXML) = 0.

   RUN VALUE(lcTCPModule) (lcHTTPHeader + lcXML,lcURL,5,2,"<").  
   /* RUN Gwy/tcpgwy.p(lcHTTPHeader + lcXML,lcURL,5,2,"<").   */

   lcReturn = RETURN-VALUE.
   
   RETURN lcReturn.

END PROCEDURE.

PROCEDURE pHeader:

   DEFINE INPUT PARAMETER iiMSRequest AS INT NO-UNDO.

   DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.
   
   DEF VAR lcOrgID       AS CHAR NO-UNDO.
   DEF VAR lcFirstName   AS CHAR NO-UNDO.
   DEF VAR lcSurName1    AS CHAR NO-UNDO.
   DEF VAR lcSurName2    AS CHAR NO-UNDO.
   DEF VAR lcBirthDay    AS CHAR NO-UNDO.
   DEF VAR lcZipCode     AS CHAR NO-UNDO.
   DEF VAR lcAddress     AS CHAR NO-UNDO.
   DEF VAR lcBankAcc     AS CHAR NO-UNDO.
   DEF VAR lcNationality AS CHAR NO-UNDO.
   DEF VAR lcNumberType  AS CHAR NO-UNDO.
   DEF VAR lcRegion      AS CHAR NO-UNDO.
   DEF VAR lcPostOffice  AS CHAR NO-UNDO.
   DEF VAR lcCustIDType  AS CHAR NO-UNDO.
   DEF VAR lcEMail       AS CHAR NO-UNDO.
   DEF VAR lcPhone1      AS CHAR NO-UNDO.
   DEF VAR lcPhone2      AS CHAR NO-UNDO.
   DEF VAR lcSalesman    AS CHAR NO-UNDO.
   DEF VAR lcSimOnly     AS CHAR NO-UNDO.
   DEF VAR lcOfferId     AS CHAR NO-UNDO. 
   DEF VAR lcSubsType    AS CHAR NO-UNDO.
   DEF VAR lcOldOperator AS CHAR NO-UNDO. 
   DEF VAR lcOldPaymType AS CHAR NO-UNDO. 
   DEF VAR lcIMEI        AS CHAR NO-UNDO.
   DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.

   DEF BUFFER bACCOrder FOR Order.

   FIND FIRST MSrequest WHERE 
              MSRequest.Brand     = Syst.Var:gcBrand    AND
              MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.
   
   FIND MobSub WHERE
        MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN "ERROR:Subscription missing".

   ASSIGN lcSubsType = MobSub.CliType
          lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").
   
   IF MsRequest.OrigRequest > 0 THEN DO:            
   
      FIND FIRST bOrigRequest WHERE 
         bOrigRequest.MsRequest = MsRequest.OrigRequest NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bOrigRequest THEN 
         RETURN "ERROR:Original request missing".
         
      IF bOrigRequest.ReqType = 10 THEN lcType = "ACC".
      ELSE IF bOrigRequest.ReqType = 0 THEN DO:
         IF bOrigRequest.ReqIParam2 > 0 THEN DO:
            lcType = "STC_RENOVE".

            FIND Order WHERE
                 Order.Brand = Syst.Var:gcBrand AND
                 Order.OrderID = bOrigRequest.ReqIParam2 NO-LOCK NO-ERROR.
            IF NOT AVAIL Order THEN RETURN "ERROR:Renove order missing".
            
            FIND OrderAccessory WHERE
                 OrderAccessory.Brand = Syst.Var:gcBrand AND
                 OrderAccessory.OrderID = Order.OrderID AND
                 OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-LOCK NO-ERROR.

            IF NOT AVAIL OrderAccessory THEN RETURN "ERROR:Renove terminal missing".
            lcIMEI = OrderAccessory.IMEI.
         
         END.
         ELSE lcType = "STC".

         lcSubsType = bOrigRequest.ReqCParam2.

      END.
      ELSE RETURN "ERROR:Unsupported original request type " + STRING(bOrigRequest.Reqtype).
   END.

   /* Common for all types */
   ASSIGN
      lcOldOperator = "Yoigo".
      
   EMPTY TEMP-TABLE ttCustomer.
      
   IF lcType EQ "ACC" THEN DO:
   
      IF bOrigRequest.ReqIParam4 > 0 THEN DO:

         FIND bAccOrder NO-LOCK WHERE
              bAccOrder.brand = Syst.Var:gcBrand AND
              bAccOrder.OrderID = bOrigRequest.ReqIParam4 NO-ERROR. 

         IF NOT AVAIL bAccOrder THEN RETURN "ERROR: Order not found".

         IF NOT fParseAccOrderCustomer(
            bAccOrder.OrderID,
            OUTPUT TABLE ttCustomer BY-REFERENCE) THEN
            RETURN "ERROR: New customer data parsing failed".
         
      END.
      ELSE DO:

         IF NOT fParseAccDataParam(
           bOrigRequest.ReqCParam1,
           OUTPUT TABLE ttCustomer BY-REFERENCE) THEN
           RETURN "ERROR: New customer data parsing failed".
      END.
      
      ASSIGN 
         lcCustIdType  = ttCustomer.CustIDType
         lcOrgId       = ttCustomer.OrgID
         lcAddress     = ttCustomer.Address
         lcZipCode     = ttCustomer.ZipCode
         lcRegion      = ttCustomer.Region
         lcPostOffice  = ttCustomer.PostOffice
         lcNationality = "ES"
         lcBankAcc     = ttCustomer.BankAcct
         lcNumberType  = "N"
         lcEMail       = ttCustomer.Email
         lcPhone1      = ""
         lcPhone2      = ""
         lcSalesman    = "Y200000000"
         lcSimOnly     = "Y"
         lcOldPaymType = STRING(Mobsub.PayType,"P/C").
         lcSubsType    = Mobsub.CLIType.

      IF LOOKUP(Mobsub.CLIType,lcBundleCLITypes) > 0 THEN
         lcSubsType = MobSub.TariffBundle.
          
      IF lcCustIDType = "CIF" THEN ASSIGN 
         lcFirstName   = ""
         lcSurName1    = ttCustomer.CustName
         lcSurName2    = ""
         lcBirthDay    = STRING(ttCustomer.Birthday,
                               "99999999").
      ELSE ASSIGN 
         lcFirstName   = ttCustomer.FirstName
         lcSurName1    = ttCustomer.Custname
         lcSurname2    = ttCustomer.Surname2
         lcBirthDay    = STRING(ttCustomer.Birthday,
                               "99999999").

   END.

   ELSE DO:
      FIND Customer WHERE 
           Customer.CustNum = MSRequest.CustNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Customer THEN RETURN "ERROR:Customer missing".

      ASSIGN
         lcCustIDType  = Customer.CustIdType
         lcOrgID       = Customer.Orgid
         lcAddress     = Customer.Address
         lcZipCode     = Customer.ZipCode
         lcRegion      = Customer.Region
         lcPostOffice  = Customer.PostOffice
         lcNationality = Customer.Nationality
         lcBankAcc     = Customer.BankAcc
         lcNumberType  = "N"
         lcEMail       = Customer.Email
         lcPhone1      = Customer.MobileNumber
         lcPhone2      = Customer.Phone
         lcSalesman    = "Y100000000"
         lcSimOnly     = "Y"
         lcOldPaymType = "P"
         lcSubsType = MsRequest.ReqCParam2.

      IF LOOKUP(lcSubsType,lcBundleCLITypes) > 0 AND
         MsRequest.ReqCParam5 > "" THEN
         lcSubsType = MsRequest.ReqCParam5.
         
      IF lcCustIDType = "CIF" THEN ASSIGN 
         lcFirstName   = ""
         lcSurName1    = Customer.CompanyName
         lcSurName2    = ""
         lcBirthDay    = STRING(Customer.FoundationDate,"99999999").
      ELSE ASSIGN 
         lcFirstName   = Customer.FirstName
         lcSurName1    = Customer.CustName
         lcSurName2    = Customer.SurName2
         lcBirthDay    = STRING(Customer.Birthday,"99999999").

      IF lcType = "STC_RENOVE" THEN ASSIGN
         lcSalesman = "Y300000000"
         lcSimOnly = "N"
         lcOfferId = Order.Offer.

      FOR FIRST Order NO-LOCK WHERE
            Order.Brand = Syst.Var:gcBrand                   AND
            Order.OrderID = bOrigRequest.ReqIParam2 /*AND
            Order.DeliveryType = {&ORDER_DELTYPE_POS} */ ,
         FIRST OrderCustomer NO-LOCK WHERE
            OrderCustomer.Brand = "1"               AND
            OrderCustomer.OrderId = Order.OrderID   AND
            OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY}:

         ASSIGN
            lcAddress     = OrderCustomer.Address
            lcZipCode     = OrderCustomer.ZipCode
            lcRegion      = OrderCustomer.Region
            lcPostOffice  = OrderCustomer.PostOffice
            .
      END.
   END.      
         
   IF lcBirthDay = ? THEN lcBirthDay = "".
   IF LENGTH(lcBankAcc) EQ 24 THEN lcBankAcc = SUBSTRING(lcBankAcc,5).
   
   DO liLoop = 1 TO 27:
      
      IF lcType NE "STC_RENOVE" THEN lcOfferId = "". 

      CREATE ttCreditCheck.
      
      CASE liLoop:
         WHEN 1 THEN ASSIGN
            ttCreditCheck.ttName   = "person_id"
            ttCreditCheck.ttValue  = lcOrgid 
            ttCreditCheck.ttFormat = "string".
         WHEN 2 THEN ASSIGN
            ttCreditCheck.ttName   = "first_name"
            ttCreditCheck.ttValue  = lcFirstName
            ttCreditCheck.ttFormat = "string".
         WHEN 3 THEN ASSIGN
            ttCreditCheck.ttName   = "last_name"
            ttCreditCheck.ttValue  = lcSurName1
            ttCreditCheck.ttFormat = "string".
         WHEN 4 THEN ASSIGN
            ttCreditCheck.ttName  = "2nd_last_name"
            ttCreditCheck.ttValue   = lcSurName2
            ttCreditCheck.ttFormat = "string".
         WHEN 5 THEN ASSIGN
            ttCreditCheck.ttName   = "birthday"
            ttCreditCheck.ttValue  = lcBirthday
            ttCreditCheck.ttFormat = "string".
         WHEN 6 THEN ASSIGN
            ttCreditCheck.ttName   = "postal_code"
            ttCreditCheck.ttValue  = lcZipCode
            ttCreditCheck.ttFormat = "string".
         WHEN 7 THEN ASSIGN
            ttCreditCheck.ttName   = "address"
            ttCreditCheck.ttValue  = lcAddress
            ttCreditCheck.ttFormat = "string".
         WHEN 8 THEN ASSIGN
            ttCreditCheck.ttName   = "account_number"
            ttCreditCheck.ttValue  = lcBankAcc
            ttCreditCheck.ttFormat = "string".
         WHEN 9 THEN ASSIGN
            ttCreditCheck.ttName  = "nationality"
            ttCreditCheck.ttValue   = lcNationality
            ttCreditCheck.ttFormat = "string".
         WHEN 10 THEN ASSIGN
            ttCreditCheck.ttName   = "kind_of_number"
            ttCreditCheck.ttValue  = lcNumberType
            ttCreditCheck.ttFormat = "string".
         WHEN 11 THEN ASSIGN
            ttCreditCheck.ttName   = "province"
            ttCreditCheck.ttValue  = lcRegion
            ttCreditCheck.ttFormat = "string".
        WHEN 12 THEN ASSIGN 
            ttCreditCheck.ttName   = "city"
            ttCreditCheck.ttValue  = lcPostOffice
            ttCreditCheck.ttFormat = "string".
        WHEN 13 THEN ASSIGN 
            ttCreditCheck.ttName   = "typeofdocument"
            ttCreditCheck.ttValue  = lcCustIdType
            ttCreditCheck.ttFormat = "string".
        WHEN 14 THEN ASSIGN 
            ttCreditCheck.ttName   = "email"
            ttCreditCheck.ttValue  = lcEmail
            ttCreditCheck.ttFormat = "string".
        WHEN 15 THEN ASSIGN 
            ttCreditCheck.ttName   = "telephone1"
            ttCreditCheck.ttValue  = lcPhone1
            ttCreditCheck.ttFormat = "string".
        WHEN 16 THEN ASSIGN 
            ttCreditCheck.ttName   = "telephone2"
            ttCreditCheck.ttValue  = lcPhone2
            ttCreditCheck.ttFormat = "string".
        WHEN 17 THEN ASSIGN 
            ttCreditCheck.ttName   = "dlv_address"
            ttCreditCheck.ttValue  = lcAddress
            ttCreditCheck.ttFormat = "string".
        WHEN 18 THEN ASSIGN 
            ttCreditCheck.ttName   = "dlv_po_code"
            ttCreditCheck.ttValue  = lcZipCode
            ttCreditCheck.ttFormat = "string".
        WHEN 19 THEN ASSIGN 
            ttCreditCheck.ttName   = "delivery_city"
            ttCreditCheck.ttValue  = lcPostOffice
            ttCreditCheck.ttFormat = "string".
        WHEN 20 THEN ASSIGN 
            ttCreditCheck.ttName   = "dlv_province"
            ttCreditCheck.ttValue  = lcRegion
            ttCreditCheck.ttFormat = "string".
        WHEN 21 THEN ASSIGN 
            ttCreditCheck.ttName   = "order_salesman_id"
            ttCreditCheck.ttValue  = lcSalesman
            ttCreditCheck.ttFormat = "string".
        WHEN 22 THEN ASSIGN 
            ttCreditCheck.ttName   = "order_simonly_flag"
            ttCreditCheck.ttValue  = lcSimOnly
            ttCreditCheck.ttFormat = "string".
        WHEN 23 THEN ASSIGN 
            ttCreditCheck.ttName   = "order_offerid" 
            ttCreditCheck.ttValue  = lcOfferId
            ttCreditCheck.ttFormat = "string". 
        WHEN 24 THEN ASSIGN 
            ttCreditCheck.ttName   = "order_subscriptiontype"
            ttCreditCheck.ttValue  = lcSubsType
            ttCreditCheck.ttFormat = "string".
        WHEN 25 THEN ASSIGN 
            ttCreditCheck.ttName   = "mnp_previous_operator"
            ttCreditCheck.ttValue  = SUBSTRING(lcOldOperator,1,16)
            ttCreditCheck.ttFormat = "string".
        WHEN 26 THEN ASSIGN 
            ttCreditCheck.ttName   = "mnp_previous_payment_type"
            ttCreditCheck.ttValue  = lcOldPaymType
            ttCreditCheck.ttFormat = "string".
        
        /* YDR-191 */
        WHEN 27 THEN ASSIGN 
            ttCreditCheck.ttName   = "imei"
            ttCreditCheck.ttValue  = lcIMEI
            ttCreditCheck.ttFormat = "string".
      END.

   END.

   RETURN "".
   
END PROCEDURE.

