/**
 * Set address and or marketing data (by custnum).
 * The data will be changed immediately (no request).
 *
 * @input int;mandatory;id of customer (custnum)
          string;mandatory;person who requests the change (salesman)
          boolean;mandatory;whether the change should be billed (create_fee)
          struct;mandatory;data to be changed (rest can be omitted) (customer)
          struct;mandatory;data of company contact customer (company_contact)
          struct;optional;memo

 * @customer id_type;string;optional
            person_id;string;optional;representative if corporate customer
            id_type;string;optional;representative if corporate customer
            company_id;string;optional;Corporate customer CIF
            title;string;optional;
            fname;string;optional;
            lname;string;optional;
            company_name;string;optional;
            coname;string;optional;
            street;string;optional;
            zip;string;optional;
     	      city;string;optional;
            language;string;optional;
            nationality;string;optional;
            bankaccount;string;optional;
	         country;string;optional;
            email;string;optional;
            sms_number;string;optional;
            phone_number;string;optional;
            mark_post;boolean;optional;
            mark_sms;boolean;optional;
            mark_email;boolean;optional;
            mark_post_3rd;boolean;optional;
            mark_sms_3rd;boolean;optional;
            mark_email_3rd;boolean;optional;
            birthday;date;optional;
            company_foundationdate;date;optional;
            new_subscription_grouping;int;optional;1=Use default invoice group,2=Use new invoice group
            payment_method;int;mandatory;
 * @company_contact title;string;
                    fname;string;
                    lname;string;
                    lname2;string;
                    street;string;
                    zip;string;
                    city;string;
                    region;string;
                    language;string;
                    nationality;string;
                    email;string;
                    sms_number;string;
                    id_type;string;
                    person_id;string;
                    city_code;string;
                    street_code;string;
                    municipality_code;string;
 * @memo title;string;mandatory;
         content;string;mandatory;
 * @output success;boolean
 *
 * Note: Address data is updated using RPC newton__set_customer_address.p
 */
{xmlrpc/xmlrpc_access.i}
{tmsconst.i}
{timestamp.i}

/* Input parameters */
DEF VAR piCustNum AS INT NO-UNDO.
DEF VAR plCreateFee AS LOGICAL NO-UNDO.
DEF VAR pcSalesman AS CHAR NO-UNDO.
DEF VAR pcstruct AS CHAR NO-UNDO.
DEF VAR pcCCstruct AS CHAR NO-UNDO.
DEF VAR pcvalue AS CHAR NO-UNDO.
DEF VAR piMobSubLimit AS INT NO-UNDO.
DEF VAR pcMobsubLimit AS CHARACTER NO-UNDO.
DEF VAR piMobSubActLimit AS INT NO-UNDO.
DEF VAR pcMobsubActLimit AS CHARACTER NO-UNDO.
DEF VAR scUser AS CHAR NO-UNDO INIT "Newton".
DEF VAR pcMemoStruct AS CHARACTER NO-UNDO. 
DEF VAR pcMemoTitle AS CHARACTER NO-UNDO. 
DEF VAR pcMemoContent AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
DEFINE VARIABLE lhCustContact AS HANDLE NO-UNDO.

IF validate_request(param_toplevel_id, "int,string,boolean,struct,struct,[struct]") EQ ? THEN
    RETURN.
pcstruct = get_struct(param_toplevel_id, "3").
pcCCstruct = get_struct(param_toplevel_id, "4").
plCreateFee = get_bool(param_toplevel_id, "2").
pcSalesman = get_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF get_paramcount(param_toplevel_id) > 5 THEN DO:

   pcMemoStruct = get_struct(param_toplevel_id, "5").
   validate_struct(pcMemoStruct,"title!,content!").

   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   ASSIGN
      pcMemoTitle = get_string(pcMemoStruct,"title")
      pcMemoContent = get_string(pcMemoStruct,"content").
END.
scUser = pcSalesman. /* Read from eventlog functions into eventlog.user */
piCustNum = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST customer
WHERE customer.custnum = piCustNum
  AND customer.brand = "1" NO-ERROR.
IF NOT AVAILABLE Customer THEN
    RETURN appl_err(SUBST("Customer for &1 not found", piCustNum)).

{commpaa.i}
katun = "VISTA_" + scUser.
gcBrand = "1".

/* Local variables */
DEF VAR lcstruct AS CHAR NO-UNDO.
DEF VAR lcc AS CHAR NO-UNDO.
DEF VAR lii AS INT NO-UNDO.
DEF VAR llt AS LOGICAL NO-UNDO.
DEF VAR llCustomerChanged AS LOGICAL INITIAL FALSE NO-UNDO.

DEF VAR lcCustomerData AS CHAR EXTENT 23 NO-UNDO.
DEF VAR llMarketingData AS LOGICAL EXTENT 6 NO-UNDO.
DEF VAR lcDataFields AS CHAR NO-UNDO.
DEF VAR lcMarketingFields AS CHAR NO-UNDO.
DEF VAR lcContactCustFields AS CHAR NO-UNDO.
DEF VAR lcCustContact AS CHAR NO-UNDO.
DEF VAR liMsSeq AS INT NO-UNDO.
DEF VAR lcField AS CHARACTER NO-UNDO.
DEF VAR ldFoundationDate AS DATE NO-UNDO. 
DEF VAR ldBirthDay AS DATE NO-UNDO. 
DEF VAR lcBirthDay AS CHAR NO-UNDO. 
DEF VAR llNoPaperInvoice AS LOGICAL INITIAL FALSE NO-UNDO.
DEF VAR liInvoiceTargetRule AS INTEGER NO-UNDO. 
DEF VAR liRequest AS INT NO-UNDO.
DEF VAR lcResult AS CHAR NO-UNDO.
DEF VAR liDelType AS INT NO-UNDO.
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcMemoHostTable AS CHAR NO-UNDO INIT "Customer".
DEF VAR liChargeType AS INT NO-UNDO.

ASSIGN
    lcCustomerData[1] = customer.HonTitle
    lcCustomerData[2] = customer.custname
    lcCustomerData[3] = customer.SurName2
    lcCustomerData[4] = customer.firstname
    lcCustomerData[5] = customer.coname
    lcCustomerData[6] = customer.address
    lcCustomerData[7] = customer.zipcode
    lcCustomerData[8] = customer.postoffice
    lcCustomerData[9] = customer.region
    lcCustomerData[10] = ENTRY(Customer.Language, {&languages}) 
    lcCustomerData[11] = customer.nationality
    lcCustomerData[12] = customer.BankAcct
    lcCustomerData[13] = customer.country
    lcCustomerData[14] = customer.email
    lcCustomerData[15] = customer.SMSNumber
    lcCustomerData[16] = customer.Phone
    lcCustomerData[17] = customer.orgid
    lcCustomerData[18] = customer.addresscodp
    lcCustomerData[19] = customer.addresscodc
    lcCustomerData[20] = customer.custidtype
    lcCustomerData[21] = customer.OrgId
    lcCustomerData[22] = customer.CompanyName
    ldFoundationDate   = customer.FoundationDate
    liInvoiceTargetRule = customer.InvoiceTargetRule
    ldBirthDay         = customer.BirthDay
    liChargeType       = customer.ChargeType.

lcCustomerData[23] = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "Invoice",
                                      "DelType",
                                      STRING(Customer.DelType)).

ASSIGN
    llMarketingData[1] = customer.DirMarkSMS
    llMarketingData[2] = customer.DirMarkEmail
    llMarketingData[3] = customer.DirMarkPost
    llMarketingData[4] = customer.OutMarkSMS
    llMarketingData[5] = customer.OutMarkEmail
    llMarketingData[6] = customer.OutMarkPost.
lcDataFields = "title,lname,lname2,fname,coname,street,zip,city,region," +
               "language,nationality,bankaccount,country," +
               "email,sms_number,phone_number,person_id,city_code,street_code,"+
               "id_type,company_id,company_name," +
               "birthday,company_foundationdate,new_subscription_grouping,payment_method". /* special list */
lcMarketingFields = "mark_sms,mark_email,mark_post," +
                    "mark_sms_3rd,mark_email_3rd,mark_post_3rd".

lcContactCustFields = "title,fname,lname,lname2,street,zip,city,region" +
                      ",language,nationality,email,sms_number" +
                      ",id_type,person_id,street_code,city_code,municipality_code".

lcstruct = validate_request(pcstruct,
        TRIM(lcDataFields + ",subscription_limit,subscription_act_limit," +
             lcMarketingFields, ",")).

IF get_paramcount(pcCCStruct) > 0 THEN 
   lcCustContact = validate_request(pcCCStruct,lcContactCustFields). /* CLI */

IF gi_xmlrpc_error NE 0 THEN RETURN.

DO lii = 1 TO NUM-ENTRIES(lcDataFields):
    
    lcField = ENTRY(lii, lcDataFields).
    
    IF LOOKUP(lcField, lcStruct) GT 0 THEN DO:

      IF lcField EQ "company_foundationdate" THEN DO:
         ldFoundationDate = get_date(pcStruct, lcField).
         IF ldFoundationDate ne Customer.FoundationDate THEN DO:
            llCustomerChanged = TRUE.
         END.
      END.
      ELSE IF lcField EQ "payment_method" THEN DO:

         liChargeType = get_int(pcStruct, lcField).
         IF Customer.ChargeType NE liChargeType THEN DO:

            IF LOOKUP(STRING(liChargeType),"2,5,6") =  0 THEN DO:
               RETURN appl_err(SUBST("Wrong payment_method value &1", liChargeType)). 
            END.
            llCustomerChanged = TRUE.
         END.
      END.
      ELSE IF lcField EQ "new_subscription_grouping" THEN DO:

         liInvoiceTargetRule = get_int(pcStruct, lcField).
         IF Customer.InvoiceTargetRule NE liInvoiceTargetRule THEN DO:

            IF LOOKUP(STRING(liInvoiceTargetRule),"1,2") =  0 THEN DO:
               RETURN appl_err(SUBST("Wrong new_subscription_grouping value &1", liInvoiceTargetRule)). 
            END.
            llCustomerChanged = TRUE.
         END.
      END.
      ELSE IF lcField EQ "birthday" THEN DO:
         lcBirthday = get_string(pcStruct, lcField).
         ldBirthday = DATE(lcBirthday) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            RETURN appl_err(SUBST("Wrong birthday format &1", lcBirthday)). 
         IF ldBirthday ne Customer.birthday THEN DO:
            llCustomerChanged = TRUE.
         END.
      END.
      ELSE DO:  
        lcc = get_string(pcstruct, ENTRY(lii, lcDataFields)).
        IF lcc NE lcCustomerData[lii] THEN DO:
            /* Store id_type and person_id to CustContact table if
               corporate customer is used */
            IF LOOKUP(ENTRY(lii, lcDataFields),"id_type,person_id") > 0 AND
            Customer.CustIdType = "CIF" THEN NEXT.
            lcCustomerData[lii] = lcc.
            llCustomerChanged = TRUE.
        END.
      END.    
      
      IF gi_xmlrpc_error NE 0 THEN RETURN.
    
    END.
END.
lcCustomerData[LOOKUP("region", lcDataFields)] =
    SUBSTRING(lcCustomerData[LOOKUP("zip", lcDataFields)], 1, 2).
DO lii = 1 TO NUM-ENTRIES(lcMarketingFields):
    IF LOOKUP(ENTRY(lii, lcMarketingFields), lcStruct) GT 0 THEN DO:
        llt = get_bool(pcstruct, ENTRY(lii, lcMarketingFields)).
        IF llt NE llMarketingData[lii] THEN DO:
            llMarketingData[lii] = llt.
            llCustomerChanged = TRUE.
        END.
    END.
END.

IF gi_xmlrpc_error NE 0 THEN RETURN.

{flimitreq.i}
{fcustdata.i}
{fmakemsreq.i}
{femailinvoice.i}

DEF BUFFER bCustomer FOR Customer.
DEFINE VARIABLE lcOrgId AS CHARACTER NO-UNDO. 
DEF VAR lcCustIdType AS CHAR NO-UNDO.
DEF VAR llDefaultSubsLimit AS LOG NO-UNDO.
DEF VAR lcNewEmailAdd AS CHAR NO-UNDO.
DEF VAR ldate AS DATE NO-UNDO.
DEF VAR litime AS INT NO-UNDO. 
DEF VAR ldePrepStcTs AS DEC NO-UNDO. 
DEF VAR llBankAcctChange AS LOG NO-UNDO. 
DEF VAR lcBankAccount AS CHAR NO-UNDO. 

IF Customer.CustIdType = "CIF" THEN 
   lcOrgId = lcCustomerData[LOOKUP("company_id", lcDataFields)].
ELSE lcOrgId = lcCustomerData[LOOKUP("person_id", lcDataFields)].

IF Customer.orgid NE lcOrgId THEN DO:

   FIND FIRST bCustomer WHERE
      bCustomer.Brand = gcBrand AND
      bCustomer.OrgID = lcOrgId
   NO-LOCK NO-ERROR.

   IF AVAIL bCustomer THEN DO:
      RETURN appl_err("Customer exists with same id").
   END.
END.
   
IF Customer.CustIdType NE "CIF" THEN DO:

   lcCustIdType = lcCustomerData[LOOKUP("id_type", lcDataFields)].
   
   IF lcCustIdType NE Customer.CustIdType THEN DO:
      
      FIND FIRST bCustomer WHERE
         bCustomer.Brand = gcBrand AND
         bCustomer.OrgID = lcOrgId AND
         bCustomer.CustIdType = lcCustIdType NO-LOCK NO-ERROR.
      
      IF AVAIL bCustomer THEN
         RETURN appl_err("Customer exists with same id").
   END.
END.

IF Customer.CustIdType = "CIF" THEN DO:
   
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).
   
   FIND CURRENT Customer EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL Customer THEN DO:
      RUN StarEventSetOldBuffer(lhCustomer).
      ASSIGN
         Customer.AuthCustIdType = get_string(pcStruct, "id_type") 
         WHEN LOOKUP("id_type", lcStruct) > 0 
         Customer.AuthCustId = get_string(pcStruct, "person_id")
         WHEN LOOKUP("person_id", lcStruct) > 0.
      RUN StarEventMakeModifyEvent(lhCustomer).
   END.
   FIND CURRENT Customer NO-LOCK.
  
   lhCustContact = BUFFER CustContact:HANDLE.
   RUN StarEventInitialize(lhCustContact).
   
   FIND FIRST CustContact WHERE
              CustContact.Brand = gcBrand AND
              CustContact.CustNum = piCustNum AND
              CustContact.CustType = 5 EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL CustContact THEN DO:
      RUN StarEventSetOldBuffer(lhCustContact).
   END.
   ELSE IF get_paramcount(pcCCStruct) > 0 THEN DO:  
      CREATE CustContact.
      ASSIGN
         CustContact.Brand = gcBrand
         CustContact.Custnum = Customer.Custnum
         CustContact.CustType = 5.
   END.

   IF AVAIL CustContact THEN DO:
      ASSIGN
         CustContact.HonTitle    = get_string(pcCCstruct, "title") 
         WHEN LOOKUP("title", lcCustContact) > 0 
         
         CustContact.FirstName   = get_string(pcCCstruct, "fname")
         WHEN LOOKUP("fname", lcCustContact) > 0 
         
         CustContact.CustName    = get_string(pcCCstruct, "lname") 
         WHEN LOOKUP("lname", lcCustContact) > 0 
         
         CustContact.SurName2    = get_string(pcCCstruct, "lname2") 
         WHEN LOOKUP("lname2", lcCustContact) > 0 
         
         CustContact.CustIdType  = get_string(pcCCstruct, "id_type") 
         WHEN LOOKUP("id_type", lcCustContact) > 0 
         
         CustContact.OrgId       = get_string(pcCCstruct, "person_id") 
         WHEN LOOKUP("person_id", lcCustContact) > 0 
         
         CustContact.Nationality = get_string(pcCCstruct, "nationality") 
         WHEN LOOKUP("nationality", lcCustContact) > 0 
         
         CustContact.Language  = LOOKUP(get_string(pcCCstruct, "language"),
                     {&languages})
         WHEN LOOKUP("language", lcCustContact) > 0 
         
         CustContact.SMSNumber = get_string(pcCCstruct, "sms_number") 
         WHEN LOOKUP("sms_number", lcCustContact) > 0 
         
         CustContact.Email       = get_string(pcCCstruct, "email") 
         WHEN LOOKUP("email", lcCustContact) > 0 
         
         CustContact.Address     = get_string(pcCCstruct, "street") 
         WHEN LOOKUP("street", lcCustContact) > 0 
         
         CustContact.ZipCode     = get_string(pcCCstruct, "zip") 
         WHEN LOOKUP("zip", lcCustContact) > 0 
         
         CustContact.PostOffice  = get_string(pcCCstruct, "city") 
         WHEN LOOKUP("city", lcCustContact) > 0 
         
         CustContact.Region      = get_string(pcCCstruct, "region") 
         WHEN LOOKUP("region", lcCustContact) > 0 
         
         CustContact.AddressCodC = get_string(pcCCstruct, "street_code") 
         WHEN LOOKUP("street_code", lcCustContact) > 0 
         
         CustContact.AddressCodP = get_string(pcCCstruct, "city_code")
         WHEN LOOKUP("city_code", lcCustContact) > 0
         
         CustContact.AddressCodM = get_string(pcCCstruct, "municipality_code")
         WHEN LOOKUP("municipality_code", lcCustContact) > 0. 

      IF NEW CustContact THEN RUN StarEventMakeCreateEvent (lhCustContact).
      ELSE RUN StarEventMakeModifyEvent(lhCustContact).
   END.
END.

IF llCustomerChanged THEN DO:
    lhCustomer = BUFFER Customer:HANDLE.
    RUN StarEventInitialize(lhCustomer).
    RUN StarEventSetOldBuffer(lhCustomer).
    FIND CURRENT Customer EXCLUSIVE-LOCK.
    ASSIGN
        customer.HonTitle = lcCustomerData[LOOKUP("title", lcDataFields)]
        customer.custname = lcCustomerData[LOOKUP("lname", lcDataFields)]
        customer.SurName2 = lcCustomerData[LOOKUP("lname2", lcDataFields)]
        customer.firstname = lcCustomerData[LOOKUP("fname", lcDataFields)]
        customer.coname = lcCustomerData[LOOKUP("coname", lcDataFields)]
        customer.language = LOOKUP(lcCustomerData[LOOKUP("language", lcDataFields)], {&languages})
        customer.nationality = lcCustomerData[LOOKUP("nationality", lcDataFields)]
        customer.SMSNumber = lcCustomerData[LOOKUP("sms_number", lcDataFields)]
        customer.Phone = lcCustomerData[LOOKUP("phone_number", lcDataFields)]
        customer.CustIdType = lcCustomerData[LOOKUP("id_type", lcDataFields)]
        customer.orgid = lcCustomerData[LOOKUP("person_id", lcDataFields)]
        customer.CompanyName = lcCustomerData[LOOKUP("company_name", lcDataFields)]
        customer.DirMarkSMS = llMarketingData[1]
        customer.DirMarkEmail = llMarketingData[2]
        customer.DirMarkPost = llMarketingData[3]
        customer.OutMarkSMS = llMarketingData[4]
        customer.OutMarkEmail = llMarketingData[5]
        customer.OutMarkPost = llMarketingData[6]
        customer.orgid = lcCustomerData[LOOKUP("company_id", lcDataFields)] WHEN customer.custidtype = "CIF"
        customer.foundationDate = ldFoundationDate 
        customer.BirthDay = ldBirthDay
        customer.InvoiceTargetRule = liInvoiceTargetRule
        customer.ChargeType = liChargeType.
        
    /* Added check for BankAccount change, YDR-1811
      It's not allowed if customer has active PostPaid subscription OR
      last termination date(of stc to prepaid) is less than 40 days. 
      
      Also checking of BankAccount lenght. It can be empty (0) length too */
    fSplitTS(fmakeTS(), ldate, litime).
    ldate = ldate - 40.
    ldePrepStcTs = fHMS2TS(ldate, "00:00:00").
    lcBankAccount = TRIM(lcCustomerData[LOOKUP("bankaccount", lcDataFields)]).
    
    IF CAN-FIND( FIRST MobSub NO-LOCK WHERE Mobsub.Brand = gcBrand AND 
       MobSub.CustNum = piCustNum AND NOT Mobsub.PayType) OR
       CAN-FIND( FIRST MsRequest NO-LOCK WHERE MsRequest.Brand = gcBrand AND
       MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
       MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
       MsRequest.ActStamp > ldePrepStcTs AND
       MsRequest.ReqCParam1 BEGINS "CONT" AND
       MsRequest.ReqCParam2 BEGINS "TARJ") AND
       MsRequest.CustNum = piCustNum THEN
         RETURN appl_err("La cuenta bancaria no puede estar en blanco").
    ELSE DO:
      IF LENGTH(lcBankAccount) = 0 OR LENGTH(lcBankAccount) = 24 THEN DO:
         customer.BankAcct = lcBankAccount.
         llBankAcctChange = TRUE.
      END.
      ELSE
         RETURN appl_err("La cuenta bancaria no puede estar en blanco").
    END.
    
    /* Electronic Invoice Project */
    lcNewEmailAdd = lcCustomerData[LOOKUP("email", lcDataFields)].

    IF Customer.Email <> lcNewEmailAdd THEN DO:
       IF Customer.DelType EQ {&INV_DEL_TYPE_EMAIL} OR
          Customer.DelType EQ {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO:

          IF lcNewEmailAdd = "" THEN
             RETURN appl_err("Customer email address can not be blank because " +
                             "customer's invoice delivery type is EMAIL").

          /* Cancel Ongoing Email Activation Request and create new */
         
          IF fPendingEmailActRequest(INPUT Customer.Custnum) THEN
             fCancelPendingEmailActRequest(
                                INPUT Customer.Custnum,
                                INPUT "Customer email address is changed").

          liRequest = fEmailInvoiceRequest(INPUT fMakeTS(),
                                           INPUT TODAY,
                                           INPUT katun,
                                           INPUT 0, /* msseq */
                                           INPUT "", /* cli */
                                           INPUT Customer.CustNum,
                                           INPUT {&REQUEST_SOURCE_NEWTON},
                                           INPUT lcNewEmailAdd,
                                           INPUT 0, /* orderid */
                                           OUTPUT lcResult).
          IF liRequest = 0 THEN
             RETURN appl_err("Customer email address can not be changed: " +
                             lcResult).

          /* If Email already validated then mark DelType EMAIL */
          IF liRequest = 1 THEN
             Customer.DelType = {&INV_DEL_TYPE_EMAIL}.
          ELSE
             Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING}.
       END. /* IF Customer.DelType EQ {&INV_DEL_TYPE_EMAIL} */

       FIND FIRST InvoiceTargetGroup WHERE
                  InvoiceTargetGroup.CustNum = Customer.CustNum AND
                  InvoiceTargetGroup.ToDate >= TODAY AND
                 (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                  InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
            EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL InvoiceTargetGroup THEN DO:
          /* If request is not already created from Customer DelType */
          IF liRequest = 0 THEN DO:
             /* Cancel existing ReqType=84 request */
             IF fPendingEmailActRequest(INPUT Customer.Custnum) THEN
                fCancelPendingEmailActRequest(
                              INPUT Customer.Custnum,
                              INPUT "Customer email address is changed").

             liRequest = fEmailInvoiceRequest(INPUT fMakeTS(),
                                              INPUT TODAY,
                                              INPUT katun,
                                              INPUT 0, /* msseq */
                                              INPUT "", /* cli */
                                              INPUT Customer.CustNum,
                                              INPUT {&REQUEST_SOURCE_FUSION_EMAIL},
                                              INPUT lcNewEmailAdd,
                                              INPUT 0, /* msseq */
                                              OUTPUT lcResult).
             IF liRequest = 0 THEN
                RETURN appl_err("Customer email address can not be changed: " +
                                lcResult).
          END. /* IF liRequest = 0 THEN DO: */

          /* If Email already validated then mark DelType EMAIL */
          IF liRequest = 1 THEN
             InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL}.
          ELSE
             InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}.

          RELEASE InvoiceTargetGroup.
       END. /* IF AVAIL InvoiceTargetGroup THEN DO: */

       Customer.Email = lcNewEmailAdd.
    END. /* IF customer.email */

    FIND CURRENT Customer NO-LOCK.
    RUN StarEventMakeModifyEvent(lhCustomer).
END.
   
fCleanEventObjects().

IF LOOKUP("subscription_limit", lcStruct) > 0 THEN DO:
   
   pcMobsubLimit = get_string(pcstruct, "subscription_limit").
   
   IF pcMobsubLimit = "" THEN DO:
      
      fGetLimit (Customer.Custnum, 0, {&LIMIT_TYPE_SUBQTY}, 0, 0, TODAY).
      
      IF AVAIL Limit THEN 
         fSetLimit(
            ROWID(Limit),
            ?,
            Limit.DefValue,
            Limit.FromDate,
            Limit.ToDate).
   END.
   ELSE DO:
      
      piMobSubLimit = INT(pcMobsubLimit) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN appl_err("Subscription limit must be integer").

      IF piMobsubLimit < 0 OR piMobsubLimit > 999 THEN 
         RETURN appl_err(SUBST("Invalid subscription limit value &1", piMobsubLimit)).
      
      fGetLimit (Customer.Custnum, 0, {&LIMIT_TYPE_SUBQTY}, 0, 0, TODAY).

      IF AVAIL Limit THEN DO:
         IF Limit.LimitAmt NE piMobsubLimit THEN
            fSetLimit(ROWID(Limit), piMobsubLimit, FALSE, TODAY, 12/31/2049).
      END.
      ELSE DO:
         fCreateLimit(Customer.Custnum,
                      0,
                      {&LIMIT_TYPE_SUBQTY},
                      piMobsubLimit,
                      0,
                      0,
                      TODAY,
                      12/31/2049).
      END.
   END.
END.

IF LOOKUP("subscription_act_limit", lcStruct) > 0 THEN DO:
   
   pcMobsubActLimit = get_string(pcstruct, "subscription_act_limit").
   
   IF pcMobsubActLimit = "" THEN DO:
      
      fGetLimit (Customer.Custnum, 0, {&LIMIT_TYPE_SUBACTQTY}, 0, 0, TODAY).
      
      IF AVAIL Limit THEN 
         fSetLimit(
            ROWID(Limit),
            ?,
            Limit.DefValue,
            Limit.FromDate,
            Limit.ToDate).
   END.
   ELSE DO:
      
      piMobSubActLimit = INT(pcMobsubActLimit) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         RETURN appl_err("Subscription activation limit must be integer").

      IF piMobsubActLimit < 0 OR piMobsubActLimit > 999 THEN 
         RETURN appl_err(SUBST("Invalid subscription activation limit value &1",
                               piMobsubActLimit)).
      
      /* Get original Subs. limit */
      piMobsubLimit = fGetMobsubLimit(INPUT Customer.Custnum,
                                      INPUT Customer.Category,
                                      OUTPUT llDefaultSubsLimit).
      IF piMobsubActLimit < piMobsubLimit THEN
         RETURN appl_err("Subscription activation limit can not be less " +
                         "than subscription limit").

      fGetLimit (Customer.Custnum, 0, {&LIMIT_TYPE_SUBACTQTY}, 0, 0, TODAY).

      IF AVAIL Limit THEN DO:
         IF Limit.LimitAmt NE piMobsubActLimit THEN
            fSetLimit(ROWID(Limit), piMobsubActLimit, FALSE, TODAY, 12/31/2049).
      END.
      ELSE DO:
         fCreateLimit(Customer.Custnum,
                      0,
                      {&LIMIT_TYPE_SUBACTQTY},
                      piMobsubActLimit,
                      0,
                      0,
                      TODAY,
                      12/31/2049).
      END.
   END.
END.

IF gc_xmlrpc_error NE "" THEN
    gi_xmlrpc_error = {&APPLICATION_ERROR}.
ELSE
    add_boolean(response_toplevel_id, "", TRUE).

/* YDR-1811 - Memo adding in a case of bank account was changed */
IF pcMemoTitle NE "" OR
   llBankAcctChange THEN DO:

   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = gcBrand
       Memo.HostTable = lcMemoHostTable
       Memo.KeyValue  = STRING(Customer.Custnum)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = katun
       Memo.CustNum   = Customer.Custnum.
   IF llBankAcctChange THEN
      ASSIGN
         Memo.MemoTitle = "Cambio de cuenta"
         Memo.MemoText  = "Solicitado por el cliente: NÂº de " +
                        "cuenta: " + Customer.BankAcct + " --> " +
                        IF lcBankAccount > "" THEN lcBankAccount ELSE
                        "blank".
   ELSE 
      ASSIGN
       Memo.MemoTitle = pcMemoTitle
       Memo.MemoText  = pcMemoContent.
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
