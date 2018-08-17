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
            zip;string;optional;format must be numerical
            region;string;optional;format must be numerical
            city;string;optional;
            city_code;string;optional;address validation code
            street_code;string;optional;address validation code
            municipality_code;string;optional;address validation code
            language;string;optional;es_ES,es_CA,es_EU,es_GA,en or 1,2,3,4,5
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
            mark_dont_share_personal_data;boolean;optional;
            mark_bank_3rd;boolean;optional
            birthday;date;optional;
            company_foundationdate;date;optional;
            new_subscription_grouping;int;optional;1=Use default invoice group,2=Use new invoice group
            payment_method;int;mandatory;
            profession;string;optional;
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
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/fbankdata.i}
{Func/msreqfunc.i}

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
DEF VAR liLanguage AS INT NO-UNDO. 
DEF VAR liContactLanguage AS INT NO-UNDO. 
DEF VAR liReq AS INT NO-UNDO. 
DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
DEFINE VARIABLE lhCustContact AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttCustomerReport NO-UNDO LIKE CustomerReport.

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

{newton/src/findtenant.i NO Common Customer CustNum piCustNum}

IF scUser EQ "selfcare" THEN
   Syst.Var:katun = scUser.
ELSE 
   Syst.Var:katun = "VISTA_" + scUser.

/* Local variables */
DEF VAR lcstruct AS CHAR NO-UNDO.
DEF VAR lcc AS CHAR NO-UNDO.
DEF VAR lii AS INT NO-UNDO.
DEF VAR llt AS LOGICAL NO-UNDO.
DEF VAR llCustomerChanged AS LOGICAL INITIAL FALSE NO-UNDO.

DEF VAR lcCustomerData AS CHAR EXTENT 24 NO-UNDO.    /* APIBSS-174 Changing for 23 to 24 */
DEF VAR llMarketingData AS LOGICAL EXTENT 8 NO-UNDO. /* APIBSS-86 */
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
DEF VAR lcMemo    AS CHAR  NO-UNDO.

lcMemo = "Agent" + CHR(255) + (IF scUser EQ "selfcare" 
                               THEN scUser ELSE "VISTA").

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
    liLanguage         = customer.Language
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
    lcCustomerData[24] = customer.Profession  /* APIBSS-174 */
    ldFoundationDate   = customer.FoundationDate
    liInvoiceTargetRule = customer.InvoiceTargetRule
    ldBirthDay         = customer.BirthDay
    liChargeType       = customer.ChargeType.

lcCustomerData[23] = Func.Common:mTMSCodeName("Invoice",
                                      "DelType",
                                      STRING(Customer.DelType)).

ASSIGN
    llMarketingData[1] = customer.DirMarkSMS
    llMarketingData[2] = customer.DirMarkEmail
    llMarketingData[3] = customer.DirMarkPost
    llMarketingData[4] = customer.OutMarkSMS
    llMarketingData[5] = customer.OutMarkEmail
    llMarketingData[6] = customer.OutMarkPost
    llMarketingData[7] = customer.DontSharePersData
    llMarketingData[8] = customer.OutMarkBank. /* APIBSS-86 */
lcDataFields = "title,lname,lname2,fname,coname,street,zip,city,region," +
               "language,nationality,bankaccount,country," +
               "email,sms_number,phone_number,person_id,city_code,street_code,"+
               "id_type,company_id,company_name," +
               "birthday,profession,company_foundationdate,new_subscription_grouping,payment_method".   /* APIBSS-174 */

DEF VAR lcAddressValidtionFields AS CHAR NO-UNDO. 
lcAddressValidtionFields = "street_code,city_code,municipality_code".

lcMarketingFields = "mark_sms,mark_email,mark_post," +
                    "mark_sms_3rd,mark_email_3rd,mark_post_3rd," +
                    "mark_dont_share_personal_data," + 
                    "mark_bank_3rd". /* APIBSS-86 */

lcContactCustFields = "title,fname,lname,lname2,street,zip,city,region" +
                      ",language,nationality,email,sms_number" +
                      ",id_type,person_id,street_code,city_code,municipality_code".

lcstruct = validate_request(pcstruct,
        TRIM(lcDataFields + "," + lcAddressValidtionFields +
              ",subscription_limit,subscription_act_limit," +
             lcMarketingFields, ",")).

IF get_paramcount(pcCCStruct) > 0 THEN DO:

   lcCustContact = validate_request(pcCCStruct,lcContactCustFields). /* CLI */

   IF LOOKUP("language", lcCustContact) > 0 THEN DO:

      lcc = get_string(pcCCstruct, "language").

      IF LOOKUP(lcc, {&languages}) EQ 0 THEN DO:
         liContactLanguage = INT(lcc) NO-ERROR.
         IF ERROR-STATUS:ERROR OR (liContactLanguage < 1 OR liContactLanguage > 5) THEN
            RETURN appl_err(SUBST("Incorrect custcontact language value: &1", lcc)).
      END.
      ELSE liContactLanguage = INT(STRING(LOOKUP(lcc, {&languages}))).
   END.
END.

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
      ELSE IF lcField EQ "language" THEN DO:
         
         lcc = get_string(pcstruct, lcField).

         IF LOOKUP(lcc, {&languages}) EQ 0 THEN DO:
            liLanguage = INT(lcc) NO-ERROR.
            IF ERROR-STATUS:ERROR OR (liLanguage < 1 OR liLanguage > 5) THEN
               RETURN appl_err(SUBST("Incorrect language value: &1", lcc)).
         END.
         ELSE liLanguage = INT(STRING(LOOKUP(lcc, {&languages}))).
            
         IF Customer.Language NE liLanguage THEN llCustomerChanged = TRUE.
      END.
      ELSE IF lcField EQ "Profession" THEN DO:  /*APIBSS-174 */
         lcc = get_string(pcstruct, lcField).
        
         IF lcc NE lcCustomerData[lii] THEN
         DO:
            IF NOT CAN-FIND(FIRST TMSCodes WHERE
                                  TMSCodes.TableName = "OrderCustomer" AND
                                  TMSCodes.FieldName = "Profession"    AND 
                                  TMSCodes.InUse     = 1               AND 
                                  TMSCodes.CodeValue = lcc NO-LOCK) THEN
               RETURN appl_err(SUBST("Incorrect profession: &1", lcc)).
            
            lcCustomerData[lii] = lcc.
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

{Func/flimitreq.i}
{Func/fcustdata.i}
{Func/fmakemsreq.i}
{Func/femailinvoice.i}

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
DEF VAR llEqual AS LOG NO-UNDO. 

IF Customer.CustIdType = "CIF" THEN 
   lcOrgId = lcCustomerData[LOOKUP("company_id", lcDataFields)].
ELSE lcOrgId = lcCustomerData[LOOKUP("person_id", lcDataFields)].

IF Customer.orgid NE lcOrgId THEN DO:

   FIND FIRST bCustomer WHERE
      bCustomer.Brand = Syst.Var:gcBrand AND
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
         bCustomer.Brand = Syst.Var:gcBrand AND
         bCustomer.OrgID = lcOrgId AND
         bCustomer.CustIdType = lcCustIdType NO-LOCK NO-ERROR.
      
      IF AVAIL bCustomer THEN
         RETURN appl_err("Customer exists with same id").
   END.
END.

/* UPDATE BEGINS */
CUST_UPDATE:
DO TRANS:

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
      RUN StarEventMakeModifyEventWithMemo(lhCustomer, 
                                           {&STAR_EVENT_USER}, 
                                           lcMemo).
   END.
   FIND CURRENT Customer NO-LOCK.
  
   lhCustContact = BUFFER CustContact:HANDLE.
   RUN StarEventInitialize(lhCustContact).
   
   FIND FIRST CustContact WHERE
              CustContact.Brand = Syst.Var:gcBrand AND
              CustContact.CustNum = piCustNum AND
              CustContact.CustType = 5 EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL CustContact THEN DO:
      RUN StarEventSetOldBuffer(lhCustContact).
   END.
   ELSE IF get_paramcount(pcCCStruct) > 0 THEN DO:  
      CREATE CustContact.
      ASSIGN
         CustContact.Brand = Syst.Var:gcBrand
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
         
         CustContact.Language  = liContactLanguage
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

    DEF VAR llRegionChanged AS LOG NO-UNDO.
    DEF VAR llAddressChanged AS LOG NO-UNDO. 

    llRegionChanged = Customer.Region NE lcCustomerData[LOOKUP("region", lcDataFields)].
    llAddressChanged = 
       (customer.Address NE lcCustomerData[LOOKUP("street", lcDataFields)] OR
        customer.PostOffice NE lcCustomerData[LOOKUP("city", lcDataFields)] OR
        customer.Country NE lcCustomerData[LOOKUP("country", lcDataFields)] OR
        customer.ZipCode NE lcCustomerData[LOOKUP("zip", lcDataFields)] OR
        customer.Region NE lcCustomerData[LOOKUP("region", lcDataFields)]).

    FIND CURRENT Customer EXCLUSIVE-LOCK.
    ASSIGN
        customer.HonTitle = lcCustomerData[LOOKUP("title", lcDataFields)]
        customer.custname = lcCustomerData[LOOKUP("lname", lcDataFields)]
        customer.SurName2 = lcCustomerData[LOOKUP("lname2", lcDataFields)]
        customer.firstname = lcCustomerData[LOOKUP("fname", lcDataFields)]
        customer.language = liLanguage
        customer.coname = lcCustomerData[LOOKUP("coname", lcDataFields)]
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
        customer.DontSharePersData = llMarketingData[7]
        customer.OutMarkBank = llMarketingData[8]  /* APIBSS-86 */
        customer.orgid = lcCustomerData[LOOKUP("company_id", lcDataFields)] WHEN customer.custidtype = "CIF"
        customer.foundationDate = ldFoundationDate 
        customer.BirthDay = ldBirthDay
        customer.InvoiceTargetRule = liInvoiceTargetRule
        customer.ChargeType = liChargeType 
        customer.profession = lcCustomerData[LOOKUP("profession", lcDataFields)].  /* APIBSS-174 */
          
   IF llAddressChanged THEN DO:
       
      CREATE ttCustomerReport.
      ASSIGN
         ttCustomerReport.StreetCode = get_string(pcstruct,"street_code")
            WHEN LOOKUP("street_code",lcstruct) > 0
         ttCustomerReport.CityCode = get_string(pcstruct,"city_code")
            WHEN LOOKUP("city_code",lcstruct) > 0
         ttCustomerReport.TownCode = get_string(pcstruct,"municipality_code") 
            WHEN LOOKUP("municipality_code",lcstruct) > 0.

      IF gi_xmlrpc_error NE 0 THEN UNDO CUST_UPDATE, RETURN.

      ASSIGN
         customer.Address    = lcCustomerData[LOOKUP("street", lcDataFields)]
         customer.PostOffice = lcCustomerData[LOOKUP("city", lcDataFields)]
         customer.Country    = lcCustomerData[LOOKUP("country", lcDataFields)]
         customer.ZipCode    = lcCustomerData[LOOKUP("zip", lcDataFields)]
         customer.Region     = lcCustomerData[LOOKUP("region", lcDataFields)].

      IF llRegionChanged THEN
         customer.InvGroup = fDefInvGroup(Customer.Region).
   
      liReq = fAddressRequest(
         Customer.Custnum,
         0,
         Customer.Address,
         Customer.ZipCode,
         Customer.PostOffice,
         Customer.Region,
         Customer.Country,
         Customer.Coname,
         ttCustomerReport.StreetCode,
         ttCustomerReport.CityCode,
         ttCustomerReport.TownCode,
         ({&REQUEST_SOURCE_NEWTON}),
         0,
         OUTPUT lcError).

      IF liReq EQ 0 OR lcError NE "" THEN
         UNDO CUST_UPDATE, RETURN appl_err(SUBST("Address change failed: &1", lcError)).
      ELSE DO:
         FIND FIRST MsRequest NO-LOCK WHERE
                    MsRequest.MsRequest = liReq NO-ERROR.
         IF AVAIL MsRequest THEN fReqStatus(2,"").
      END.

      FIND FIRST CustomerReport NO-LOCK WHERE
                 CustomerReport.Custnum = customer.Custnum NO-ERROR.
      
      IF AVAIL CustomerReport THEN
         BUFFER-COMPARE ttCustomerreport TO CustomerReport SAVE llEqual.
      ELSE llEqual = (ttCustomerreport.TownCode EQ "" AND
                      ttCustomerreport.CityCode EQ "" AND
                      ttCustomerreport.StreetCode EQ "").

      IF NOT llEqual THEN DO:
         IF AVAIL CustomerReport THEN DO:
            FIND CURRENT CustomerReport EXCLUSIVE-LOCK.
            IF llDoEvent THEN RUN StarEventSetOldBuffer((BUFFER CustomerReport:HANDLE)). 
            BUFFER-COPY ttCustomerreport EXCEPT Custnum TO CustomerReport.
            IF llDoEvent THEN RUN StarEventMakeModifyEvent((BUFFER CustomerReport:HANDLE)). 
         END. 
         ELSE DO:
            CREATE Customerreport.
            Customerreport.Custnum = Customer.Custnum.
            BUFFER-COPY ttCustomerreport EXCEPT Custnum TO CustomerReport.
         END.
      END.

   END.
        
    /* Added check for BankAccount change, YDR-1811
      It's not allowed if customer has active PostPaid subscription OR
      last termination date(of stc to prepaid) is less than 40 days. 
      
      Also checking of BankAccount lenght. It can be empty (0) length too */
    lcBankAccount = TRIM(lcCustomerData[LOOKUP("bankaccount", lcDataFields)]).
    
    IF Customer.BankAcct <> lcBankAccount THEN DO:
       IF LENGTH(lcBankAccount) = 0 AND 
          NOT fChkBankAccChange(Customer.CustNum) THEN
            UNDO CUST_UPDATE, RETURN appl_err("La cuenta bancaria no puede estar en blanco").
       ELSE DO:
         IF LENGTH(lcBankAccount) = 0 OR LENGTH(lcBankAccount) = 24 THEN DO:
            IF customer.BankAcct = lcBankAccount
            THEN llBankAcctChange = FALSE.
            ELSE llBankAcctChange = TRUE.

            customer.BankAcct = lcBankAccount.
         END.
         ELSE
            UNDO CUST_UPDATE, RETURN appl_err("Incorrect bank account length").
       END.
    END.
    
    /* Electronic Invoice Project */
    lcNewEmailAdd = lcCustomerData[LOOKUP("email", lcDataFields)].

    IF Customer.Email <> lcNewEmailAdd THEN DO:
       IF Customer.DelType EQ {&INV_DEL_TYPE_EMAIL} OR
          Customer.DelType EQ {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO:

          IF lcNewEmailAdd = "" THEN
             UNDO CUST_UPDATE, RETURN appl_err("Customer email address can not be blank because " +
                             "customer's invoice delivery type is EMAIL").

          /* Cancel Ongoing Email Activation Request and create new */
         
          IF fPendingEmailActRequest(INPUT Customer.Custnum) THEN
             fCancelPendingEmailActRequest(
                                INPUT Customer.Custnum,
                                INPUT "Customer email address is changed").

          liRequest = fEmailInvoiceRequest(INPUT Func.Common:mMakeTS(),
                                           INPUT TODAY,
                                           INPUT Syst.Var:katun,
                                           INPUT 0, /* msseq */
                                           INPUT "", /* cli */
                                           INPUT Customer.CustNum,
                                           INPUT {&REQUEST_SOURCE_NEWTON},
                                           INPUT lcNewEmailAdd,
                                           INPUT 0, /* orderid */
                                           OUTPUT lcResult).
          IF liRequest = 0 THEN
             UNDO CUST_UPDATE, RETURN appl_err("Customer email address can not be changed: " +
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

             liRequest = fEmailInvoiceRequest(INPUT Func.Common:mMakeTS(),
                                              INPUT TODAY,
                                              INPUT Syst.Var:katun,
                                              INPUT 0, /* msseq */
                                              INPUT "", /* cli */
                                              INPUT Customer.CustNum,
                                              INPUT {&REQUEST_SOURCE_FUSION_EMAIL},
                                              INPUT lcNewEmailAdd,
                                              INPUT 0, /* msseq */
                                              OUTPUT lcResult).
             IF liRequest = 0 THEN
                UNDO CUST_UPDATE, RETURN appl_err("Customer email address can not be changed: " +
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
    RUN StarEventMakeModifyEventWithMemo(lhCustomer, 
                                         {&STAR_EVENT_USER}, 
                                         lcMemo).
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
      IF ERROR-STATUS:ERROR THEN UNDO CUST_UPDATE, RETURN appl_err("Subscription limit must be integer").

      IF piMobsubLimit < 0 OR piMobsubLimit > 999 THEN 
         UNDO CUST_UPDATE, RETURN appl_err(SUBST("Invalid subscription limit value &1", piMobsubLimit)).
      
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
         UNDO CUST_UPDATE, RETURN appl_err("Subscription activation limit must be integer").

      IF piMobsubActLimit < 0 OR piMobsubActLimit > 999 THEN 
         UNDO CUST_UPDATE, RETURN appl_err(SUBST("Invalid subscription activation limit value &1",
                               piMobsubActLimit)).
      
      /* Get original Subs. limit */
      piMobsubLimit = fGetMobsubLimit(INPUT Customer.Custnum,
                                      INPUT Customer.Category,
                                      OUTPUT llDefaultSubsLimit).
      IF piMobsubActLimit < piMobsubLimit THEN
         UNDO CUST_UPDATE, RETURN appl_err("Subscription activation limit can not be less " +
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
       Memo.Brand     = Syst.Var:gcBrand
       Memo.HostTable = lcMemoHostTable
       Memo.KeyValue  = STRING(Customer.Custnum)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = Syst.Var:katun
       Memo.CustNum   = Customer.Custnum.
   IF llBankAcctChange THEN
      ASSIGN
         Memo.MemoTitle = "Cambio de cuenta"
         Memo.MemoText  = "Solicitado por el cliente: Nº de " +
                        "cuenta: " + Customer.BankAcct + " --> " +
                        IF lcBankAccount > "" THEN lcBankAccount ELSE
                        "blank".
   ELSE 
      ASSIGN
       Memo.MemoTitle = pcMemoTitle
       Memo.MemoText  = pcMemoContent.
END.
END.

FINALLY:
   EMPTY TEMP-TABLE ttCustomerReport NO-ERROR.
END.

