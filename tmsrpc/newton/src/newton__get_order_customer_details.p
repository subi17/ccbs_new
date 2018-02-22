/**
 * Get OrderCustomer details.
 *
 * @input  custnum;int;mandatory;customer number of the Customer for which OrderCustomer details are seeked.
 * @output ordercustomerroot;struct;root struct for the OrderCustomer details
 * @ordercustomerroot  title;string;title of the order customer
                       first_name;string;first name of the order customer
                       surname1;string;first surname of the order customer
                       surname2;string;second surname of the order customer
                       company;string;company of the order customer
                       street;string;street address of the order customer
                       street_number;string; building number of order customer
                       addtional_address;string; complementary information of address
                       street_code;string;address validation C code
                       zip;string;zip code of the order customer
                       city;string;city of the order customer
                       city_code;string;address validation P code
                       municipality_code;string;address validation M code
                       region;string;region of the order customer
                       country;string;country of the order customer
                       contact_number_fix;string;
                       contact_number_mobile;string;
                       email;string;email of the order customer
                       bank_code;string;bank code of the order customer
                       language;string;language of the order customer
                       nationality;string;nationality of the order customer
                       custnum;int;customer number of the order customer
                       id_type;string;
                       customer_id;string;
                       birthday;date_time;birth day of the order customer
                       sms_yoigo;int;1 if marketing is done with SMS and by Yoigo - 0 otherwise
                       email_yoigo;int;1 if marketing is done with email by Yoigo - 0 otherwise
                       post_yoigo;int;1 if marketing is done with post by Yoigo - 0 otherwise
                       sms_3rd;int;1 if marketing is done with SMS by a 3rd party - 0 otherwise
                       email_3rd;int;1 if marketing is done with email by a 3rd party - 0 otherwise
                       post_3rd;int;1 if marketing is done with post by a 3rd party - 0 otherwise
                       additional_documentation;int; addtional documentation status
                       delivery_address;array;
                       contact_person;array;
 *  @delivery_address  delivery_struct;struct
 *  @contact_person    contact_struct;struct
 *  @mobile_pouser_data  mobile_pouser_data;struct
 *  @fixed_pouser_data   fixed_pouser_data;struct
 *  @delivery_struct   street_code;string;address validation C code
                       city_code;string;address validation P code
                       title;string;title of the person for the delivery
                       first_name;string;first name of person for the delivery
                       surname_1;string;first surname of the person for the delivery
                       surname_2;string;second surname of the person for the delivery
                       street;string;street address for the delivery
                       street_number;string; building number of order customer
                       addtional_address;string; complementary information of address
                       zip;string;zip code for the delivery
                       city;string;city for the delivery
                       region;string;region for the delivery
                       country;string;country for the delivery
 *  @contact_struct  street_code;string;address validation C code
                     city_code;string;address validation P code
                     title;string;title of the contact person
                     first_name;string;first name of the contact person
                     surname_1;string;first surname of the contact person
                     surname_2;string;second surname of the contact person
                     street;string;street address of the contact person
                     street_number;string; building number of order customer
                     addtional_address;string; complementary information of address
                     zip;string;zip code for the contact person
                     city;string;city for the contact person
                     region;string;region for the contact person
                     country;string;country for the contact person
 *  @mobile_pouser_data first_name;string;first name
                        surname_1;string;first surname
                        surname_2;string;second surname
                        customer_id_type;string;person id type
                        customer_id;string;person id
                        company_id;string;company cif id
                        company;string;company name
 *  @fixed_pouser_data first_name;string;first name
                       surname_1;string;first surname
                       surname_2;string;second surname
                       customer_id_type;string;person id type
                       customer_id;string;person id
                       company_id;string;company cif id
                       company;string;company name
 */


{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}
{Func/custfunc.i}

DEFINE VARIABLE piOrderId              AS INTEGER   NO-UNDO. 
DEFINE VARIABLE top_struct            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOrderCustomerArray  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOrderCustomerStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError               AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piOrderId = get_pos_int(param_toplevel_id, "0").

{newton/src/findtenant.i YES OrderCanal Order OrderId piOrderId}

FUNCTION fAddHolderCustomer RETURN LOGICAL
   (INPUT piRowType AS INTEGER):

   DEFINE VARIABLE lcStruct       AS CHARACTER NO-UNDO.

   FIND OrderCustomer WHERE
     OrderCustomer.Brand = "1"         AND
     OrderCustomer.OrderId = piOrderId AND
     OrderCustomer.RowType = piRowType NO-LOCK NO-ERROR.

   IF NOT AVAILABLE OrderCustomer
   THEN RETURN FALSE.

   lcStruct = add_struct(top_struct, IF piRowType = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}
                                     THEN "mobile_pouser_data" ELSE "fixed_pouser_data").

   add_string(lcStruct, "first_name", OrderCustomer.FirstName).
   add_string(lcStruct, "surname1", OrderCustomer.SurName1).
   add_string(lcStruct, "surname2", OrderCustomer.SurName2).

   IF OrderCustomer.CustIdType EQ "CIF"
   THEN DO:
      add_string(lcStruct, "customer_id_type", OrderCustomer.AuthCustIDType).
      add_string(lcStruct, "customer_id", OrderCustomer.AuthCustID).
      add_string(lcStruct, "company_id", OrderCustomer.CustId).
      add_string(lcStruct, "company" , OrderCustomer.Company).
   END.
   ELSE DO:
      add_string(lcStruct, "customer_id_type", OrderCustomer.CustIdType).
      add_string(lcStruct, "customer_id", OrderCustomer.CustId).
   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fAddOrderCustomer RETURN LOGICAL
   (INPUT piRowType AS INTEGER, OUTPUT pcError AS CHARACTER):

   DEFINE VARIABLE lcStruct       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liZipCode      AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE lcLanguage     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcLanguageList AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcStructName   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCategory     AS CHARACTER NO-UNDO. 

   pcError = "".
   
   FIND OrderCustomer WHERE 
     OrderCustomer.Brand = "1"         AND 
     OrderCustomer.OrderId = piOrderId AND
     OrderCustomer.RowType = piRowType NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE OrderCustomer THEN 
   DO:
      pcError = SUBST(
        "OrderCustomer of Customer &1 with RowType &2 was not found",
        piOrderId, piRowType).
      RETURN FALSE.
   END.
   
   lcLanguageList = "es_ES,es_CA,es_EU,es_GA,en".

   IF piRowType <> 1 THEN
   DO:
      CASE piRowType:
         WHEN {&ORDERCUSTOMER_ROWTYPE_DELIVERY}
         THEN lcStructName = "delivery_address".
         WHEN {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}
         THEN lcStructName = "contact_address".
      END CASE.
      lcStruct = add_struct(top_struct, lcStructName).
   END.
   ELSE
      lcStruct = top_struct.

   lcLanguage = "".
   IF INTEGER(OrderCustomer.Language) > 0 THEN
      lcLanguage = ENTRY(INTEGER(OrderCustomer.Language), lcLanguageList).

   add_string(lcStruct, "title"      , OrderCustomer.CustTitle  ). 
   add_string(lcStruct, "first_name" , OrderCustomer.FirstName  ).
   add_string(lcStruct, "surname1"   , OrderCustomer.SurName1   ).
   add_string(lcStruct, "surname2"   , OrderCustomer.SurName2   ).
   add_string(lcStruct, "street_code", OrderCustomer.AddressCodC).
   add_string(lcStruct, "city_code"  , OrderCustomer.AddressCodP).
   add_string(lcStruct, "municipality_code", OrderCustomer.AddressCodM).

   IF piRowType EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
   THEN add_string(lcStruct, "company" , OrderCustomer.Company).

   add_string(lcStruct, "zip"        , OrderCustomer.ZipCode ).
   add_string(lcStruct, "city"       , OrderCustomer.PostOffice ).
   add_string(lcstruct, "region"     , OrderCustomer.Region  ).
   add_string(lcStruct, "country"    , OrderCustomer.Country ).
   
   IF OrderCustomer.Street       EQ "" AND
      OrderCustomer.BuildingNum  EQ "" AND
      OrderCustomer.AddressCompl EQ "" THEN 
     add_string(lcStruct, "street", OrderCustomer.Address ).
   ELSE DO:
     add_string(lcStruct, "street", OrderCustomer.Street ).
     IF OrderCustomer.BuildingNum NE "" THEN 
       add_string(lcStruct, "street_number", OrderCustomer.BuildingNum ).
     IF OrderCustomer.AddressCompl NE "" THEN 
       add_string(lcStruct, "additional_address", OrderCustomer.AddressCompl).
   END.

   add_int(lcStruct,"additional_documentation",OrderCustomer.AdditionalDoc).
     
   IF piRowType EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} THEN
   DO:
      add_string(  lcStruct, "contact_number_fix", OrderCustomer.FixedNumber ). 
      add_string(  lcStruct, "contact_number_mobile", OrderCustomer.MobileNumber ). 
      add_string(  lcStruct, "email"      , OrderCustomer.Email      ).
      add_string(  lcStruct, "bank_code"  , OrderCustomer.BankCode   ).
      add_string(  lcStruct, "language"   , lcLanguage               ). 
      add_string(  lcStruct, "nationality", OrderCustomer.Nationality).
      add_int(     lcStruct, "custnum"    , OrderCustomer.Custnum    ).
      add_string(  lcStruct, "id_type"    , OrderCustomer.CustIdType ).
      add_string(  lcStruct, "customer_id", OrderCustomer.CustId     ).
      add_datetime(lcStruct, "birthday"   , 
                   DATETIME(OrderCustomer.BirthDay,0       )).
      add_int(     lcStruct, "sms_yoigo"  , 
                   INTEGER(OrderCustomer.OperSMSMarketing  )).
      add_int(     lcStruct, "email_yoigo", 
                   INTEGER(OrderCustomer.OperEmailMarketing)).
      add_int(     lcStruct, "post_yoigo" , 
                   INTEGER(OrderCustomer.OperPostMarketing )).
      add_int(     lcStruct, "sms_3rd"    , 
                   INTEGER(OrderCustomer.OutSMSMarketing   )).
      add_int(     lcStruct, "email_3rd"  , 
                   INTEGER(OrderCustomer.OutEmailMarketing )).
      add_int(     lcStruct, "post_3rd"   , 
                   INTEGER(OrderCustomer.OutPostMarketing  )).

      IF OrderCustomer.Category > "" THEN 
      DO:
          FIND FIRST CustCat NO-LOCK WHERE 
                     CustCat.brand    EQ Syst.Var:gcBrand       AND
                     CustCat.category EQ OrderCustomer.Category NO-ERROR.
          IF AVAIL CustCat AND CustCat.Segment > "" THEN   
              add_string(lcStruct, "segment", CustCat.Segment).
          ELSE       
              add_string(lcStruct, "segment",fgetCustSegment(ordercustomer.CustIdType,
                                                             ordercustomer.selfemployed,
                                                             ordercustomer.pro,
                                                             ordercustomer.custid,  /* YDR-2621 */
                                                             OUTPUT lcCategory)).    
      END.
      ELSE
      DO:
          /* i.e., Old orders */
          FIND FIRST CustCat NO-LOCK WHERE 
                     CustCat.brand    EQ Syst.Var:gcBrand       AND
                     CustCat.category EQ Customer.Category NO-ERROR.
          IF AVAIL CustCat THEN   
              add_string(lcStruct, "segment", CustCat.Segment).
      END.

      add_int(     lcStruct, "mark_dont_share_personal_data", 
                   INTEGER(OrderCustomer.DontSharePersData  )).                                                                      
   END.

   RETURN TRUE.
END.

top_struct = add_struct(response_toplevel_id, "").

fAddOrderCustomer({&ORDERCUSTOMER_ROWTYPE_AGREEMENT}, OUTPUT lcError).
IF lcError <> "" THEN
   RETURN appl_err(SUBST("OrderCustomer for order &1 not found", piOrderId)).

fAddOrderCustomer({&ORDERCUSTOMER_ROWTYPE_DELIVERY}, lcError).
fAddOrderCustomer({&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}, lcError).
fAddHolderCustomer({&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}).
fAddHolderCustomer({&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER}).
