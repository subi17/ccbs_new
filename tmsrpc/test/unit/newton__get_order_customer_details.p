
/**
 * This is the test set for the corresponding newton__get_order_customer_details
 * XML RPC method.
 *
 * The fixture usage of this test set:
 *
 * - fixtures fetched: 
 *
 * - The RPC method is assumed to fetch in addition following records:
 *
 *
 * - restrictions for building other fixtures: 
 */

{test_xmlrpc_includes.i}
{unit/checkutils.i}


gcFixtures = "ordercustomer".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcDeliveryStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcContactStruct  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iOrderId AS INTEGER NO-UNDO. 


/* OrderCustomer fields as variables */
DEFINE VARIABLE lcAddress            AS CHARACTER NO-UNDO.


DEFINE VARIABLE lcAddressCodC        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcAddressCodP        AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcBankCode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCompany            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCountry            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCustId             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCustIdType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCustTitle          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEmail              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFirstName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLanguage           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcExpectedLanguage   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcNationality        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPostOffice         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRegion             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSurName1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSurName2           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcZipCode            AS CHARACTER NO-UNDO.
DEFINE VARIABLE liOperEMailMarketing AS INTEGER NO-UNDO.
DEFINE VARIABLE liOperPostMarketing  AS INTEGER NO-UNDO.
DEFINE VARIABLE liOperSMSMarketing   AS INTEGER NO-UNDO.
DEFINE VARIABLE liOutEMailMarketing  AS INTEGER NO-UNDO.
DEFINE VARIABLE liOutPostMarketing   AS INTEGER NO-UNDO.
DEFINE VARIABLE liOutSMSMarketing    AS INTEGER NO-UNDO.
DEFINE VARIABLE liOrderId            AS INTEGER NO-UNDO.
DEFINE VARIABLE ldtBirthDay          AS DATE NO-UNDO.
DEFINE VARIABLE lcFixedNumber     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMobileNumber    AS CHARACTER NO-UNDO. 



DEF VAR lLogOn AS LOGICAL NO-UNDO. 
lLogOn = FALSE.

FUNCTION fLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   IF lLogOn THEN MESSAGE pcMsg.
   RETURN TRUE.
END.



/** 
 * This function calls the RPC method and checks the call succeeds.
 */
FUNCTION call_get_order_customer_details RETURN LOGICAL:
    fLog("before call...").
    run_rpc_method("newton.get_order_customer_details").

    fLog("giXMLRpcError: " + STRING(gi_xmlrpc_error) + 
         ",gcXMLRPCError: " + gc_xmlrpc_error).
    fLog("end call...").

    RETURN TRUE.
END FUNCTION.


FUNCTION fGetOrderCustomerFieldsToVars RETURN LOGICAL
   (INPUT pcStruct AS CHARACTER):

    fLog("GetVars begin").

    lcAddress            = get_string(pcStruct, "street"     ). 
    lcAddressCodC        = get_string(pcStruct, "street_code" ).
    lcAddressCodP        = get_string(pcStruct, "city_code"  ).
    lcBankCode           = get_string(pcStruct, "bank_code"  ). 
    ldtBirthDay          = get_date(  pcStruct, "birthday"   ).
    lcCompany            = get_string(pcStruct, "company"    ).
    lcCountry            = get_string(pcStruct, "country"    ).
    lcCustId             = get_string(pcStruct, "customer_id"). 
    lcCustIdType         = get_string(pcStruct, "id_type"    ). 
    lcCustTitle          = get_string(pcStruct, "title"      ).
    lcEmail              = get_string(pcStruct, "email"      ). 
    lcFirstName          = get_string(pcStruct, "first_name" ). 
    lcLanguage           = get_string(pcStruct, "language"   ). 
    lcNationality        = get_string(pcStruct, "nationality"). 
    liOperEMailMarketing = get_int(   pcStruct, "email_yoigo"). 
    liOperPostMarketing  = get_int(   pcStruct, "post_yoigo" ).
    liOperSMSMarketing   = get_int(   pcStruct, "sms_yoigo"  ).
    liOutEMailMarketing  = get_int(   pcStruct, "email_3rd"  ).
    liOutPostMarketing   = get_int(   pcStruct, "post_3rd"   ).
    liOutSMSMarketing    = get_int(   pcStruct, "sms_3rd"    ).
    lcPostOffice         = get_string(pcStruct, "city"       ). 
    lcRegion             = get_string(pcStruct, "region"     ). 
    lcSurName1           = get_string(pcStruct, "surname1"   ). 
    lcSurName2           = get_string(pcStruct, "surname2"   ).
    lcZipCode            = get_string(   pcStruct, "zip"     ). 

    fLog("GetVars end").
END.

FUNCTION fCheckOrderCustomer RETURN LOGICAL (
    INPUT pcStruct AS CHARACTER,
    INPUT pcFixture AS CHARACTER,
    INPUT piRowType AS INTEGER):

   fLog("fCheckOrderCustomer begin").
   fLog("pcStruct = " + pcStruct ).
   fLog("pcFixture = " + pcFixture).
   fLog("piRowType = " + STRING(piRowType)).

   fetch_fixture(pcFixture, BUFFER OrderCustomer:HANDLE).
   
   fLog("fetch_fixture passed").

   fGetOrderCustomerFieldsToVars(pcStruct).

   fLog("Get vars passed.").


   checkChar( "OrderCustomer.CustTitle"   ,OrderCustomer.CustTitle, 
              lcCustTitle ).

   fLog("Custtitle passed").

   checkChar( "OrderCustomer.FirstName"   ,OrderCustomer.FirstName, 
              lcFirstName ).

   fLog("SurName1 passed").

   checkChar( "OrderCustomer.SurName1"    ,OrderCustomer.SurName1 , 
              lcSurName1 ).

   fLog("Surname2 passed").

   checkChar( "OrderCustomer.SurName2"    ,OrderCustomer.SurName2 , 
              lcSurName2 ).

   fLog("SurName2 passed").

   checkChar( "OrderCustomer.city_code"   ,OrderCustomer.AddressCodP, 
              lcAddressCodP ).
   checkChar( "OrderCustomer.street_code" ,OrderCustomer.AddressCodC, 
              lcAddressCodC ).


   IF piRowType = 1 THEN
      checkChar( "OrderCustomer.Company" ,OrderCustomer.Company, lcCompany ).

   checkChar( "OrderCustomer.Address"    ,OrderCustomer.Address, lcAddress ).
   checkChar(  "OrderCustomer.ZipCode"    ,
             OrderCustomer.ZipCode , lcZipCode ).
   checkChar( "OrderCustomer.PostOffice" ,
             OrderCustomer.PostOffice, lcPostOffice ).
   checkChar( "OrderCustomer.Country"    ,OrderCustomer.Country, lcCountry ).

   IF piRowType = 1 THEN
   DO:
      checkChar( "OrderCustomer.Email"       ,OrderCustomer.Email      , 
                 lcEmail       ).
      checkChar( "OrderCustomer.BankCode"    ,OrderCustomer.BankCode   , 
                 lcBankCode    ).

      CASE OrderCustomer.Language:
         WHEN "1" THEN lcExpectedLanguage = "es_ES".
         WHEN "2" THEN lcExpectedLanguage = "es_CA".
         WHEN "3" THEN lcExpectedLanguage = "es_EU".
         WHEN "4" THEN lcExpectedLanguage = "es_GA".
         WHEN "5" THEN lcExpectedLanguage = "en".
      END.

      checkChar( "OrderCustomer.Language"    ,lcExpectedLanguage   , 
                 lcLanguage    ).

      checkChar( "OrderCustomer.Nationality" ,OrderCustomer.Nationality, 
                 lcNationality ).

      checkChar( "OrderCustomer.CustIdType"  ,OrderCustomer.CustIdType, 
                 lcCustIdType ).
      checkChar( "OrderCustomer.CustId"      ,OrderCustomer.CustId    , 
                 lcCustId ).

      checkInt( "OrderCustomer.OperSMSMarketing", 
                INTEGER(OrderCustomer.OperSMSMarketing), 
                liOperSMSMarketing).
      checkInt( "OrderCustomer.OperEmailMarketing", 
                INTEGER(OrderCustomer.OperEmailMarketing), 
                liOperEmailMarketing).
      checkInt( "OrderCustomer.OperPostMarketing", 
                INTEGER(OrderCustomer.OperPostMarketing),  
                liOperPostMarketing).

      /* check contact_number_fix */
      /* check contact_number_mobile */
      /* check custnum */
      /* check birthday */
  
      checkInt( "OrderCustomer.OutSMSMarketing", 
                INTEGER(OrderCustomer.OutSMSMarketing)  , liOutSMSMarketing  ).
      checkInt( "OrderCustomer.OutEmailMarketing", 
                INTEGER(OrderCustomer.OutEmailMarketing), liOutEmailMarketing).
      checkInt( "OrderCustomer.OutPostMarketing", 
                INTEGER(OrderCustomer.OutPostMarketing) , liOutPostMarketing ).
    END.

    fLog("fCheckOrderCustomer end").

    RETURN TRUE.
END.



FUNCTION fCheckData RETURN LOGICAL (INPUT piOrderId AS INTEGER):
   
   gcReturnStruct = get_struct("", "").
   
   fCheckOrderCustomer(gcReturnStruct, 
                       "OrderCustomerDetail" + STRING(iOrderId), 1).

   /* Check delivery and contact addresses */
   IF iOrderId > 1 THEN
      CASE iOrderId:

         WHEN 2 THEN /* Delivery struct, no contact struct */
         DO:
              gcDeliveryStruct = get_struct(gcReturnStruct, "delivery_address").
              fCheckOrderCustomer(gcDeliveryStruct, 
                  "OrderCustomerDetail2RowType4", 4).
         END.

         WHEN 3 THEN /* No delivery struct and contact struct */
         DO:
             gcContactStruct = get_struct(gcReturnStruct, "contact_address").
             fCheckOrderCustomer(gcContactStruct, 
                  "OrderCustomerDetail3RowType5", 5).
         END.


         WHEN 4 THEN /* delivery and contact struct */
         DO:
              gcDeliveryStruct = get_struct(gcReturnStruct, "delivery_address").
              fCheckOrderCustomer(gcDeliveryStruct, 
                  "OrderCustomerDetail4RowType4", 4).

              gcContactStruct = get_struct(gcReturnStruct, "contact_address").
              fCheckOrderCustomer(gcContactStruct, 
                  "OrderCustomerDetail4RowType5", 5).
         END.
      END.
   RETURN TRUE.
END.


PROCEDURE test_get_order_customer_details_with_rowtype1_ordercustomer_only:
   iOrderId = 1.
   fLog("before adding param").
   add_int(gcParamArray, "", iOrderId).
   fLog("after adding param").
   call_get_order_customer_details().
   assert_success().
   fCheckData(iOrderId).
END.



PROCEDURE test_get_order_customer_details_without_contact_address:
   iOrderId = 2.
   add_int(gcParamArray, "", iOrderId).
   call_get_order_customer_details().
   assert_success().
   fCheckData(iOrderId).
END.


PROCEDURE test_get_order_customer_details_without_delivery_address:
   iOrderId = 3.
   add_int(gcParamArray, "", iOrderId).
   call_get_order_customer_details().
   assert_success().
   fCheckData(iOrderId).
END.



PROCEDURE test_get_order_customer_details_with_all_addresses:
   iOrderId = 4.
   add_int(gcParamArray, "", iOrderId).
   call_get_order_customer_details().
   assert_success().
   fCheckData(iOrderId).
END.
