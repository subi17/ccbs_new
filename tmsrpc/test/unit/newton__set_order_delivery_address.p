{test_xmlrpc_includes.i}
{unit/checkutils.i}

gcFixtures = "ordercustomer,order".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iOrderId AS INTEGER NO-UNDO. 


DEF VAR lLogOn AS LOGICAL NO-UNDO. 
lLogOn = FALSE.

/* Changed delivery address values */
DEFINE VARIABLE lcChangedTitle AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedFirstName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedSurName1 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedSurName2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedStreet AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedZip AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedRegion AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedCountry AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedCityCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedStreetCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChangedPostOffice AS CHARACTER NO-UNDO. 

/* Original delivery address values */
DEFINE VARIABLE lcOriginalTitle AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalFirstName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalSurName1 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalSurName2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalStreet AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalZip AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalRegion AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalCountry AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalCityCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalStreetCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOriginalPostOffice AS CHARACTER NO-UNDO. 


/* Flags to indicate whether value should change or not */
DEFINE VARIABLE lChangedTitle AS LOGICAL NO-UNDO.  
DEFINE VARIABLE lChangedFirstName AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lChangedSurName1 AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lChangedSurName2 AS LOGICAL NO-UNDO.  
DEFINE VARIABLE lChangedStreet AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lChangedZip AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lChangedRegion AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lChangedCountry AS LOGICAL NO-UNDO.  
DEFINE VARIABLE lChangedCityCode AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lChangedStreetCode AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lChangedPostOffice AS LOGICAL NO-UNDO. 


ASSIGN
   lcChangedTitle = "newtitle"
   lcChangedFirstName = "newfirstname"
   lcChangedSurName1 = "newlastname1"
   lcChangedSurName2 = "newlastname2" 
   lcChangedStreet = "newstreet"
   lcChangedZip    = "09999"
   lcChangedRegion = "newregion"
   lcChangedCountry = "newcountry"
   lcChangedCityCode = "AAAAAAAAA"
   lcChangedStreetCode = "SSSSSSS"
   lcChangedPostOffice = "newcity"

    lChangedTitle  = FALSE  
    lChangedFirstName  = FALSE 
    lChangedSurName1  = FALSE 
    lChangedSurName2  = FALSE  
    lChangedStreet  = FALSE 
    lChangedZip  = FALSE 
    lChangedRegion  = FALSE 
    lChangedCountry  = FALSE  
    lChangedCityCode  = FALSE 
    lChangedStreetCode  = FALSE 
    lChangedPostOffice  = FALSE.



FUNCTION fLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   IF lLogOn THEN MESSAGE pcMsg.
   RETURN TRUE.
END.


/** 
 * This function calls the RPC method and checks the call succeeds.
 */
FUNCTION call_set_order_delivery_address RETURN LOGICAL:
    fLog("before call...").
    run_rpc_method("newton.set_order_delivery_address").
    fLog("end call... ").
    RETURN TRUE.
END FUNCTION.


FUNCTION fSetOriginalDeliveryAddress RETURN LOGICAL:
   CASE iOrderId:
      WHEN 1 THEN 
         /* No original delivery address, defaults taken from 
            OrderCustomer RowType 1 */
         ASSIGN
            lcOriginalTitle = "RowType1Title"
            lcOriginalFirstName = "RowType1FirstName"
            lcOriginalSurName1 = "RowType1SurName1"
            lcOriginalSurName2 = "RowType1SurName2"
            lcOriginalStreet = "RowType1Street"
            lcOriginalZip    = "31000"
            lcOriginalRegion = "RowType1Region"
            lcOriginalCountry = "RowType1Country"
            lcOriginalCityCode = "RowType1CodP"
            lcOriginalStreetCode = "RowType1CodC"
            lcOriginalPostOffice = "RowType1City".

      WHEN 2 THEN 
         /* Original delivery address exists, defaults taken from 
            OrderCustomer RowType 4 */

         ASSIGN
            lcOriginalTitle = "RowType4Title"
            lcOriginalFirstName = "RowType4FirstName"
            lcOriginalSurName1 = "RowType4SurName1"
            lcOriginalSurName2 = "RowType4SurName2"
            lcOriginalStreet = "RowType4Street"
            lcOriginalZip    = "34000"
            lcOriginalRegion = "RowType4Region"
            lcOriginalCountry = "RowType4Country"
            lcOriginalCityCode = "RowType4CodP"
            lcOriginalStreetCode = "RowType4CodC"
            lcOriginalPostOffice = "RowType4City".
  END.


  fLog("original city = " + lcOriginalPostOffice).
  fLog("changed city = " + lcChangedPostOffice).


  RETURN TRUE.
END.
   

FUNCTION fAddParam RETURN LOGICAL (
     INPUT pcDeliveryAddressStruct AS CHARACTER,
     INPUT pcParamName AS CHARACTER,
     INPUT plChanged AS LOGICAL):

     DEFINE VARIABLE cSetValue AS CHARACTER NO-UNDO. 

     IF plChanged THEN
        /* Add changed value and set changed flag to TRUE */
        CASE pcParamName:
           WHEN "title" THEN 
           DO:
              cSetValue = lcChangedTitle.
              lChangedTitle = TRUE.
           END.

           WHEN "first_name" THEN 
           DO:
              cSetValue = lcChangedFirstName.
              lChangedFirstName = TRUE.
           END.

           WHEN "surname_1" THEN 
           DO:
              cSetValue = lcChangedSurName1.
              lChangedSurName1 = TRUE.
           END.

           WHEN "surname_2" THEN 
           DO:
              cSetValue = lcChangedSurName2.
              lChangedSurName2 = TRUE.
           END.

           WHEN "street" THEN 
           DO:
              cSetValue = lcChangedStreet.
              lChangedStreet = TRUE.
           END.

           WHEN "zip" THEN 
           DO:
              cSetValue = lcChangedZip.
              lChangedZip = TRUE.
           END.

           WHEN "region" THEN 
           DO:
              cSetValue = lcChangedRegion.
              lChangedRegion = TRUE.
           END.

           WHEN "country" THEN 
           DO:
              cSetValue = lcChangedCountry.
              lChangedCountry = TRUE.
           END.

           WHEN "city_code" THEN 
           DO:
              cSetValue = lcChangedCityCode.
              lChangedCityCode = TRUE.
           END.

           WHEN "street_code" THEN 
           DO:
              cSetValue = lcChangedStreetCode.
              lChangedStreetCode = TRUE.
           END.

           WHEN "city" THEN 
           DO:
              cSetValue = lcChangedPostOffice.
              lChangedPostOffice = TRUE.
              fLog("set changed city: " + cSetValue).
           END.
     END.
  ELSE /* Not plChanged, add original value and changed flag to FALSE */
        CASE pcParamName:
           WHEN "title" THEN 
           DO:
              cSetValue = lcOriginalTitle.
              lChangedTitle = FALSE.
           END.

           WHEN "first_name" THEN 
           DO:
              cSetValue = lcOriginalFirstName.
              lChangedFirstName = FALSE.
           END.

           WHEN "surname_1" THEN 
           DO:
              cSetValue = lcOriginalSurName1.
              lChangedSurName1 = FALSE.
           END.

           WHEN "surname_2" THEN 
           DO:
              cSetValue = lcOriginalSurName2.
              lChangedSurName2 = FALSE.
           END.

           WHEN "street" THEN 
           DO:
              cSetValue = lcOriginalStreet.
              lChangedStreet = FALSE.
           END.

           WHEN "zip" THEN 
           DO:
              cSetValue = lcOriginalZip.
              lChangedZip = FALSE.
           END.

           WHEN "region" THEN 
           DO:
              cSetValue = lcOriginalRegion.
              lChangedRegion = FALSE.
           END.

           WHEN "country" THEN 
           DO:
              cSetValue = lcOriginalCountry.
              lChangedCountry = FALSE.
           END.

           WHEN "city_code" THEN 
           DO:
              cSetValue = lcOriginalCityCode.
              lChangedCityCode = FALSE.
           END.

           WHEN "street_code" THEN 
           DO:
              cSetValue = lcOriginalStreetCode.
              lChangedStreetCode = FALSE.
           END.

           WHEN "city" THEN 
           DO:
              cSetValue = lcOriginalPostOffice.
              fLog("set original city " + cSetValue).
              lChangedPostOffice = FALSE.
           END.
      END.
      add_string(pcDeliveryAddressStruct, pcParamName, cSetValue).

      fLog("Parameter " + pcParamName + " has value " + cSetValue).

      RETURN TRUE.
END.

FUNCTION fCheckField RETURN LOGICAL
  (INPUT pcFieldName AS CHARACTER,
   INPUT pcDbValue AS CHARACTER,
   INPUT pcChangedValue AS CHARACTER,
   INPUT pcOriginalValue AS CHARACTER,
   INPUT plChanged AS LOGICAL):

/*  IF pcFieldName = "OrderCustomer.PostOffice" THEN
  DO: */
  fLog("pcFieldName: " + pcFieldName ).
  fLog("pcDbValue: " + pcDbValue ).
  fLog("pcChangedValue : " + pcChangedValue ).
  fLog("pcOriginalValue : " + pcOriginalValue ).
  flog("plChanged : " + STRING(plChanged)).
/*  END. */

  IF plChanged THEN
     checkChar(pcFieldName, pcDbValue, pcChangedValue).
  ELSE
     checkChar(pcFieldName, pcDbValue, pcOriginalValue).
   
  RETURN TRUE.

END.


FUNCTION fCheckDeliveryAddressField RETURN LOGICAL
   (INPUT pcParamName AS CHARACTER):
     
   CASE pcParamName:
     WHEN "title" THEN 
         fCheckField("OrderCustomer.Title", 
            OrderCustomer.CustTitle,
            lcChangedTitle, lcOriginalTitle, lChangedTitle).

     WHEN "first_name" THEN 
         fCheckField("OrderCustomer.Title", 
            OrderCustomer.FirstName,
            lcChangedFirstName, lcOriginalFirstName, lChangedFirstName).

     WHEN "surname_1" THEN 
         fCheckField("OrderCustomer.SurName1", 
              OrderCustomer.SurName1, 
              lcChangedSurName1, lcOriginalSurName1, lChangedSurName1).

     WHEN "surname_2" THEN 
         fCheckField("OrderCustomer.SurName2", 
              OrderCustomer.SurName2, 
              lcChangedSurName2, lcOriginalSurName2, lChangedSurName2).

     WHEN "street" THEN 
         fCheckField("OrderCustomer.Address", 
              OrderCustomer.Address, 
              lcChangedStreet, lcOriginalStreet, lChangedStreet).

     WHEN "zip" THEN 
         fCheckField("OrderCustomer.ZipCode", 
              OrderCustomer.ZipCode, 
              lcChangedZip, lcOriginalZip, lChangedZip).

     WHEN "region" THEN 
         fCheckField("OrderCustomer.Region", 
              OrderCustomer.Region, 
              lcChangedRegion, lcOriginalRegion, lChangedRegion).

     WHEN "country" THEN 
         fCheckField("OrderCustomer.Country", 
              OrderCustomer.Country, 
              lcChangedCountry, lcOriginalCountry, lChangedCountry).

     WHEN "city_code" THEN 
         fCheckField("OrderCustomer.AddressCodP", 
              OrderCustomer.AddressCodP, 
              lcChangedCityCode, lcOriginalCityCode, lChangedCityCode).

     WHEN "street_code" THEN 
         fCheckField("OrderCustomer.AddressCodC", 
              OrderCustomer.AddressCodC, 
              lcChangedStreetCode, lcOriginalStreetCode, lChangedStreetCode).

     WHEN "city" THEN 
         fCheckField("OrderCustomer.PostOffice", 
              OrderCustomer.PostOffice, 
              lcChangedPostOffice, lcOriginalPostOffice, lChangedPostOffice).
   END.

END.



FUNCTION fCheckDeliveryAddress RETURN LOGICAL
   (INPUT pcOrderCustomerFixture AS CHARACTER):

   IF pcOrderCustomerFixture <> "" THEN
      fetch_fixture(pcOrderCustomerFixture, BUFFER OrderCustomer:HANDLE).
   ELSE
   DO:
      FIND FIRST OrderCustomer WHERE OrderCustomer.Brand = "1" AND
         OrderCustomer.OrderId = iOrderId AND OrderCustomer.RowType = 4 
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE OrderCustomer THEN
         assert(TRUE, 
            "Delivery address was not created based on rowtype 1 OrderCustomer").
   END.


   fLog("OrderCustomer.PostOffice" + OrderCustomer.PostOffice).

   fCheckDeliveryAddressField("title").
   fCheckDeliveryAddressField("first_name").
   fCheckDeliveryAddressField("surname_1").
   fCheckDeliveryAddressField("surname_2").
   fCheckDeliveryAddressField("street").
   fCheckDeliveryAddressField("zip").
   fCheckDeliveryAddressField("region").
   fCheckDeliveryAddressField("country").
   fCheckDeliveryAddressField("city_code").
   fCheckDeliveryAddressField("street_code").
   fCheckDeliveryAddressField("city").

   RETURN TRUE.



END.



FUNCTION fAddMinParameters RETURN LOGICAL
   (INPUT pcActor AS CHARACTER,
   INPUT plChangedStreet AS LOGICAL,
   INPUT plChangedZip AS LOGICAL,
   INPUT plChangedRegion AS LOGICAL,
   INPUT plChangedPostOffice AS LOGICAL): 

   DEFINE VARIABLE lcDeliveryAddressStruct AS CHARACTER NO-UNDO. 

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", pcActor).
   lcDeliveryAddressStruct = add_struct(gcParamArray, "").
   fAddParam(lcDeliveryAddressStruct, "street", plChangedStreet).
   fAddParam(lcDeliveryAddressStruct, "zip", plChangedZip).
   fAddParam(lcDeliveryAddressStruct, "region", plChangedRegion).
   fAddParam(lcDeliveryAddressStruct, "city", plChangedPostOffice).

   RETURN TRUE.
END.



FUNCTION fAddSomeParameters RETURN LOGICAL
   (INPUT pcActor AS CHARACTER, 
    INPUT plChangedFirstName AS LOGICAL, 
    INPUT plChangedStreet AS LOGICAL, 
    INPUT plChangedZip AS LOGICAL, 
    INPUT plChangedRegion AS LOGICAL, 
    INPUT plChangedCityCode AS LOGICAL, 
    INPUT plChangedStreetCode AS LOGICAL, 
    INPUT plChangedPostOffice AS LOGICAL): 


   DEFINE VARIABLE lcDeliveryAddressStruct AS CHARACTER NO-UNDO. 

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", pcActor).
   lcDeliveryAddressStruct = add_struct(gcParamArray, "").

   /* One additional parameter, not first */
   fAddParam(lcDeliveryAddressStruct, "first_name", plChangedFirstName).

   /* Mandatary parameters */
   fAddParam(lcDeliveryAddressStruct, "street", plChangedStreet).
   faddParam(lcDeliveryAddressStruct, "zip", plChangedZip).
   fAddParam(lcDeliveryAddressStruct, "region", plChangedRegion).
   
   /* Some more additional parameters, not all rest of them */
   fAddParam(lcDeliveryAddressStruct, "city_code", plChangedCityCode).
   fAddParam(lcDeliveryAddressStruct, "street_code", plChangedStreetCode).
   fAddParam(lcDeliveryAddressStruct, "city", plChangedPostOffice).

   RETURN TRUE.
END.




FUNCTION fAddAllParameters RETURN LOGICAL
   (INPUT pcActor AS CHARACTER,
    INPUT plChangedTitle AS LOGICAL,
    INPUT plChangedFirstName AS LOGICAL,
    INPUT plChangedSurname1 AS LOGICAL, 
    INPUT plChangedSurname2 AS LOGICAL, 
    INPUT plChangedStreet AS LOGICAL, 
    INPUT plChangedZip AS LOGICAL, 
    INPUT plChangedRegion AS LOGICAL, 
    INPUT plChangedCountry AS LOGICAL, 
    INPUT plChangedCityCode AS LOGICAL, 
    INPUT plChangedStreetCode AS LOGICAL, 
    INPUT plChangedPostOffice AS LOGICAL):

   DEFINE VARIABLE lcDeliveryAddressStruct AS CHARACTER NO-UNDO. 

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", pcActor).
   lcDeliveryAddressStruct = add_struct(gcParamArray, "").

   fAddParam(lcDeliveryAddressStruct, "title", plChangedTitle).
   fAddParam(lcDeliveryAddressStruct, "first_name", plChangedFirstName).
   fAddParam(lcDeliveryAddressStruct, "surname_1", plChangedSurName1).
   fAddParam(lcDeliveryAddressStruct, "surname_2", plChangedSurName2).
   fAddParam(lcDeliveryAddressStruct, "street", plChangedStreet).
   fAddParam(lcDeliveryAddressStruct, "zip", plChangedZip).
   fAddParam(lcDeliveryAddressStruct, "region", plChangedRegion).
   fAddParam(lcDeliveryAddressStruct, "country", plChangedCountry).
   fAddParam(lcDeliveryAddressStruct, "city_code", plChangedCityCode).
   fAddParam(lcDeliveryAddressStruct, "street_code", plChangedStreetCode).
   fAddParam(lcDeliveryAddressStruct, "city", plChangedPostOffice). 

   RETURN TRUE.

END.


PROCEDURE test_order_with_minimum_parameters_and_changes:
  fLog("----------------------------------------------------------").
  fLog("test_order_with_minimum_parameters_and_changes").

  iOrderId = 2.

  fSetOriginalDeliveryAddress().
  fAddMinParameters("newuser",
      TRUE, FALSE, TRUE, TRUE). /* street, zip, region, city */

  call_set_order_delivery_address().
  assert_success().

  fCheckDeliveryAddress("OrderCustomerDetail2RowType4").
END.


PROCEDURE test_order_with_some_parameters_and_changes:
  fLog("----------------------------------------------------------").
  fLog("test_order_with_some_parameters_and_changes").

  iOrderId = 2.

  fSetOriginalDeliveryAddress().
  fAddSomeParameters("newuser",
     TRUE, TRUE, FALSE, /* first_name, street, zip */
     FALSE, TRUE, FALSE, /* region, city_code, street_code */
     TRUE). /* city */

  call_set_order_delivery_address().
  assert_success().

  fCheckDeliveryAddress("OrderCustomerDetail2RowType4").
END.


PROCEDURE test_order_with_existing_delivery_address_and_all_changed:
  fLog("----------------------------------------------------------").
  fLog("test_order_with_existing_delivery_address_and_all_changed").

  iOrderId = 2.
  fSetOriginalDeliveryAddress().

  fAddAllParameters("newuser",
     TRUE, TRUE, /* title, first name */
     TRUE, TRUE, /* surname1, surname2  */
     TRUE, TRUE, /* street, zip */
     TRUE, TRUE, /* region, country */
     TRUE, TRUE, /* city_code, street_code */
     TRUE).      /* city */

  call_set_order_delivery_address().
  assert_success().

  fCheckDeliveryAddress("OrderCustomerDetail2RowType4").
END.


PROCEDURE test_order_with_existing_delivery_address_and_without_changes:
  fLog("----------------------------------------------------------").
  fLog("test_order_with_existing_delivery_address_and_without_changes").

  iOrderId = 2.
  fSetOriginalDeliveryAddress().

  fAddAllParameters("newuser",
     FALSE, FALSE, /* title, first name */
     FALSE, FALSE, /* surname1, surname2  */
     FALSE, FALSE, /* street, zip */
     FALSE, FALSE, /* region, country */
     FALSE, FALSE, /* city_code, street_code */
     FALSE).      /* city */

  call_set_order_delivery_address().
  assert_success().

  fCheckDeliveryAddress("OrderCustomerDetail2RowType4").
END.

PROCEDURE test_order_without_delivery_address_and_without_changes:
  fLog("----------------------------------------------------------").
  fLog("test_order_without_delivery_address_and_without_changes").
  iOrderId = 1.
  fSetOriginalDeliveryAddress().
  
  fAddAllParameters("newuser",
     FALSE, FALSE, /* title, first name */
     FALSE, FALSE, /* surname1, surname2  */
     FALSE, FALSE, /* street, zip */
     FALSE, FALSE, /* region, country */
     FALSE, FALSE, /* city_code, street_code */
     FALSE).      /* city */

  call_set_order_delivery_address().
  assert_success().

  fCheckDeliveryAddress("").
END.


PROCEDURE test_order_without_delivery_address_and_with_all_changed:
  fLog("----------------------------------------------------------").
  fLog("test_order_without_delivery_address_and_with_all_changed").

  iOrderId = 1.
  fSetOriginalDeliveryAddress().
 

  fAddAllParameters("newuser",
     TRUE, TRUE, /* title, first name */
     TRUE, TRUE, /* surname1, surname2  */
     TRUE, TRUE, /* street, zip */
     TRUE, TRUE, /* region, country */
     TRUE, TRUE, /* city_code, street_code */
     TRUE).      /* city */

  call_set_order_delivery_address().
  assert_success().

  fCheckDeliveryAddress("").
END.






