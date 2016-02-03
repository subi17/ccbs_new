/**
 * This is the test set for the corresponding newton__set_customer_address
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
{Func/timestamp.i}

gcFixtures = "customer".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcParamStruct AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 


FUNCTION call_set_customer_details RETURN LOGICAL:
    run_rpc_method("newton.set_customer_details").
    RETURN TRUE.
END FUNCTION.



DEFINE VARIABLE piCustNum   AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcSalesman  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE plCreateFee AS LOGICAL NO-UNDO. 

DEFINE VARIABLE pcConame     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStreet     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcZip        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCity       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcRegion     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCountry    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCityCode   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStreetCode AS CHARACTER NO-UNDO. 

DEFINE VARIABLE pcTitle       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcLname       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcLname2      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcFname       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcLanguage    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcNationality AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcBankAccount AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcEmail       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcSMSNumber   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcPhoneNumber AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcPersonId    AS CHARACTER NO-UNDO. 

DEFINE VARIABLE plMarkSMS      AS LOGICAL NO-UNDO. 
DEFINE VARIABLE plMarkEmail    AS LOGICAL NO-UNDO. 
DEFINE VARIABLE plMarkPost     AS LOGICAL NO-UNDO. 
DEFINE VARIABLE plMarkSMS3rd   AS LOGICAL NO-UNDO. 
DEFINE VARIABLE plMarkEmail3rd AS LOGICAL NO-UNDO. 
DEFINE VARIABLE plMarkPost3rd  AS LOGICAL NO-UNDO. 

&GLOBAL-DEFINE languages "es_ES,es_CA,es_EU,es_GA,en"

FUNCTION fFillParamData RETURN LOGICAL:
   piCustNum    = 34. 
   pcSalesman   = "salesman".
   plCreateFee  = FALSE.
   pcConame     = "coname".
   pcStreet     = "street sdas".
   pcZip        = "23772".
   pcCity       = "Madrid".
   pcRegion     = "43".
   pcCountry    = "Spain".
   pcCityCode   = "324".
   pcStreetCode = "4234989".

   pcTitle      = "title".
   pcLname      = "lname".
   pcLname2     = "lname2".
   pcFname      = "fname".
   pcLanguage   = "es_ES".
   pcNationality = "".
   pcBankAccount = "23123212".
   pcEmail       = "hhh.sss@gmail.com".
   pcSMSNumber   = "12312322".
   pcPhoneNumber = "45443122".
   pcPersonId    = "ffg23123".

   plMarkSMS     = TRUE.
   plMarkEmail   = FALSE.
   plMarkPost    = TRUE.
   plMarkSMS3rd   = FALSE.
   plMarkEmail3rd = TRUE.
   plMarkPost3rd  = FALSE.

   RETURN TRUE.
END.

DEFINE VARIABLE lcDataCFields AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDataLFields AS CHARACTER NO-UNDO. . 
DEFINE VARIABLE lDataCDefault AS LOGICAL EXTENT 19 NO-UNDO. 
DEFINE VARIABLE lDataLDefault AS LOGICAL EXTENT 6 NO-UNDO.


lcDataCFields = "title,lname,lname2,fname,coname,street,zip,city,region," +
                "language,nationality,bankaccount,country," +
                "email,sms_number,phone_number,person_id,city_code,street_code".

lcDataLFields =  "mark_sms,mark_email,mark_post," +
                 "mark_sms_3rd,mark_email_3rd,mark_post_3rd".


FUNCTION fAddStringIfNotDefault RETURN LOGICAL 
   (INPUT pcFieldName AS CHARACTER, INPUT pcFieldValue AS CHARACTER):
   IF NOT lDataCDefault[LOOKUP(pcFieldName, lcDataCFields)] THEN
      add_string(gcParamStruct, pcFieldName, pcFieldValue).
   RETURN TRUE.
END.

FUNCTION fAddLogicalIfNotDefault RETURN LOGICAL
   (INPUT pcFieldName AS CHARACTER, INPUT plFieldValue AS LOGICAL):
   IF NOT lDatalDefault[LOOKUP(pcFieldName, lcDataLFields)] THEN
      add_boolean(gcParamStruct, pcFieldName, plFieldValue).
   RETURN TRUE.
END.

FUNCTION fSetDefaultFields RETURN LOGICAL 
     (INPUT pcDefaultFields AS CHARACTER,
     INPUT pcType AS CHARACTER):
   DEFINE VARIABLE iField AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iFieldCount AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cWholeFieldList AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cField AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lValue AS LOGICAL NO-UNDO. 
   CASE pcType:
      WHEN "CHARACTER" THEN cWholeFieldList = lcDataCFields.
      WHEN "LOGICAL"   THEN cWholeFieldList = lcDataLFields.
   END.
   iFieldCount = NUM-ENTRIES(cWholeFieldList).

   REPEAT iField = 1 TO iFieldCount:
       cField = ENTRY(iField, cWholeFieldList).
       IF LOOKUP(cField, pcDefaultFields) > 0 THEN
          lValue = TRUE.
       ELSE 
          lValue = FALSE.
       CASE pcType:
          WHEN "CHARACTER" THEN lDataCDefault[iField] = lValue.
          WHEN "LOGICAL"   THEN lDataLDefault[iField] = lValue.
       END.
   END.
   RETURN TRUE.
END.


FUNCTION fAddDataParams RETURN LOGICAL:
   add_int(gcParamArray, "", piCustNum).
   add_string(gcParamArray, "", pcSalesman).
   add_boolean(gcParamArray, "", plCreateFee).
   
   gcParamStruct = add_struct(gcParamArray, "").
   fAddStringIfNotDefault("coname"     , pcConame).
   fAddStringIfNotDefault("street"     , pcStreet).
   fAddStringIfNotDefault("zip"        , pcZip).
   fAddStringIfNotDefault("city"       , pcCity).
   fAddStringIfNotDefault("region"     , pcRegion).
   fAddStringIfNotDefault("country"    , pcCountry).
   fAddStringIfNotDefault("city_code"  , pcCityCode).
   fAddStringIfNotDefault("street_code", pcStreetCode).

   fAddStringIfNotDefault("title"       , pcTitle).
   fAddStringIfNotDefault("lname"       , pcLname).
   fAddStringIfNotDefault("lname2"      , pcLname2).
   fAddStringIfNotDefault("fname"       , pcFname).
   fAddStringIfNotDefault("language"    , pcLanguage).
   fAddStringIfNotDefault("nationality" , pcNationality).
   fAddStringIfNotDefault("bankaccount" , pcBankAccount).
   fAddStringIfNotDefault("email"       , pcEmail).
   fAddStringIfNotDefault("sms_number"  , pcSMSNumber).
   fAddStringIfNotDefault("phone_number", pcPhoneNumber).
   fAddStringIfNotDefault("person_id"   , pcPersonId).

   fAddLogicalIfNotDefault("mark_sms"      , plMarkSMS).
   fAddLogicalIfNotDefault("mark_email"    , plMarkEmail).
   fAddLogicalIfNotDefault("mark_post"     , plMarkPost).
   fAddLogicalIfNotDefault("mark_sms_3rd"  , plMarkSMS3rd).
   fAddLogicalIfNotDefault("mark_email_3rd", plMarkEmail3rd).
   fAddLogicalIfNotDefault("mark_post_3rd" , plMarkPost3rd).
   /*gcParamStruct = add_struct(gcParamArray, "").*/
   RETURN TRUE.
END.

FUNCTION fGetExpectedCData RETURN CHARACTER 
   (INPUT pcFieldName AS CHARACTER):
  DEFINE VARIABLE cVal AS CHARACTER NO-UNDO. 

  IF NOT lDataCDefault[LOOKUP(pcFieldName, lcDataCFields)] THEN
  DO:
     CASE pcFieldName:
        WHEN "title"        THEN cVal = pcTitle.
        WHEN "lname"        THEN cVal = pcLname.
        WHEN "lname2"       THEN cVal = pcLname2.
        WHEN "fname"        THEN cVal = pcFname.
        WHEN "coname"       THEN cVal = pcConame.
        WHEN "nationality"  THEN cVal = pcNationality.
        WHEN "bankaccount"  THEN cVal = pcBankAccount.
        WHEN "email"        THEN cVal = pcEmail.
        WHEN "sms_number"   THEN cVal = pcSMSNumber.
        WHEN "phone_number" THEN cVal = pcPhoneNumber.
        WHEN "person_id"    THEN cVal = pcPersonId.
        WHEN "language"     THEN cVal = pcLanguage.
     END.
  END.
  ELSE
  DO:
     CASE pcFieldName:
        WHEN "title"        THEN cVal = "deftitle". 
        WHEN "lname"        THEN cVal = "deflname".
        WHEN "lname2"       THEN cVal = "deflname2".
        WHEN "fname"        THEN cVal = "deffirstname".
        WHEN "coname"       THEN cVal = "defconame".
        WHEN "nationality"  THEN cVal = "defnation".
        WHEN "bankaccount"  THEN cVal = "22222-333333".
        WHEN "email"        THEN cVal = "def.def2@gmail.com".
        WHEN "sms_number"   THEN cVal = "55555555".
        WHEN "phone_number" THEN cVal = "77777777".
        WHEN "person_id"    THEN cVal = "defperson".
        WHEN "language"     THEN cVal = "en".
     END.
     cVal = STRING(piCustNum) + cVal.
  END.

  RETURN cVal.
END.


FUNCTION fGetExpectedLData RETURN LOGICAL
   (INPUT pcFieldName AS CHARACTER):
  DEFINE VARIABLE lVal AS LOGICAL NO-UNDO. 

  IF NOT lDatalDefault[LOOKUP(pcFieldName, lcDataLFields)] THEN
     CASE pcFieldName:
        WHEN "mark_sms"       THEN lVal = plMarkSMS.
        WHEN "mark_email"     THEN lVal = plMarkEmail.
        WHEN "mark_post"      THEN lVal = plMarkPost.
        WHEN "mark_sms_3rd"   THEN lVal = plMarkSMS3rd.
        WHEN "mark_email_3rd" THEN lVal = plMarkEmail3rd.
        WHEN "mark_post_3rd"  THEN lVal = plMarkPost3rd.
     END.
  ELSE
     CASE pcFieldName:
        WHEN "mark_sms"      THEN lVal = FALSE.
        WHEN "mark_email"    THEN lVal = TRUE.
        WHEN "mark_post"     THEN lVal = FALSE.
        WHEN "mark_sms_3rd"  THEN lVal = TRUE.
        WHEN "mark_email3rd" THEN lVal = FALSE.
        WHEN "mark_post3rd"  THEN lVal = TRUE.
     END.
  RETURN lVal.
END.

FUNCTION fCheckCustomerData RETURN LOGICAL:
   FIND Customer WHERE Customer.CustNum = piCustNum NO-LOCK NO-ERROR.
   IF AVAIL Customer THEN
   DO:
      checkChar("Customer.HonTitle",    Customer.HonTitle, 
         fGetExpectedCData("title")).
      checkChar("Customer.custname",    Customer.CustName, 
         fGetExpectedCData("lname")).
      checkChar("Customer.surname2",    Customer.SurName2, 
         fGetExpectedCData("lname2")).
      checkChar("Customer.firstname",   Customer.firstname, 
         fGetExpectedCData("fname")).
      checkChar("Customer.coname",      Customer.coname, 
         fGetExpectedCData("coname")).
      checkChar("Customer.Nationality", Customer.Nationality, 
         fGetExpectedCData("nationality")).
      checkChar("Customer.BankAccount", Customer.BankAcct, 
         fGetExpectedCData("bankaccount")).
      checkChar("Customer.Email",       Customer.Email, 
         fGetExpectedCData("email")).
      checkChar("Customer.SMSNumber",   Customer.SMSNumber, 
         fGetExpectedCData("sms_number")).
      checkChar("Customer.Phone",       Customer.Phone, 
         fGetExpectedCData("phone_number")).
      checkChar("Customer.OrgId",       Customer.OrgId, 
         fGetExpectedCData("person_id")).
      checkChar("Customer.Language", ENTRY(Customer.Language, {&languages}),
         fGetExpectedCData("language")).

      checkLogical("Customer.DirMarkSMS",   Customer.DirMarkSMS, 
         fGetExpectedLData("mark_sms"), "").
      checkLogical("Customer.DirMarkEmail", Customer.DirMarkEmail, 
         fGetExpectedLData("mark_email"), "").
      checkLogical("Customer.DirMarkPost",  Customer.DirMarkPost, 
         fGetExpectedLData("mark_post"), "").
      checkLogical("Customer.OutMarkSMS",   Customer.OutMarkSMS, 
         fGetExpectedLData("mark_sms_3rd"), "").
      checkLogical("Customer.OutMarkEmail", Customer.OutMarkEmail, 
         fGetExpectedLData("mark_email_3rd"), "").
      checkLogical("Customer.OutMarkPost",  Customer.OutMarkPost, 
         fGetExpectedLData("mark_post_3rd"), "").
   END.
   ELSE
      assert(FALSE, "Expected Customer did not exist").
   RETURN TRUE.
END.


FUNCTION fCheckLimitNotExist RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   FIND FIRST Limit WHERE Limit.CustNum = piCustNum AND Limit.LimitType eq 2 AND
              Limit.TMRuleSeq eq 0 AND Limit.LimitId eq 0 AND
              Limit.ToDate GT TODAY NO-LOCK NO-ERROR.
   IF AVAIL Limit THEN
      assert(FALSE, "Limit existed unexpectedly").
   RETURN TRUE.

END.


FUNCTION fCheckCreatedLimit RETURN LOGICAL (INPUT piCustNum AS INTEGER,
   INPUT piLimitValue AS INTEGER):

   FIND FIRST Limit WHERE Limit.CustNum = piCustNum AND Limit.LimitType eq 2 AND
        Limit.TMRuleSeq eq 0 AND Limit.LimitId eq 0
        NO-LOCK NO-ERROR.
   IF NOT AVAIL Limit THEN
      assert(FALSE, "Expected Limit did not exist").
   ELSE
   DO:
      checkDecimal("Limit.LimitAmt", Limit.LimitAmt, DECIMAL(piLimitValue)).
      checkInt("Limit.ValueType", Limit.ValueType, 1).
      checkLogical("Limit.DefValue", Limit.DefValue, FALSE, "").
      checkDate("Limit.FromDate", Limit.FromDate, TODAY).
      checkDate("Limit.ToDate", Limit.ToDate, DATE(12,31,2049)).
      checkInt("Limit.MsSeq", Limit.MsSeq, 0).
   END.
   RETURN TRUE.
END.

FUNCTION fCheckUpdatedLimit RETURN LOGICAL (INPUT piCustNum AS INTEGER,
   INPUT piLimitValue AS INTEGER):

   FIND FIRST Limit WHERE Limit.CustNum = piCustNum AND Limit.LimitType eq 2 AND
        Limit.TMRuleSeq eq 0 AND Limit.LimitId eq 0
        NO-LOCK NO-ERROR.
   IF NOT AVAIL Limit THEN
      assert(FALSE, "Expected Limit did not exist").
   ELSE
   DO:
      checkDecimal("Limit.LimitAmt", Limit.LimitAmt, DECIMAL(piLimitValue)).
      checkLogical("Limit.DefValue", Limit.DefValue, FALSE, "").
   END.
   RETURN TRUE.

END.



PROCEDURE test_all_params_given:
   fFillParamData().
   piCustNum = 34.
   fSetDefaultFields("", "CHARACTER").
   fSetDefaultFields("", "LOGICAL").
   fAddDataParams().
   gcParamStruct = add_struct(gcParamArray, "").
   call_set_customer_details().
   assert_success().
   assert(get_bool("",""), "Return value was not TRUE").
   fCheckCustomerData().
   fCheckLimitNotExist(34).
END.


PROCEDURE test_all_params_limit_created:
   fFillParamData().
   fSetDefaultFields("", "CHARACTER").
   fSetDefaultFields("", "LOGICAL").
   fAddDataParams().
   add_string(gcParamStruct, "subscription_limit", "20").
   gcParamStruct = add_struct(gcParamArray, "").
   call_set_customer_details().
   assert_success().
   assert(get_bool("",""), "Return value was not TRUE").
   fCheckCustomerData().
   fCheckCreatedLimit(34, 20).
END.


PROCEDURE test_all_params_limit_updated:
   fFillParamData().
   piCustNum = 121.
   fSetDefaultFields("", "CHARACTER").
   fSetDefaultFields("", "LOGICAL").
   fAddDataParams().
   add_string(gcParamStruct, "subscription_limit", "35").
   gcParamStruct = add_struct(gcParamArray, "").
   call_set_customer_details().
   assert_success().
   assert(get_bool("",""), "Return value was not TRUE").
   fCheckCustomerData().
   fCheckUpdatedLimit(121, 35).
END.

PROCEDURE test_too_low_limit:
   fFillParamData().
   fSetDefaultFields("", "CHARACTER").
   fSetDefaultFields("", "LOGICAL").
   fAddDataParams().
   add_string(gcParamStruct, "subscription_limit", "-1").
   gcParamStruct = add_struct(gcParamArray, "").
   call_set_customer_details().
   assert_fault({&APPLICATION_ERROR}, "Invalid subscription limit value -1"). 
END.


PROCEDURE test_too_high_limit:
   fFillParamData().
   fSetDefaultFields("", "CHARACTER").
   fSetDefaultFields("", "LOGICAL").
   fAddDataParams().
   add_string(gcParamStruct, "subscription_limit", "1000").
   gcParamStruct = add_struct(gcParamArray, "").
   call_set_customer_details().
   assert_fault({&APPLICATION_ERROR}, "Invalid subscription limit value 1000"). 
END.


PROCEDURE test_partially_defaulted_data:
   fFillParamData().
   fSetDefaultFields("title,lname2,email", "CHARACTER").
   fSetDefaultFields("MarkSMS,MarkSMS3rd,MarkPost3rd", "LOGICAL").
   fAddDataParams().
   gcParamStruct = add_struct(gcParamArray, "").
   call_set_customer_details().
   assert_success().
   fCheckCustomerData().
END.





PROCEDURE test_not_found_customer:
   fFillParamData().
   piCustNum = 999999.
   fSetDefaultFields("", "CHARACTER").
   fSetDefaultFields("", "LOGICAL").
   fAddDataParams().
   gcParamStruct = add_struct(gcParamArray, "").
   call_set_customer_details().
   assert_fault({&APPLICATION_ERROR}, "Customer for 999999 not found"). 
END 
