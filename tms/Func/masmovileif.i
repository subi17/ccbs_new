/* ----------------------------------------------------------------------
  MODULE .......: masmovileif.i
  TASK .........: Functions for masmovile integration
  APPLICATION ..: TMS
  AUTHOR .......: ilsavola
  CREATED ......: 29.8.2016
  CHANGED ......:
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/tmsconst.i}
{xmlrpc/xmlrpc_client.i}
{Func/forderstamp.i}
{Func/fixedlinefunc.i}

DEF VAR lcConURL AS CHAR NO-UNDO.
DEF VAR liTesting AS INT NO-UNDO.
DEF VAR liPrintXML AS INT NO-UNDO.
DEF VAR lcUpload AS CHAR NO-UNDO.
DEF VAR lcDownload AS CHAR NO-UNDO.

/*For testing*/
/*litesting
   sets xml data file writing on
   makes hard codings to order creation data*/
liTesting = 0. /*2 = adsl, 1 = fiber*/
lcdownload = "50M".
lcupload = "5M".
liPrintXML = 1.

DEF STREAM sOut.

&GLOBAL-DEFINE MASMOVIL_ERROR_ADAPTER_PARSING "1"
&GLOBAL-DEFINE MASMOVIL_ERROR_ADAPTER_NETWORK "2"
&GLOBAL-DEFINE MASMOVIL_ERROR_MASMOVIL "3"
&GLOBAL-DEFINE MASMOVIL_RETRY_ERROR_CODES "APIKIT-00404,APIKIT-00405,APIKIT-00406,APIKIT-00415,WO-10000000,ESB-99999999"

FUNCTION fMasXMLGenerate_test RETURNS CHAR
   (icMethod AS CHAR):
   IF liPrintXML NE 0 THEN DO:
      xmlrpc_initialize(FALSE).
      OUTPUT STREAM sOut TO VALUE("/tmp/Xmasmovile_xml_" + 
      REPLACE(STRING(fmakets()), ".", "_") +
      ".xml") APPEND.
      PUT STREAM sOut UNFORMATTED 
         string(serialize_rpc_call("masmovil." + icMethod)) SKIP. 
      PUT STREAM sOut "" SKIP.   
      OUTPUT STREAM sOut CLOSE.
      xmlrpc_initialize(FALSE).
   END.   
END.   


FUNCTION fInitMMConnection RETURNS CHAR
   ():
   lcConURL = Syst.Parameters:getc("urlMasmovil","URL").
   IF lcConURL = ? OR lcConURL = "" THEN 
      RETURN "ERROR in connection settings".

   IF initialize(lcConURL, 30) EQ FALSE THEN
      RETURN "ERROR in connection initialization".

   RETURN "".
END.


FUNCTION fAddCharacteristic RETURNS CHAR
   (icBase AS CHAR,
    icParam AS CHAR,
    icValue AS CHAR,
    icOldValue AS CHAR):
   DEF VAR lcCharacteristicStruct AS CHAR.
   DEF VAR lcInStruct AS CHAR.
/*
   lcCharacteristicStruct = add_struct(icBase,"").
   lcInStruct = add_struct(lcCharacteristicStruct,"Characteristic").
   add_string(lcInStruct, "name", icParam).
   add_string(lcInStruct, "value", icValue ).
   add_string(lcInStruct, "oldValue", icOldValue).
*/

   lcCharacteristicStruct = add_struct(icBase,"").
   add_string(lcCharacteristicStruct, "name", icParam).
   add_string(lcCharacteristicStruct, "value", icValue ).
   add_string(lcCharacteristicStruct, "oldValue", icOldValue).

   RETURN "".
END.

FUNCTION fAddService RETURNS CHAR
   (icBase AS CHAR,
    icSerID AS CHAR,
    icSerName AS CHAR,
    icSerAction AS CHAR,
    icSerType AS CHAR):
   DEF VAR lcSerStruct AS CHAR.
   DEF VAR lcInStruct AS CHAR.
/*
   lcSerStruct = add_struct(icBase, "").
   lcInStruct = add_struct(lcSerStruct, "Service").
   add_string(lcInStruct, "serviceID", icSerID).
   add_string(lcInStruct, "serviceName", icSerName).
   add_string(lcInStruct, "action", icSerAction).
   add_string(lcInStruct, "type", icSerType).
   RETURN lcInStruct.
 */ 
   lcSerStruct = add_struct(icBase, "").
   add_string(lcSerStruct, "serviceID", icSerID).
   add_string(lcSerStruct, "serviceName", icSerName).
   add_string(lcSerStruct, "action", icSerAction).
   add_string(lcSerStruct, "type", icSerType).
   RETURN lcSerStruct.
  
END.


FUNCTION fMasCreate_FixedLineOrder RETURNS CHAR
   (iiOrderId AS INT,
    OUTPUT ocResultCode AS CHAR,
    OUTPUT ocResultDesc AS CHAR):
   DEF VAR lcOrderStruct AS CHAR NO-UNDO.
   DEF VAR lcServiceArray AS CHAR NO-UNDO.
   DEF VAR lcContactStruct AS CHAR NO-UNDO.
   DEF VAR lcAddressStruct AS CHAR NO-UNDO.
   DEF VAR lcOutputStruct AS CHAR NO-UNDO.
   DEF VAR lcCharacteristicsArray AS CHAR NO-UNDO.
   DEF VAR liResponseCode AS INT NO-UNDO.
   DEF VAR lcOrderType AS CHAR NO-UNDO.
   DEF VAR lcXMLStruct AS CHAR NO-UNDO. /*Input to TMS*/
   DEF VAR lcResponse AS CHAR NO-UNDO.
   DEF VAR lcResultCode AS CHAR NO-UNDO.
   DEF VAR lcResultDesc AS CHAR NO-UNDO.
   DEF VAR lcConnServiceId AS CHAR NO-UNDO.
   DEF VAR lcConnServiceName AS CHAR NO-UNDO.
   DEF VAR lcConnServiceType AS CHAR NO-UNDO.
   DEF VAR lcInstallationStruct AS CHAR NO-UNDO.
   DEF VAR lcServiceStruct AS CHAR NO-UNDO.
   DEF VAR ldaSellDate AS DATE.
   DEF VAR ldaCreDate AS DATE.
   DEF VAR lcResult AS CHAR NO-UNDO.

   DEF BUFFER bOrder FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER OrderFusion FOR OrderFusion.
   DEF BUFFER bCLIType FOR CliType.

   FIND FIRST bOrder NO-LOCK where 
              bOrder.Brand EQ Syst.Parameters:gcBrand AND
              bOrder.OrderId EQ iiOrderid NO-ERROR.
   IF NOT AVAIL bOrder THEN 
      RETURN "Error: Order not found " + STRING(iiOrderID) .

  /*Use delivery customer information if it is avbailable*/
   FIND FIRST OrderCustomer NO-LOCK WHERE 
              OrderCustomer.Brand EQ Syst.Parameters:gcBrand AND
              OrderCustomer.OrderId EQ iiOrderid AND 
              OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
              NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN
      RETURN "Error: Customer data not found " + STRING(iiOrderID) .
   
   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand EQ Syst.Parameters:gcBrand AND
              OrderFusion.OrderID EQ iiOrderID NO-ERROR.
   IF NOT AVAIL OrderFusion THEN
               RETURN "Error: Fixed Order data not found " + STRING(iiOrderID) .


   /*Generate order type*/
IF liTesting NE 0 THEN DO:
   IF liTesting EQ 1 THEN DO:
            lcOrderType = "Alta FTTH + VOIP".
            lcConnServiceId = "FTTH".
            lcConnServiceName = "FTTH".
            lcConnServiceType = "FTTH".
   END.
   ELSE DO:
            lcOrderType = "Alta xDSL + VOIP".
            lcConnServiceId = "ADSL".
            lcConnServiceName = "ADSL".
            lcConnServiceType = "ADSL".
   END.
END. /*testing related*/
ELSE DO:
   IF fIsConvergenceTariff(bOrder.CliType) THEN DO:
      FIND FIRST bCLIType NO-LOCK WHERE
                 bCLIType.CLIType EQ bOrder.CliType NO-ERROR.
      IF AVAIL bCLIType THEN DO:
         IF bCLIType.FixedLineType EQ 1 THEN DO:
            lcOrderType = "Alta xDSL + VOIP".
            lcConnServiceId = "ADSL".
            lcConnServiceName = "ADSL connection".
            lcConnServiceType = "ADSL".
         END.
         ELSE IF bCLIType.FixedLineType EQ 2 THEN DO:
            lcOrderType = "Alta FTTH + VOIP".
            lcConnServiceId = "FTTH".
            lcConnServiceName = "FTTH connection".
            lcConnServiceType = "FTTH".
         END.
         ELSE RETURN "Not allowed Fixed line type".
      END.
   END.
   ELSE
      RETURN "Error Not allowed CLITYPE " + bOrder.CliType.
END.

   IF fTS2Date(bOrder.CrStamp, ldaCreDate) EQ FALSE THEN
      RETURN "Error: Date reading failed".

   IF fTS2Date(bOrder.CrStamp, ldaSellDate) EQ FALSE THEN
      RETURN "Error: Date reading failed".

   lcOutputStruct = add_struct(param_toplevel_id, "").
   /*Order struct*/
/*   lcOrderStruct = add_struct(lcOutputStruct,"Order").*/
   add_string(lcOutputStruct, "orderID", 
                             "Y" + STRING(bOrder.Orderid)).
   add_string(lcOutputStruct, "orderType", lcOrderType). 
   add_string(lcOutputStruct, "orderName", "ALTA").
   add_string(lcOutputStruct, "sellchannel", "YOIGO").
   add_string(lcOutputStruct, "selldate", STRING(ldaSellDate)). 
   add_string(lcOutputStruct, "seller", "YOIGO"). 
   add_string(lcOutputStruct, "createdBy", "YOIGO").
   add_string(lcOutputStruct, "createdDate", STRING(ldaCreDate)). 

   /*Installation*/
   lcInstallationStruct = add_struct(lcOutputStruct, "Installation").
   lcContactStruct = add_struct(lcInstallationStruct, "Contact").
   add_string(lcContactStruct, "firstName", OrderCustomer.FirstName).
   /*add_string(lcContactStruct, "middleName", "").*/
   add_string(lcContactStruct, "lastName", OrderCustomer.Surname1 + " " + OrderCustomer.Surname2).
   add_string(lcContactStruct, "documentNumber",OrderCustomer.CustID). 
   add_string(lcContactStruct, "documentType", OrderCustomer.CustIdType).
   add_string(lcContactStruct, "email", OrderCustomer.Email).
   add_string(lcContactStruct, "phoneNumber", OrderCustomer.FixedNum).

   lcAddressStruct = add_struct(lcInstallationStruct, "Address").
   add_string(lcAddressStruct, "country", OrderCustomer.Country).
   add_string(lcAddressStruct, "province", OrderCustomer.Region).
   add_string(lcAddressStruct, "town", OrderCustomer.PostOffice).
   add_string(lcAddressStruct, "street", OrderCustomer.Street).
   add_string(lcAddressStruct, "streetType", OrderCustomer.StreetType). 
   add_string(lcAddressStruct, "number", OrderCustomer.BuildingNum).
   add_string(lcAddressStruct, "bis_duplicate", OrderCustomer.BisDuplicate).
   add_string(lcAddressStruct, "block", OrderCustomer.Block).
   add_string(lcAddressStruct, "door", OrderCustomer.Door).
   add_string(lcAddressStruct, "letter", OrderCustomer.Letter).
   add_string(lcAddressStruct, "stair", OrderCustomer.Stair).
   add_string(lcAddressStruct, "floor", OrderCustomer.Floor).
   add_string(lcAddressStruct, "hand", OrderCustomer.Hand).
   add_string(lcAddressStruct, "km", OrderCustomer.Km).
   add_string(lcAddressStruct, "zipCode", OrderCustomer.ZipCode).
   IF lcConnServiceId EQ "ADSL" THEN
      add_string(lcInstallationStruct, "modality", OrderFusion.ADSLLinkstate).

   lcServiceArray = add_array(lcOutputStruct,"Services").

    /*Services entry - Phone*/
   lcServiceStruct = fAddService(lcServiceArray, 
               "FixedPhone", 
               "Fixed Phone Number", 
               "add", 
               "PHONE").

   /*Characteristics for the service*/
   lcCharacteristicsArray = add_array(lcServiceStruct,"Characteristics").


   /*Mandatory in portability*/
   IF OrderFusion.FixedNumberType NE "new" THEN DO:
      fAddCharacteristic(lcCharacteristicsArray, /*base*/
                         "donoroperator",        /*param name*/
                         OrderFusion.FixedCurrOperCode,  /*param value*/
                         "").                    /*old value*/

      fAddCharacteristic(lcCharacteristicsArray, /*base*/
                         "portabilitytype",      /*param name*/
                         "I", /*port in = I*/    /*param value*/
                         "").                    /*old value*/
    fAddCharacteristic(lcCharacteristicsArray,  /*base*/
                      "receptooperator",        /*param name*/
                      "0031",/*must be 0031*/   /*param value*/
                      "").                      /*old value*/
 
   END.

   fAddCharacteristic(lcCharacteristicsArray, /*base*/
                      "phoneNumber",          /*param name*/
                      OrderFusion.FixedNumber,        /*param value*/
                      "").                    /*old value*/

   /*Services entry - Line*/
   lcServiceStruct = fAddService(lcServiceArray, 
               lcConnServiceId, 
               lcConnServiceName, 
               "add", 
               lcConnServiceType).
  
   /*Characteristics for the service*/
   lcCharacteristicsArray = add_array(lcServiceStruct,"Characteristics" ).
   IF lcConnServiceId EQ "FTTH" THEN DO:
      IF liTesting NE 0 THEN DO:
         fAddCharacteristic(lcCharacteristicsArray,      /*base*/
                         "UploadSpeed",               /*param name*/
                         lcUpload,    /*param value*/
                         "").                         /*old value*/
         fAddCharacteristic(lcCharacteristicsArray,      /*base*/
                            "DownloadSpeed",             /*param name*/
                            lcDownload,  /*param value*/
                            "").                         /*old value*/
 
      END.
      ELSE DO:
         fAddCharacteristic(lcCharacteristicsArray,      /*base*/
                         "UploadSpeed",               /*param name*/
                         bCLIType.FixedLineUpload,    /*param value*/
                         "").                         /*old value*/
         fAddCharacteristic(lcCharacteristicsArray,      /*base*/
                            "DownloadSpeed",             /*param name*/
                            bCLIType.FixedLineDownload,  /*param value*/
                            "").                         /*old value*/
      END.
   END.

   fAddCharacteristic(lcCharacteristicsArray, /*base*/
                      "gescal",               /*param name*/
                      OrderCustomer.Gescal,             /*param value*/
                      "").                    /*old value*/

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).
   xmlrpc_initialize(FALSE).
   fMasXMLGenerate_test("createFixedLine").
   RUN pRPCMethodCall("masmovil.createFixedLine", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      ocResultCode = STRING(gi_xmlrpc_error).
      ocResultDesc = gc_xmlrpc_error.
      RETURN SUBST("NW_ERROR: &1", gc_xmlrpc_error).
   END.

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   lcResponse = validate_struct(lcXMLStruct,"resultCode!,resultDescription").
   lcResultCode = get_string(lcXMLSTruct, "resultCode").
   IF LOOKUP('resultDescription', lcXMLSTruct) GT 0 THEN
      lcResultDesc = get_string(lcXMLStruct, "resultDescription").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   ocResultCode =  lcResultCode.
   ocResultDesc =  lcResultDesc.
   message "Debug: " + lcResultCode + ";" + lcResultDesc VIEW-AS ALERT-BOX .
   IF NOT(lcResultCode EQ "" OR lcResultCode EQ "00") THEN  RETURN "ERROR".
    

   RETURN "".
END. /*Function fCreate_FixedLine*/



FUNCTION fMasCheckFixedLineStatus RETURNS CHAR
   (iiOrderId AS INT,
    OUTPUT ocOrderType AS CHAR,
    OUTPUT ocStatus AS CHAR,
    OUTPUT ocStatusDesc AS CHAR,
    OUTPUT ocAddInfo AS CHAR,
    OUTPUT odeLastDate AS DECIMAL):

   DEF VAR lcArray AS CHAR NO-UNDO.
   DEF VAR lcXMLStruct AS CHAR NO-UNDO. /*Input to TMS*/
   DEF VAR lcStatusFields AS CHAR NO-UNDO.
   DEF VAR lcOrderType AS CHAR NO-UNDO.
   DEF VAR lcStatus AS CHAR NO-UNDO.
   DEF VAR lcStatusDesc AS CHAR NO-UNDO.
   DEF VAR lcAdditionalInfo AS CHAR NO-UNDO.
   DEF VAR lcLastDate AS CHAR NO-UNDO.
   DEF VAR ldeLastDate AS DECIMAL NO-UNDO.

   DEF VAR lcInStruct AS CHAR NO-UNDO.

   add_string(param_toplevel_id, "", "Y" + STRING(iiOrderid)).
   add_string(param_toplevel_id, "", "false").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).
   xmlrpc_initialize(FALSE).
   fMasXMLGenerate_test("masmovil.checkOrderStatus").
   RUN pRPCMethodCall("masmovil.checkOrderStatus", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      ocStatus = STRING(gi_xmlrpc_error).
      ocStatusDesc = gc_xmlrpc_error.
      RETURN SUBST("NW_ERROR: &1", gc_xmlrpc_error).
   END.

   lcInStruct = get_struct(response_toplevel_id, "").
   lcArray = get_array(lcInStruct, "Statuses").
   lcXMLStruct = get_struct(lcArray, "0").


   lcStatusFields = validate_struct(lcXMLStruct,"OrderType,ServiceType,Status!,StatusDescription!,process,additionalInfo,lastDate!").
   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response validation failed: &1", gc_xmlrpc_error).
   ASSIGN
      lcOrderType = get_string(lcXMLStruct, "OrderType")
         WHEN LOOKUP("OrderType", lcStatusFields) > 0
      lcStatus = get_string(lcXMLStruct, "Status")
      lcStatusDesc = get_string(lcXMLStruct, "StatusDescription")
         WHEN LOOKUP("StatusDescription", lcStatusFields) > 0
      lcAdditionalInfo = get_string(lcXMLStruct, "additionalInfo")
         WHEN LOOKUP("additionalInfo", lcStatusFields) > 0
      lcLastDate = get_string(lcXMLStruct, "lastDate").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   ldeLastDate = _iso8601_to_timestamp(lcLastDate).
   IF ldeLastDate EQ ? THEN RETURN "Returned date format is incorrect".

   ocOrderType = lcOrderType.
   ocStatus =  lcStatus.
   ocStatusDesc =  lcStatusDesc.
   ocAddInfo = lcAdditionalInfo.
   odeLastDate = ldeLastDate.

   RETURN "".

END. /*fMasCancel_FixedLineOrder*/

FUNCTION fMasCancel_FixedLineOrder RETURNS CHAR
   (iiOrderId AS INT,
    idaDate AS DATE,
    icMotive AS CHAR, 
    OUTPUT ocResultCode AS CHAR,
    OUTPUT ocResultDesc AS CHAR):

   DEF VAR lcOutputStruct AS CHAR NO-UNDO.
   DEF VAR lcXMLStruct AS CHAR NO-UNDO. /*Input to TMS*/
   DEF VAR lcResponse AS CHAR NO-UNDO.
   DEF VAR lcResultCode AS CHAR NO-UNDO.
   DEF VAR lcResultDesc AS CHAR NO-UNDO.

   lcOutputStruct = add_struct(param_toplevel_id, "").

   add_string(lcOutputStruct, "orderID", 
                             "Y" + STRING(iiOrderid)).
   add_string(lcOutputStruct, "cancellationDate", STRING(idaDate)).  
   add_string(lcOutputStruct, "cancellationMotive",  icMotive).

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).
   xmlrpc_initialize(FALSE).
   fMasXMLGenerate_test("CancelFixedLine").
   RUN pRPCMethodCall("masmovil.cancelFixedLine", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      ocResultCode = STRING(gi_xmlrpc_error).
      ocResultDesc = gc_xmlrpc_error.
      RETURN SUBST("NW_ERROR: &1", gc_xmlrpc_error).
   END.

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   lcResponse = validate_struct(lcXMLStruct,"resultCode!,resultDescription").
   lcResultCode = get_string(lcXMLStruct, "resultCode").
   IF LOOKUP('resultDescription', lcXMLSTruct) GT 0 THEN
      lcResultDesc = get_string(lcXMLStruct, "resultDescription").

   IF gi_xmlrpc_error NE 0 THEN 
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   ocResultCode =  lcResultCode.
   ocResultDesc =  lcResultDesc.
   message "CancelDbg: " + lcResultCode + ";" + lcResultDesc VIEW-AS ALERT-BOX.
   IF NOT(lcResultCode EQ "" OR lcResultCode EQ "00") THEN  RETURN "ERROR".

   RETURN "".
END. /*fMasCancel_FixedLineOrder*/


FUNCTION fMasGet_FixedNbr RETURNS CHAR
   (icPostalCode AS CHAR ,
    OUTPUT ocNum AS CHAR,
    OUTPUT ocResultCode AS CHAR,
    OUTPUT ocResultDesc AS CHAR): /*Error message*/
   DEF VAR lcXMLStruct AS CHAR NO-UNDO. /*Input to TMS*/
   DEF VAR lcResponse AS CHAR NO-UNDO.
   DEF VAR lcResult AS CHAR NO-UNDO.

   add_string(param_toplevel_id, "", icPostalCOde).

   IF gi_xmlrpc_error NE 0 THEN
         RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).
   xmlrpc_initialize(FALSE).
   fMasXMLGenerate_test("getnewResource").
   RUN pRPCMethodCall("masmovil.getNewResource", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      ocResultCode = STRING(gi_xmlrpc_error).
      ocResultDesc = gc_xmlrpc_error.
      RETURN SUBST("NW_ERROR: &1", gc_xmlrpc_error).
   END.

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   lcResponse = validate_struct(lcXMLStruct,"idNumero,fechaAsignacionCmt,fechaUltimoCambio,numero!,_links").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   ocNum = get_string(lcXMLStruct,"numero").

   IF ocNum EQ "" THEN
       RETURN "Masmovil Error: Number not returned. Area: " + icPostalCode.
   

   RETURN "".

END.









