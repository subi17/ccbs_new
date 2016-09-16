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
liTesting = 0.
DEF STREAM sOut.

&GLOBAL-DEFINE MASMOVIL_RETRY_ERROR_CODES "APIKIT-00404,APIKIT-00405,APIKIT-00406,APIKIT-00415,WO-10000000,ESB-99999999"

FUNCTION fMasXMLGenerate_test RETURNS CHAR
   (icMethod AS CHAR):
   IF liTesting NE 0 THEN DO:
      xmlrpc_initialize(FALSE).
      OUTPUT STREAM sOut TO VALUE("Xmasmovile_xml_" + 
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
   initialize(lcConURL, 15).
   RETURN "".
END.


FUNCTION fAddCharacteristic RETURNS CHAR
   (icBase AS CHAR,
    icParam AS CHAR,
    icValue AS CHAR,
    icOldValue AS CHAR):
   DEF VAR lcCharacteristicStruct AS CHAR.
   DEF VAR lcInStruct AS CHAR.

   lcCharacteristicStruct = add_struct(icBase,"").
   lcInStruct = add_struct(lcCharacteristicStruct,"Characteristic").
   add_string(lcInStruct, "name", icParam).
   add_string(lcInStruct, "value", icValue ).
   add_string(lcInStruct, "oldValue", icOldValue).
   
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

   lcSerStruct = add_struct(icBase, "").
   lcInStruct = add_struct(lcSerStruct, "Service").
   add_string(lcInStruct, "serviceID", icSerID).
   add_string(lcInStruct, "serviceName", icSerName).
   add_string(lcInStruct, "action", icSerAction).
   add_string(lcInStruct, "type", icSerType).
   RETURN lcInStruct.
   
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
   DEF BUFFER bOC FOR OrderCustomer.
   DEF BUFFER bOF FOR OrderFusion.
   DEF BUFFER bCLIType FOR CliType.

   FIND FIRST bOrder NO-LOCK where 
              bOrder.Brand EQ Syst.Parameters:gcBrand AND
              bOrder.OrderId EQ iiOrderid NO-ERROR.
   IF NOT AVAIL bOrder THEN 
      RETURN "Error: Order not found " + STRING(iiOrderID) .

  /*Use delivery customer information if it is avbailable*/
   FIND FIRST bOC NO-LOCK WHERE 
              bOC.Brand EQ Syst.Parameters:gcBrand AND
              bOC.OrderId EQ iiOrderid AND 
              bOC.RowType EQ 4
              NO-ERROR.
   IF NOT AVAIL bOC THEN DO:
      FIND FIRST bOC NO-LOCK WHERE 
                 bOC.Brand EQ Syst.Parameters:gcBrand AND
                 bOC.OrderId EQ iiOrderid AND 
                 bOc.RowType EQ 1 /*This customer should be available*/
                 NO-ERROR.
      IF NOT AVAIL bOC THEN 
         RETURN "Error: Customer data not found " + STRING(iiOrderID) .

   END.
   
   FIND FIRST bOF NO-LOCK WHERE
              bOF.Brand EQ Syst.Parameters:gcBrand AND
              bOF.OrderID EQ iiOrderID NO-ERROR.
   IF NOT AVAIL bOF THEN
               RETURN "Error: Fixed Order data not found " + STRING(iiOrderID) .


   /*Generate order type*/
IF liTesting NE 0 THEN DO:
            lcOrderType = "Alta FTTH + VOIP".
            lcConnServiceId = "FTTH".
            lcConnServiceName = "FTTH connection".
            lcConnServiceType = "FTTH".

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
   lcOrderStruct = add_struct(lcOutputStruct,"Order").
   add_string(lcOrderStruct, "orderID", 
                             "Y" + STRING(bOrder.Orderid)).
   add_string(lcOrderStruct, "orderType", lcOrderType). 
   add_string(lcOrderStruct, "orderName", "ALTA").
   add_string(lcOrderStruct, "sellchannel", "YOIGO").
   add_string(lcOrderStruct, "selldate", STRING(ldaSellDate)). 
   add_string(lcOrderStruct, "seller", "YOIGO"). 
   add_string(lcOrderStruct, "createdBy", "YOIGO").
   add_string(lcOrderStruct, "creadate", STRING(ldaCreDate)). 

   /*Installation*/
   lcInstallationStruct = add_struct(lcOrderStruct, "Installation").
   lcContactStruct = add_struct(lcInstallationStruct, "Contact").
   add_string(lcContactStruct, "firstName", bOC.FirstName).
   add_string(lcContactStruct, "middleName", "").
   add_string(lcContactStruct, "lastName", bOC.Surname1 + " " + bOC.Surname2).
   add_string(lcContactStruct, "documentNumber",bOC.CustID). 
   add_string(lcContactStruct, "documentType", bOC.CustIdType).
   add_string(lcContactStruct, "Email", bOC.Email).
   add_string(lcContactStruct, "phoneNumber", bOC.ContactNum).

   lcAddressStruct = add_struct(lcInstallationStruct, "Address").
   add_string(lcAddressStruct, "country", bOC.Country).
   add_string(lcAddressStruct, "province", bOC.Region).
   add_string(lcAddressStruct, "town", bOC.PostOffice).
   add_string(lcAddressStruct, "street", bOC.Street).
   add_string(lcAddressStruct, "streetType", bOC.StreetType). 
   add_string(lcAddressStruct, "number", bOC.BuildingNum).
   add_string(lcAddressStruct, "bis_duplicate", bOC.BisDuplicate).
   add_string(lcAddressStruct, "block", bOC.Block).
   add_string(lcAddressStruct, "door", bOC.Door).
   add_string(lcAddressStruct, "letter", bOC.Letter).
   add_string(lcAddressStruct, "stair", bOC.Stair).
   add_string(lcAddressStruct, "floor", bOC.Floor).
   add_string(lcAddressStruct, "hand", bOC.Hand).
   add_string(lcAddressStruct, "Km", bOC.Km).
   add_string(lcAddressStruct, "zipCode", bOc.ZipCode).
   IF lcConnServiceId EQ "ADSL" THEN
      add_string(lcInstallationStruct, "modality", bOF.ADSLLinkstate).

   lcServiceArray = add_array(lcOrderStruct,"Services").

    /*Services entry - Phone*/
   lcServiceStruct = fAddService(lcServiceArray, 
               "FixedPhone", 
               "Fixed Phone Number", 
               "add", 
               "PHONE").

   /*Characteristics for the service*/
   lcCharacteristicsArray = add_array(lcServiceStruct,"Characteristics").


   /*Mandatory in portability*/
   IF bOF.FixedNumberType NE "new" THEN DO:
      fAddCharacteristic(lcCharacteristicsArray, /*base*/
                         "donoroperator",        /*param name*/
                         bOF.FixedCurrOperCode,  /*param value*/
                         "").                    /*old value*/

      fAddCharacteristic(lcCharacteristicsArray,  /*base*/
                         "phoneNumberTmp",        /*param name*/
                         "",                      /*param value*/
                         "").                     /*old value*/
      fAddCharacteristic(lcCharacteristicsArray, /*base*/
                         "portabilitytype",      /*param name*/
                         "I", /*port in = I*/    /*param value*/
                         "").                    /*old value*/
 
   END.

   fAddCharacteristic(lcCharacteristicsArray, /*base*/
                      "phoneNumber",          /*param name*/
                      bOF.FixedNumber,        /*param value*/
                      "").                    /*old value*/

   fAddCharacteristic(lcCharacteristicsArray, /*base*/
                      "reselleroperator",      /*param name*/
                      "Sonera",                /*param value*/
                      "").                     /*old value*/

   fAddCharacteristic(lcCharacteristicsArray,  /*base*/
                      "receptooperator",        /*param name*/
                      "0031",/*must be 0031*/   /*param value*/
                      "").                      /*old value*/
 

   /*Services entry - Line*/
   lcServiceStruct = fAddService(lcServiceArray, 
               lcConnServiceId, 
               lcConnServiceName, 
               "add", 
               lcConnServiceType).
  
   /*Characteristics for the service*/
   lcCharacteristicsArray = add_array(lcServiceStruct,"Characteristics" ).
   IF lcConnServiceId EQ "FTTH" THEN DO:
      fAddCharacteristic(lcCharacteristicsArray,      /*base*/
                         "UploadSpeed",               /*param name*/
                         bCLIType.FixedLineUpload,    /*param value*/
                         "").                         /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,      /*base*/
                         "DownloadSpeed",             /*param name*/
                         bCLIType.FixedLineDownload,  /*param value*/
                         "").                         /*old value*/
 
   END.

   fAddCharacteristic(lcCharacteristicsArray, /*base*/
                      "gescal",               /*param name*/
                      bOC.Gescal,             /*param value*/
                      "").                    /*old value*/

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).
   fMasXMLGenerate_test("createFixedLine").
   RUN pRPCMethodCall("masmovil.createFixedLine", TRUE).

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("NW_ERROR: &1", gc_xmlrpc_error).

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   lcResponse = validate_struct(lcXMLStruct,"resultCode!,resultDescription").
   lcResultCode = get_string(lcResponse, "resultCode").
   IF LOOKUP('resultDescription', lcResponse) GT 0 THEN
      lcResultDesc = get_string(lcResponse, "resultDescription").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   IF lcResultCode NE "" THEN DO:
      ocResultCode =  lcResultCode.
      ocResultDesc =  lcResultDesc.
      RETURN "ERROR".
   END.   

   RETURN "".
END. /*Function fCreate_FixedLine*/


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
   fMasXMLGenerate_test("CancelFixedLine").
   RUN pRPCMethodCall("masmovil.cancelFixedLine", TRUE).

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("NW_ERROR: &1", gc_xmlrpc_error).

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   lcResponse = validate_struct(lcXMLStruct,"resultCode!,resultDescription").
   lcResultCode = get_string(lcResponse, "resultCode").
   IF LOOKUP('resultDescription', lcResponse) GT 0 THEN
      lcResultDesc = get_string(lcResponse, "resultDescription").

   IF gi_xmlrpc_error NE 0 THEN 
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).
   
   IF lcResultCode NE "" THEN DO:
      ocResultDesc = lcResultCode + "," + lcResultDesc.
      ocResultCode = lcResultCode.
      RETURN "ERROR".
   END.   
   RETURN "".
END. /*fMasCancel_FixedLineOrder*/


FUNCTION fMasGet_FixedNbr RETURNS CHAR
   (icPostalCode AS CHAR ,
    OUTPUT ocNum AS CHAR,
    OUTPUT ocResult AS CHAR): /*Error message*/
   DEF VAR lcOutputStruct AS CHAR NO-UNDO.
   DEF VAR lcXMLStruct AS CHAR NO-UNDO. /*Input to TMS*/
   DEF VAR lcResponse AS CHAR NO-UNDO.
   DEF VAR lcResult AS CHAR NO-UNDO.

   DEF VAR ldtLastChange AS DATETIME.
   DEF VAR ldtAssigned AS DATETIME.
   
   lcOutputStruct = add_struct(param_toplevel_id, "").
   
   add_string(lcOutputStruct, "postalCode", icPostalCOde).
   
   IF gi_xmlrpc_error NE 0 THEN
         RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).
   xmlrpc_initialize(FALSE).
   fMasXMLGenerate_test("getnewResource").
   RUN pRPCMethodCall("masmovil.getNewResource", TRUE).

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("NW_ERROR: &1", gc_xmlrpc_error).

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   lcResponse = validate_struct(lcXMLStruct,"idNumero,fechaAsignationCmt,fechaUltimoCambio,numero!,links").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   ocNum = get_string(lcResponse,"numero").

   IF ocNum EQ "" THEN DO:
       ocResult = "Masmovil Error: Number not returned. Area: " + icPostalCode.
       RETURN "ERROR".
   END.
   RETURN "".

END.









