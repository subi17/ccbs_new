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
{fcgi_agent/xmlrpc/xmlrpc_client.i}
{Func/forderstamp.i}
{Func/fixedlinefunc.i}
{Func/custfunc.i}

DEF VAR lcConURL AS CHAR NO-UNDO.
DEF VAR liPrintXML AS INT NO-UNDO.

/*For testing*/
liPrintXML = 0.

DEF STREAM sOut.

FUNCTION fMasXMLGenerate_test RETURNS CHAR
   (icMethod AS CHAR):
   IF liPrintXML NE 0 THEN DO:
      xmlrpc_initialize(FALSE).
      OUTPUT STREAM sOut TO VALUE("/tmp/Xmasmovile_xml_" + 
      REPLACE(STRING(Func.Common:mMakeTS()), ".", "_") +
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

   DEF VAR lcServiceArray AS CHAR NO-UNDO.
   DEF VAR lcContactStruct AS CHAR NO-UNDO.
   DEF VAR lcAddressStruct AS CHAR NO-UNDO.
   DEF VAR lcClientStruct AS CHAR NO-UNDO.
   DEF VAR lcOutputStruct AS CHAR NO-UNDO.
   DEF VAR lcCharacteristicsArray AS CHAR NO-UNDO.
   DEF VAR lcOrderType AS CHAR NO-UNDO.
   DEF VAR lcXMLStruct AS CHAR NO-UNDO. /*Input to TMS*/
   DEF VAR lcResponse AS CHAR NO-UNDO.
   DEF VAR lcConnServiceId AS CHAR NO-UNDO.
   DEF VAR lcConnServiceName AS CHAR NO-UNDO.
   DEF VAR lcConnServiceType AS CHAR NO-UNDO.
   DEF VAR lcInstallationStruct AS CHAR NO-UNDO.
   DEF VAR lcServiceStruct AS CHAR NO-UNDO.
   DEF VAR ldaSellDate AS DATE NO-UNDO.
   DEF VAR ldaCreDate AS DATE NO-UNDO.
   DEF VAR lcLastName AS CHAR NO-UNDO.
   DEF VAR lcCategory AS CHAR NO-UNDO.
   DEF VAR lcGescalValue AS CHAR NO-UNDO. 

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER bOrderCustomer FOR OrderCustomer.
   DEF BUFFER bHolderOrderCustomer FOR OrderCustomer.
   
   DEF BUFFER OrderFusion FOR OrderFusion.
   DEF BUFFER CLIType FOR CliType.

   FIND FIRST Order NO-LOCK where 
              Order.Brand EQ Syst.Var:gcBrand AND
              Order.OrderId EQ iiOrderid NO-ERROR.
   IF NOT AVAIL Order THEN 
      RETURN "ERROR: Order not found " + STRING(iiOrderID) .

  /*Use delivery customer information if it is avbailable*/
   FIND FIRST OrderCustomer NO-LOCK WHERE 
              OrderCustomer.Brand EQ Syst.Var:gcBrand AND
              OrderCustomer.OrderId EQ iiOrderid AND 
              OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
              NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN
      RETURN "ERROR: Install address data not found " + STRING(iiOrderID) .
   
   FIND FIRST bOrderCustomer NO-LOCK WHERE 
              bOrderCustomer.Brand EQ Syst.Var:gcBrand AND
              bOrderCustomer.OrderId EQ iiOrderid AND 
              bOrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
              NO-ERROR.
   
   IF NOT AVAIL bOrderCustomer THEN
      RETURN "ERROR: Customer data not found " + STRING(iiOrderID) .

   FIND FIRST bHolderOrderCustomer NO-LOCK WHERE 
              bHolderOrderCustomer.Brand EQ Syst.Var:gcBrand AND
              bHolderOrderCustomer.OrderId EQ iiOrderid AND 
              bHolderOrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER}
              NO-ERROR.
   
   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand EQ Syst.Var:gcBrand AND
              OrderFusion.OrderID EQ iiOrderID NO-ERROR.
   IF NOT AVAIL OrderFusion THEN
               RETURN "ERROR: Fixed Order data not found " + STRING(iiOrderID) .

   IF fIsConvergenceTariff(Order.CliType) THEN DO:
      FIND FIRST CLIType NO-LOCK WHERE
                 CLIType.CLIType EQ Order.CliType NO-ERROR.

      IF AVAIL CLIType THEN DO:
         IF CLIType.FixedLineType EQ 1 THEN DO:
            lcOrderType = "Alta xDSL".
            lcConnServiceId = "ADSL".
            lcConnServiceName = "ADSL connection".
            lcConnServiceType = "ADSL".
         END.
         ELSE IF CLIType.FixedLineType EQ 2 THEN DO:
            lcOrderType = "Alta FTTH".
            lcConnServiceId = "FTTH".
            lcConnServiceName = "FTTH connection".
            lcConnServiceType = "FTTH".
         END.
         ELSE RETURN "Not allowed Fixed line type".

         /* NEBACO-47 */
         IF Clitype.Clitype MATCHES "CONTFHNB*" THEN
            lcOrderType = lcOrderType + " NEBA".

         lcOrderType = lcOrderType +  " + VOIP".

      END.
   END.
   ELSE
      RETURN "ERROR: Not allowed CLITYPE " + Order.CliType.
   IF Func.Common:mTS2Date(Order.CrStamp, OUTPUT ldaCreDate) EQ FALSE THEN
      RETURN "ERROR: Date reading failed".

   IF Func.Common:mTS2Date(Order.CrStamp, OUTPUT ldaSellDate) EQ FALSE THEN
      RETURN "ERROR: Date reading failed".

   lcOutputStruct = add_struct(param_toplevel_id, "").
   /*Order struct*/
   add_string(lcOutputStruct, "orderID", 
                             "Y" + STRING(Order.Orderid)).
   add_string(lcOutputStruct, "orderType", lcOrderType). 
   add_string(lcOutputStruct, "orderName", "ALTA").
   add_string(lcOutputStruct, "sellchannel", "YOIGO").
   add_string(lcOutputStruct, "selldate",
              Class.timedate:ConvertToISO8601(ldaSellDate)). 
   add_string(lcOutputStruct, "seller", "YOIGO"). 
   add_string(lcOutputStruct, "createdBy", "YOIGO").
   add_string(lcOutputStruct, "createdDate", 
              Class.timedate:ConvertToISO8601(ldaCreDate)). 
   lcClientStruct = add_struct(lcOutputStruct, "Client").
   add_string(lcClientStruct, "clientID", OrderCustomer.CustId).
   add_string(lcClientStruct, "type", 
                              fgetCustSegment(bordercustomer.CustIdType,
                                              bordercustomer.selfemployed,
                                              bordercustomer.pro,
                                              bordercustomer.custid,  /* YDR-2621 */
                                              OUTPUT lcCategory)). 
   /*Installation*/
   lcInstallationStruct = add_struct(lcOutputStruct, "Installation").
   lcContactStruct = add_struct(lcInstallationStruct, "Contact").
   add_string(lcContactStruct, "firstName", OrderCustomer.FirstName).
   add_string(lcContactStruct, "middleName", OrderCustomer.Surname1).
   IF OrderCustomer.Surname2 NE "" THEN 
      lcLastName = OrderCustomer.Surname2.
   ELSE  
      lcLastName = OrderCustomer.Surname1.
   add_string(lcContactStruct, "lastName", lcLastName).
   add_string(lcContactStruct, "documentNumber", bOrderCustomer.CustID). 
   add_string(lcContactStruct, "documentType", bOrderCustomer.CustIdType).
   add_string(lcContactStruct, "email", OrderCustomer.Email).
   add_string(lcContactStruct, "phoneNumber",(IF OrderCustomer.MobileNumber > ""
                                              THEN OrderCustomer.MobileNumber 
                                              ELSE OrderCustomer.FixedNumber)).

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
   
   IF OrderFusion.IUA NE "" THEN DO:
       fAddCharacteristic(lcCharacteristicsArray, /*base*/
                         "IUA",                   /*param name*/
                         OrderFusion.IUA,         /*param value*/
                         "").                     /*old value*/
      
   END.

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
                         "receptoroperator",        /*param name*/
                         "00031",/*must be 0031*/   /*param value*/
                         "").                      /*old value*/
   END.
   
   IF AVAILABLE bHolderOrderCustomer
   THEN DO:
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "firstName",                     /*param name*/
                         bHolderOrderCustomer.FirstName,  /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "middleName",                    /*param name*/
                         bHolderOrderCustomer.Surname1,   /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "lastName",                      /*param name*/
                         IF bHolderOrderCustomer.Surname2 > "" /*param value*/
                         THEN bHolderOrderCustomer.Surname2
                         ELSE bHolderOrderCustomer.Surname1,
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "documentType",                  /*param name*/
                         bHolderOrderCustomer.CustIdType, /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "documentNumber",                /*param name*/
                         bHolderOrderCustomer.CustId,     /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "stair",                         /*param name*/
                         bHolderOrderCustomer.stair,      /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "letter",                        /*param name*/
                         bHolderOrderCustomer.Letter,     /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "street",                        /*param name*/
                         bHolderOrderCustomer.Street,     /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "street",                        /*param name*/
                         bHolderOrderCustomer.Street,     /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,            /*base*/
                         "bis_duplicate",                   /*param name*/
                         bHolderOrderCustomer.BisDuplicate, /*param value*/
                         "").                               /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "block",                         /*param name*/
                         bHolderOrderCustomer.Block,      /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "door",                          /*param name*/
                         bHolderOrderCustomer.Door,       /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,           /*base*/
                         "number",                         /*param name*/
                         bHolderOrderCustomer.BuildingNum, /*param value*/
                         "").                              /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "km",                            /*param name*/
                         bHolderOrderCustomer.Km,         /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "country",                       /*param name*/
                         bHolderOrderCustomer.Country,    /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "hand",                          /*param name*/
                         bHolderOrderCustomer.Hand,       /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "floor",                         /*param name*/
                         bHolderOrderCustomer.Floor,      /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "zipCode",                       /*param name*/
                         bHolderOrderCustomer.ZipCode,    /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "province",                      /*param name*/
                         bHolderOrderCustomer.Region,     /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "town",                          /*param name*/
                         bHolderOrderCustomer.PostOffice, /*param value*/
                         "").                             /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,          /*base*/
                         "streetType",                    /*param name*/
                         bHolderOrderCustomer.StreetType, /*param value*/
                         "").                             /*old value*/
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
 
      fAddCharacteristic(lcCharacteristicsArray,      /*base*/
                         "UploadSpeed",               /*param name*/
                         CLIType.FixedLineUpload,    /*param value*/
                         "").                         /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,      /*base*/
                         "DownloadSpeed",             /*param name*/
                         CLIType.FixedLineDownload,  /*param value*/
                         "").                         /*old value*/

/* YDR-2532 */

      fAddCharacteristic(lcCharacteristicsArray,      /*base*/
                         "TerritoryOwner",             /*param name*/
                         ENTRY(1,OrderCustomer.TerritoryOwner),  /*param value*/
                         "").                         /*old value*/ 
                           
      fAddCharacteristic(lcCharacteristicsArray, /*base*/
                         "AddressId",            /*param name*/
                         OrderCustomer.AddressId,    /*param value*/
                         "").                   /*old value*/
      fAddCharacteristic(lcCharacteristicsArray,     /*base*/
                         "CaracteristicaTecnica",    /*param name*/
                         "DHCP", /*param value*/
                         "").                         /*old value*/
/* YDR-2532 */

   END.

   IF LENGTH(OrderCustomer.Gescal) < 37 THEN
      lcGescalValue = OrderCustomer.Gescal + FILL(" ",(37 - LENGTH(OrderCustomer.Gescal))).
   ELSE 
      lcGescalValue = OrderCustomer.Gescal.

   fAddCharacteristic(lcCharacteristicsArray, /*base*/
                      "gescal",               /*param name*/
                      lcGescalValue,          /*param value*/
                      "").                    /*old value*/

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).

   xmlrpc_initialize(FALSE).
   fMasXMLGenerate_test("createFixedLine").
   RUN pRPCMethodCall("masmovil.createFixedLine", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      ocResultCode = STRING(gi_xmlrpc_error).
      ocResultDesc = gc_xmlrpc_error.
      RETURN "NW_ERROR".
   END.

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   lcResponse = validate_struct(lcXMLStruct,"resultCode!,resultDescription").
   ocResultCode = get_string(lcXMLSTruct, "resultCode").
   IF LOOKUP('resultDescription', lcXMLSTruct) GT 0 THEN
      ocResultDesc = get_string(lcXMLStruct, "resultDescription").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   IF ocResultCode NE "00" THEN 
      RETURN SUBST("ERROR: Result code &1", ocResultCode).

   RETURN "OK".
 
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
      RETURN "NW_ERROR".
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

   RETURN "OK".

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
   add_string(lcOutputStruct, "cancellationDate",
              Class.timedate:ConvertToISO8601(idaDate)).  
   add_string(lcOutputStruct, "cancellationMotive",  icMotive).

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).

   xmlrpc_initialize(FALSE).
   fMasXMLGenerate_test("CancelFixedLine").
   RUN pRPCMethodCall("masmovil.cancelFixedLine", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      ocResultCode = STRING(gi_xmlrpc_error).
      ocResultDesc = gc_xmlrpc_error.
      RETURN SUBST("ERROR: &1", gc_xmlrpc_error).
   END.

   lcXMLStruct = get_struct(response_toplevel_id,"0").
    
   ASSIGN
      lcResponse = validate_struct(lcXMLStruct,"resultCode!,resultDescription")
      lcResultCode = get_string(lcXMLStruct, "resultCode")
      lcResultDesc = get_string(lcXMLStruct, "resultDescription")
         WHEN LOOKUP('resultDescription', lcResponse) > 0.

   IF gi_xmlrpc_error NE 0 THEN 
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   ASSIGN
      ocResultCode =  lcResultCode
      ocResultDesc =  lcResultDesc.
   
   IF ocResultCode NE "00" THEN
      RETURN SUBST("ERROR: Result code &1", ocResultCode).
   
   RETURN "OK".

END. /*fMasCancel_FixedLineOrder*/


FUNCTION fMasGet_FixedNbr RETURNS CHAR
   (icPostalCode AS CHAR ,
    OUTPUT ocNum AS CHAR,
    OUTPUT ocResultCode AS CHAR,
    OUTPUT ocResultDesc AS CHAR): /*Error message*/

   DEF VAR lcXMLStruct AS CHAR NO-UNDO. /*Input to TMS*/
   DEF VAR lcResponse AS CHAR NO-UNDO.

   add_string(param_toplevel_id, "", icPostalCOde).

   IF gi_xmlrpc_error NE 0 THEN
         RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).
   
   fMasXMLGenerate_test("getnewResource").
   RUN pRPCMethodCall("masmovil.getNewResource", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      ocResultCode = STRING(gi_xmlrpc_error).
      ocResultDesc = gc_xmlrpc_error.
      RETURN "NW_ERROR".
   END.

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   lcResponse = validate_struct(lcXMLStruct,"idNumero,fechaAsignacionCmt,fechaUltimoCambio,numero!,_links").
   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   ocNum = get_string(lcXMLStruct,"numero").
   
   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   IF NOT ocNum > "" THEN
      RETURN "ERROR: Number not returned. Area: " + icPostalCode.

   RETURN "OK".

END.

FUNCTION fMasmovil_ACC RETURNS CHAR
   (iiOrderId AS INT,
    OUTPUT ocResultCode AS CHAR,
    OUTPUT ocResultDesc AS CHAR):

   DEF VAR lcContactStruct AS CHAR NO-UNDO.
   DEF VAR lcXMLStruct AS CHAR NO-UNDO. /*Input to TMS*/
   DEF VAR lcResponse AS CHAR NO-UNDO.

   DEF BUFFER Mobsub        FOR Mobsub.
   DEF BUFFER Order         FOR Order.
   DEF BUFFER bOrder        FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER OrderFusion   FOR OrderFusion.
   DEF BUFFER bActionLog    FOR ActionLog.

   FIND FIRST Order NO-LOCK where 
              Order.Brand EQ Syst.Var:gcBrand AND
              Order.OrderId EQ iiOrderid NO-ERROR.
   IF NOT AVAIL Order THEN 
      RETURN "ERROR: Order not found " + STRING(iiOrderID) .
   
   FIND MobSub NO-LOCK WHERE
        MobSub.MsSeq = Order.MsSeq NO-ERROR.
   IF NOT AVAIL MobSub THEN 
      RETURN "ERROR: Active subscription not found".

   FOR EACH OrderFusion NO-LOCK WHERE
            OrderFusion.FixedNumber = Mobsub.FixedNumber,
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand = Syst.Var:gcBrand AND
            bOrder.OrderID = OrderFusion.OrderID AND
            bOrder.MsSeq = Mobsub.MsSeq AND
            bOrder.StatusCode = {&ORDER_STATUS_DELIVERED} BY bOrder.CrStamp DESC:
       LEAVE.
   END.

   IF NOT AVAIL bOrder THEN DO:
      /* If Order is not available then check if subscription was merged with   */
      /* 2P subscription, if yes then check for merged 2P subscription Order Id */
      FIND FIRST bActionLog NO-LOCK  WHERE
                 bActionLog.Brand     EQ Syst.Var:gcBrand     AND
                 bActionLog.TableName EQ "MobSub"             AND
                 bActionLog.KeyValue  EQ STRING(MobSub.MsSeq) AND
                 bActionLog.ActionID  EQ {&MERGE2P3P}         NO-ERROR.

      IF AVAIL bActionLog THEN DO:
         FOR EACH OrderFusion NO-LOCK WHERE
                  OrderFusion.Brand       EQ Syst.Var:gcBrand                             AND
                  OrderFusion.OrderId     EQ INT(ENTRY(2,bActionLog.ActionChar,CHR(255))) AND
                  OrderFusion.FixedNumber EQ Mobsub.FixedNumber,
             EACH bOrder NO-LOCK WHERE
                  bOrder.Brand      EQ Syst.Var:gcBrand                             AND
                  bOrder.OrderID    EQ INT(ENTRY(2,bActionLog.ActionChar,CHR(255))) AND
                  bOrder.MsSeq      EQ INT(ENTRY(1,bActionLog.ActionChar,CHR(255))) AND
                  bOrder.StatusCode EQ {&ORDER_STATUS_DELIVERED} BY bOrder.CrStamp DESC:
             LEAVE.
         END.
      END.

      IF NOT AVAIL bOrder THEN
         RETURN "ERROR: Work order id not found".
   END.

  /*Use delivery customer information if it is avbailable*/
   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   EQ Syst.Var:gcBrand             AND
              OrderCustomer.OrderId EQ bOrder.OrderId               AND
              OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_ACC} NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN
      RETURN "ERROR: ACC customer data not found " + STRING(iiOrderID) .

   lcContactStruct = add_struct(param_toplevel_id, "").
   /*Order struct*/
   add_string(lcContactStruct, "orderID", 
                             "Y" + STRING(bOrder.Orderid)).
   add_string(lcContactStruct, "documentNumber", OrderCustomer.CustID).
   add_string(lcContactStruct, "documentType", OrderCustomer.CustIdType).
   add_string(lcContactStruct, "firstName", OrderCustomer.FirstName).
   add_string(lcContactStruct, "middleName", OrderCustomer.Surname1).
   add_string(lcContactStruct, "lastName", (IF OrderCustomer.Surname2 NE ""
                                            THEN OrderCustomer.Surname2
                                            ELSE OrderCustomer.Surname1)).
   add_string(lcContactStruct, "email", OrderCustomer.Email).
   add_string(lcContactStruct, "phoneNumber",(IF OrderCustomer.MobileNumber > ""
                                              THEN OrderCustomer.MobileNumber 
                                              ELSE OrderCustomer.FixedNumber)).

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).

   xmlrpc_initialize(FALSE).
   fMasXMLGenerate_test("agreementCustomerChange").
   RUN pRPCMethodCall("masmovil.agreementCustomerChange", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      ocResultCode = STRING(gi_xmlrpc_error).
      ocResultDesc = gc_xmlrpc_error.
      RETURN "NW_ERROR".
   END.

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   lcResponse = validate_struct(lcXMLStruct,"resultCode!,resultDescription").
   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   ocResultCode = get_string(lcXMLSTruct, "resultCode").

   IF LOOKUP('resultDescription', lcResponse) GT 0 THEN
      ocResultDesc = get_string(lcXMLStruct, "resultDescription").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   IF ocResultCode NE "00" THEN 
      RETURN SUBST("ERROR: Result code &1", ocResultCode).

   RETURN "OK".
 
END. /*Function fCreate_FixedLine*/

