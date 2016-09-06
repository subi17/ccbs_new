/* ----------------------------------------------------------------------
  MODULE .......: masmovileif.i
  TASK .........: Functions for masmovile integration
  APPLICATION ..: TMS
  AUTHOR .......: ilsavola
  CREATED ......: 29.8.2016
  CHANGED ......:
  Version ......: Yoigo
----------------------------------------------------------------------- */

{/apps/yoigo/tms/Syst/commali.i}
{/apps/yoigo/tms/Func/cparam2.i}
{/apps/yoigo/tms/Func/msreqfunc.i}
{/apps/yoigo/tms/Syst/tmsconst.i}
{xmlrpc/xmlrpc_client.i}
{/apps/yoigo/tms/Func/forderstamp.i}


/*Global variables for building masmovile data*/

DEF VAR lcConURL AS CHAR NO-UNDO.

FUNCTION fInitMMConnection RETURNS CHAR
   ():
   lcConURL = fCParam("URL","urlMasmovil").
   IF lcConURL = ? OR lcConURL = "" THEN 
      RETURN "ERROR in connection settings".
   initialize(lcConURL, 15).
   RETURN "".
END.


FUNCTION fMasCreate_FixedLine RETURNS CHAR
   (iiOrderId AS INT,
    OUTPUT ocResultCode AS CHAR,
    OUTPUT ocResultDesc AS CHAR):
   DEF VAR lcOrderStruct AS CHAR NO-UNDO.
   DEF VAR lcServiceArray AS CHAR NO-UNDO.
   DEF VAR lcContactStruct AS CHAR NO-UNDO.
   DEF VAR lcAddressStruct AS CHAR NO-UNDO.
   DEF VAR lcOutputStruct AS CHAR NO-UNDO.
   DEF VAR lcServiceStruct AS CHAR NO-UNDO.
   DEF VAR lcCharacteristicsArray AS CHAR NO-UNDO.
   DEF VAR lcCharacteristicStruct AS CHAR NO-UNDO.
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
   DEF VAR ldaSellDate AS DATE.
   DEF VAR ldaCreDate AS DATE.
   DEF VAR lcResult AS CHAR NO-UNDO.

   DEF BUFFER bOrder FOR Order.
   DEF BUFFER bOC FOR OrderCustomer.

   FIND FIRST bOrder NO-LOCK where 
              bOrder.Brand EQ gcBrand AND
              bOrder.OrderId EQ iiOrderid NO-ERROR.
   IF NOT AVAIL bOrder THEN 
      RETURN "Error: Order not found " + STRING(iiOrderID) .

  /*Use delivery customer information if it is avbailable*/
   FIND FIRST bOC NO-LOCK where 
              bOC.Brand EQ gcBrand AND
              bOC.OrderId EQ iiOrderid AND 
              bOC.RowType EQ 4
              NO-ERROR.
   IF NOT AVAIL bOC THEN DO:
      FIND FIRST bOC NO-LOCK where 
                 bOC.Brand EQ gcBrand AND
                 bOC.OrderId EQ iiOrderid AND 
                 bOc.RowType EQ 1 /*This customer should be available*/
                 NO-ERROR.
      IF NOT AVAIL bOC THEN 
         RETURN "Error: Customer data not found " + STRING(iiOrderID) .

   END.
   /*Generate order type*/
   IF bOrder.CliType BEGINS "CONTDSL" OR 
      bOrder.CliType BEGINS "CONTFH" THEN DO: 
      FIND FIRST CLIType NO-LOCK WHERE
                 CLIType.CLIType EQ bOrder.CliType NO-ERROR.
      IF AVAIL CLIType THEN DO:
         IF CLIType.FixedLineType EQ 1 THEN DO:
            lcOrderType = "Alta xDSL + VOIP".
            lcConnServiceId = "ADSL".
            lcConnServiceName = "ADSL connection".
            lcConnServiceType = "ADSL".
         END.
         ELSE IF CLIType.FixedLineType EQ 2 THEN DO:
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

   IF fTS2Date(bOrder.CrStamp, ldaCreDate) EQ FALSE THEN
      RETURN "Error: Date reading failed".

   IF fTS2Date(bOrder.CrStamp, ldaCreDate) EQ FALSE THEN
      RETURN "Error: Date reading failed".

   lcOutputStruct = add_struct(param_toplevel_id, "").
   /*Order struct*/
   lcOrderStruct = add_struct(lcOutputStruct,"Order").
   add_string(lcOrderStruct, "orderID", 
                             "Y" + STRING(bOrder.Orderid)).
   add_string(lcOrderStruct, "orderType", lcOrderType).  
   add_string(lcOrderStruct, "sellchannel", "YOIGO"/*bOrder.orderchannel*/).
   add_string(lcOrderStruct, "selldate", STRING(bOrder.CrStamp)). 
   add_string(lcOrderStruct, "seller", /*bOrder.Salesman*/ "YOIGO"). 
   add_string(lcOrderStruct, "createdBy", "YOIGO").
   add_string(lcOrderStruct, "creadate", STRING(bOrder.CrStamp)). 

   /*Installation*/
   lcInstallationStruct = add_struct(lcOrderStruct,"Installation").
   lcContactStruct = add_struct(lcInstallationStruct,"Contact").
   add_string(lcContactStruct, "firstName", bOC.FirstName).
   add_string(lcContactStruct, "lastName", bOC.Surname1 + " " + bOC.Surname2).
   add_string(lcContactStruct, "documentNumber",bOC.CustID). 
   add_string(lcContactStruct, "documentType", bOC.CustIdType).
   add_string(lcContactStruct, "Email", bOC.Email).
   add_string(lcContactStruct, "phoneNumber", bOC.ContactNum).

   lcAddressStruct = add_struct(lcInstallationStruct,"Address").
   add_string(lcAddressStruct, "country", bOC.Country).
   add_string(lcAddressStruct, "province",bOC.Region).
   add_string(lcAddressStruct, "town",bOC.PostOffice).
   add_string(lcAddressStruct, "street", bOC.Street).
   add_string(lcAddressStruct, "number", bOc.BuildingNum).
/*   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).*/
   add_string(lcAddressStruct, "zipCode",bOc.ZipCode).

   lcServiceArray = add_array(lcOrderStruct,"Services").
    /*Services entry - Phone*/
   lcServiceStruct = add_struct(lcServiceArray, ""). 
   add_string(lcServiceStruct, "serviceID", "PHONE").
   add_string(lcServiceStruct, "action", "Add").
   add_string(lcServiceStruct, "type", "Fixed line phone").

   /*Characteristics for the service*/
   lcCharacteristicsArray = add_array(lcServiceStruct,"Characteristics").
   lcCharacteristicStruct = add_struct(lcCharacteristicsArray, 
                                        "Characteristic").
   add_string(lcCharacteristicStruct, "name", "TEST").
   add_string(lcCharacteristicStruct, "value", "new value TEST").

   /*Services entry - Line*/
   lcServiceStruct = add_struct(lcServiceArray, ""). 
   add_string(lcServiceStruct, "serviceID", lcConnServiceId).
   add_string(lcServiceStruct, "action", "Add").
   add_string(lcServiceStruct, "type", lcConnServiceType).
   
   /*Characteristics for the service*/
   lcCharacteristicsArray = add_array(lcServiceStruct,"Characteristics").
   lcCharacteristicStruct = add_struct(lcCharacteristicsArray, 
                                        "Characteristic").
   add_string(lcCharacteristicStruct, "name", "TEST").
   add_string(lcCharacteristicStruct, "value", "new value TEST").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).
IF 0 > 1 THEN RETURN "TEST RESULT OK".
   RUN pRPCMethodCall("masmovile.CreateFixedLine", TRUE).

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
   add_string(lcOutputStruct, "llationMotive",  icMotive).

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error).

   RUN pRPCMethodCall("masmovile.CancelFixedLine", TRUE).

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

   RUN pRPCMethodCall("masmovile.getnewResource", TRUE).

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








