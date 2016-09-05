/* ----------------------------------------------------------------------
  MODULE .......: masmovileif.i
  TASK .........: Functions for masmovile integration
  APPLICATION ..: TMS
  AUTHOR .......: ilsavola
  CREATED ......: 29.8.2016
  CHANGED ......:
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{msreqfunc.i}
{tmsconst.i}
{xmlrpc/xmlrpc_client.i}
{forderstamp.i}


/*Global variables for building masmovile data*/

DEF VAR lcConURL AS CHAR NO-UNDO.

FUNCTION fInitMMConnection RETURNS CHAR
   ():
   lcConURL = fCParam("URL","urlMasmovil").
   IF lcConURL = ? OR lcConURL = "" THEN RETURN "ERROR".
   initialize(lcConURL, 15).
   RETURN "".
END.


FUNCTION fCreateMas_FixedLine RETURNS CHAR
   (iiOrderId AS INT):
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
   DEF VAR lcResultCode AS CHAR NO-UNDO.
   DEF VAR lcResultDesc AS CHAR NO-UNDO.
   DEF VAR lcConnServiceId AS CHAR NO-UNDO.
   DEF VAR lcConnServiceName AS CHAR NO-UNDO.
   DEF VAR lcConnServiceType AS CHAR NO-UNDO.
   DEF VAR lcInstallationStruct AS CHAR NO-UNDO.

   DEF BUFFER bOrder FOR Order.
   DEF BUFFER bOC FOR OrderCustomer.

   FIND FIRST bOrder NO-LOCK where 
              bOrder.Brand EQ gcBrand AND
              bOrder.OrderId EQ iiOrderid NO-ERROR.
   IF NOT AVAIL bOrder THEN 
      RETURN "fCreate_FixedLine: Order not found " + STRING(iiOrderID) .
   
   /*Generate order type*/
   IF bOrder.CliType EQ "CONTS2GB" OR 
      bOrder.CliType EQ "CONTS8GB" THEN DO: 
      FIND FIRST CLIType NO-LOCK WHERE
                 CLIType.CLIType EQ bOrder.CliType NO-ERROR.
      IF AVAIL CLIType THEN DO:
         IF CLIType.CLIType EQ "1" THEN DO:
            lcOrderType = "Alta xDSL + VOIP".
            lcConnServiceId = "ADSL".
            lcConnServiceName = "ADSL connection".
            lcConnServiceType = "ADSL".

         END.
         ELSE DO:
            lcOrderType = "Alta FTTH + VOIP".
            lcConnServiceId = "FTTH".
            lcConnServiceName = "FTTH connection".
            lcConnServiceType = "FTTH".           
         END.   
      END.
   END.
   ELSE 
      RETURN "Error Not allowed CLITYPE " + bOrder.CliType.
   
   lcOutputStruct = add_struct(param_toplevel_id, "").

   /*Order struct*/
   lcOrderStruct = add_struct(lcOutputStruct,"Order").
   add_string(lcOrderStruct, "orderID", 
                             "Y" + STRING(bOrder.Orderid)).
   add_string(lcOrderStruct, "orderType", lcOrderType).  
   add_string(lcOrderStruct, "sellchannel", bOrder.orderchannel).
   add_string(lcOrderStruct, "selldate", STRING(bOrder.CrStamp)).
   add_string(lcOrderStruct, "seller", /*bOrder.Salesman*/ "YOIGO"). 
   add_string(lcOrderStruct, "createdBy", "YOIGO").
   add_string(lcOrderStruct, "creadate", STRING(bOrder.CrStamp)).

   /*Installation*/
   lcInstallationStruct = add_struct(lcOrderStruct,"Installation").
   lcContactStruct = add_struct(lcInstallationStruct,"Contact").
   add_string(lcContactStruct, "firstName", bOC.FirstName).
   add_string(lcContactStruct, "lastName", bOC.Surname1 + " " + bOC.Surname2).
   add_string(lcContactStruct, "documentNumber","8888008").
   add_string(lcContactStruct, "documentType","Passport").
   add_string(lcContactStruct, "Email",bOC.Email).
   add_string(lcContactStruct, "phoneNumber",bOC.ContactNum).

   lcOrderStruct = add_struct(lcInstallationStruct,"Address").
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
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
   add_string(lcAddressStruct, "",).
*/
   lcServiceArray = add_array(lcOrderStruct,"Services").
    /*Services entry - Phone*/
   lcServiceStruct = add_struct(lcServiceArray, ""). 
   add_string(lcServiceStruct, "serviceID", "PHONE").
   add_string(lcServiceStruct, "action", "Add").
   add_string(lcServiceStruct, "type", "Fixed line phone").
 

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

   RUN pRPCMethodCall("masmovile.CreateFixedLine", TRUE).

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("NW_ERROR: &1", gc_xmlrpc_error).

   lcXMLStruct = get_struct(response_toplevel_id,"0").
   lcResultCode = get_string(lcXMLStruct, "resultCode").
   lcResultDesc = get_string(lcXMLStruct, "resultDescription").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   IF lcResultCode NE "" THEN RETURN lcResultCode + "," + lcResultDesc.

   RETURN "".
END. /*Function fCreate_FixedLine*/



/*
FUNCTION fGetMas_FixedNbr RETURNS CHAR
   ():


END.


  */ 






