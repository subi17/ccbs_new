/**
 * Update delivery address information for an order. Only the fields gives as
 * optional parameters will be changed.
 *
 * @input  tms_id;int;mandatory;OrderId of the order
           actor;string;mandatory;creator of the changes
           new_values;struct;mandatory;struct of the changed fields
 * @newvalues title;string;optional;new title
   first_name;string;optional;new first name
   surname_1;string;optional;new first surname
   surname_2;string;optional;new second surname
   street;string;mandatory;new street address
   zip;string;mandatory;new zipcode
   city;string;mandatory;new city
   region;string;mandatory;new region
   country;string;optional;new country
 * @output bool;TRUE if there was any changes FALSE otherwise.
*/

{xmlrpc/xmlrpc_access.i}
{tmsconst.i}

DEFINE VARIABLE piOrderId                  AS INTEGER   NO-UNDO. 
DEFINE VARIABLE pcCreator                  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcOrderCustomerFields      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOrderCustomerFields      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcParamStructFields        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iField                     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcParamName                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcParamValue               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lChanged                   AS LOGICAL   NO-UNDO. 

DEFINE VARIABLE lcOrderCustomerData        AS CHARACTER EXTENT 14 NO-UNDO.
DEFINE VARIABLE llOrderCustomerDataChanged AS LOGICAL   EXTENT 14 NO-UNDO INIT FALSE. 

DEFINE BUFFER OC FOR OrderCustomer.

/* Eventlog parameters */

DEF VAR scUser AS CHAR NO-UNDO.
scUser = "Newton".
&GLOBAL-DEFINE STAR_EVENT_USER scUser

{lib/eventlog.i}
DEFINE VARIABLE lhOrderCustomer AS HANDLE NO-UNDO.
/* Eventlog definition end */

lChanged = FALSE.

lcOrderCustomerFields = 
   "title,first_name,surname_1,surname_2,street,zip,region,country,city_code,street_code,city,street_number,additional_address,municipality_code".
gcOrderCustomerFields = 
   "title,first_name,surname_1,surname_2,street!,zip!,region!,country,city_code,street_code,city!,street_number,additional_address,municipality_code".
IF  validate_request(param_toplevel_id, "int,string,struct") EQ ? THEN RETURN.

IF gi_xmlrpc_error NE 0 THEN RETURN.

piOrderId  = get_int(param_toplevel_id, "0").
pcCreator  = get_string(param_toplevel_id, "1").

pcStruct            = get_struct(param_toplevel_id, "2").
lcParamStructFields = validate_request(pcStruct, gcOrderCustomerFields).

IF gi_xmlrpc_error NE 0 THEN RETURN.

scUser = "VISTA_" + pcCreator.

FIND Order WHERE
     Order.Brand = "1" AND
     Order.OrderId = piOrderId NO-LOCK NO-ERROR.
IF NOT AVAIL Order THEN RETURN appl_err("Order not avaible").

IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN
   RETURN appl_err("Order is delivered or closed").

IF Order.Logistics NE "" THEN 
   RETURN appl_err("Order is already in logistics handling").

FIND OrderCustomer WHERE 
     OrderCustomer.Brand = "1" AND
     OrderCustomer.OrderId = piOrderId AND
     OrderCustomer.RowType = 4 NO-LOCK NO-ERROR.

IF NOT AVAILABLE OrderCustomer THEN DO:
   
   FIND FIRST OC WHERE
      OC.Brand = "1" AND
      OC.OrderId = piOrderId AND
      OC.RowType = 1 NO-LOCK NO-ERROR.
   IF NOT AVAIL OC THEN RETURN appl_err("OrderCustomer not available"). 

   lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrderCustomer).
   CREATE OrderCustomer.
   ASSIGN
      OrderCustomer.Brand     = "1"
      OrderCustomer.Order     = piOrderId
      OrderCustomer.RowType   = 4
      OrderCustomer.CustTitle = OC.CustTitle
      OrderCustomer.FirstName = OC.FirstName
      OrderCustomer.Surname1  = OC.SurName1
      OrderCustomer.Surname2  = OC.Surname2
      OrderCustomer.Country   = OC.Country
      OrderCustomer.PostOffice = OC.PostOffice
      OrderCustomer.Region    = OC.Region
      OrderCustomer.Address   = OC.Address
      OrderCustomer.ZipCode   = OC.ZipCode
      OrderCustomer.Street    = OC.Street
      OrderCustomer.BuildingNum = OC.BuildingNum
      OrderCustomer.AddressCompl = OC.AddressCompl.
      
   RUN StarEventMakeCreateEvent( lhOrderCustomer ).
END.


ASSIGN
   lcOrderCustomerData[1]  = OrderCustomer.CustTitle
   lcOrderCustomerData[2]  = OrderCustomer.FirstName
   lcOrderCustomerData[3]  = OrderCustomer.SurName1
   lcOrderCustomerData[4]  = OrderCustomer.SurName2
   lcOrderCustomerData[5]  = OrderCustomer.Street
   lcOrderCustomerData[6]  = OrderCustomer.ZipCode
   lcOrderCustomerData[7]  = OrderCustomer.Region
   lcOrderCustomerData[8]  = OrderCustomer.Country
   lcOrderCustomerData[9]  = OrderCustomer.AddressCodP
   lcOrderCustomerData[10] = OrderCustomer.AddressCodC 
   lcOrderCustomerData[11] = OrderCustomer.PostOffice
   lcOrderCustomerData[12] = OrderCustomer.BuildingNum
   lcOrderCustomerData[13] = OrderCustomer.AddressCompl
   lcOrderCustomerData[14] = OrderCustomer.AddressCodM.

 
DO iField = 1 TO NUM-ENTRIES(lcOrderCustomerFields):
   
   lcParamName = ENTRY(iField, lcOrderCustomerFields).
   IF LOOKUP(lcParamName, lcParamStructFields) > 0 THEN
   DO:
      lcParamValue = get_string(pcStruct, lcParamName).
      IF lcOrderCustomerData[iField] ne lcParamValue THEN
      DO:
         lcOrderCustomerData[iField] = lcParamValue.
         llOrderCustomerDataChanged[iField] = TRUE.
         lChanged = TRUE.
      END.
   END.
END.

IF lChanged THEN DO:

   FIND CURRENT OrderCustomer EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   IF LOCKED OrderCustomer THEN RETURN appl_err("Record is in lock!").
    
   lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrderCustomer).
   RUN StarEventSetOldBuffer(lhOrderCustomer).
   
   IF llOrderCustomerDataChanged[1] THEN
       OrderCustomer.CustTitle = lcOrderCustomerData[1].
   IF llOrderCustomerDataChanged[2] THEN
       OrderCustomer.FirstName = lcOrderCustomerData[2].
   IF llOrderCustomerDataChanged[3] THEN
       OrderCustomer.SurName1  = lcOrderCustomerData[3].
   IF llOrderCustomerDataChanged[4] THEN
       OrderCustomer.SurName2  = lcOrderCustomerData[4].
   IF llOrderCustomerDataChanged[5] THEN
       OrderCustomer.Street   = lcOrderCustomerData[5].
   IF llOrderCustomerDataChanged[6] THEN
       OrderCustomer.ZipCode   = lcOrderCustomerData[6].
   IF llOrderCustomerDataChanged[7] THEN
       OrderCustomer.Region    = lcOrderCustomerData[7].
   IF llOrderCustomerDataChanged[8] THEN
       OrderCustomer.Country   = lcOrderCustomerData[8].
   IF llOrderCustomerDataChanged[9] THEN
       OrderCustomer.AddressCodP = lcOrderCustomerData[9].
   IF llOrderCustomerDataChanged[10] THEN
       OrderCustomer.AddressCodC = lcOrderCustomerData[10].
   IF llOrderCustomerDataChanged[11] THEN
       OrderCustomer.PostOffice = lcOrderCustomerData[11].
   IF llOrderCustomerDataChanged[12] THEN
       OrderCustomer.BuildingNum = lcOrderCustomerData[12].
   IF llOrderCustomerDataChanged[13] THEN
       OrderCustomer.AddressCompl = lcOrderCustomerData[13].
   IF llOrderCustomerDataChanged[14] THEN
       OrderCustomer.AddressCodM = lcOrderCustomerData[14].

   /* update completed address */
   IF llOrderCustomerDataChanged[5] OR
      llOrderCustomerDataChanged[12] OR
      llOrderCustomerDataChanged[13] THEN DO:

         OrderCustomer.Address = OrderCustomer.Street .
         IF OrderCustomer.BuildingNum NE "" THEN 
            OrderCustomer.Address = OrderCustomer.Address + " " + OrderCustomer.BuildingNum .
         IF OrderCustomer.AddressCompl NE "" THEN 
            OrderCustomer.Address = OrderCustomer.Address + " " + OrderCustomer.AddressCompl .

   END.
   RUN StarEventMakeModifyEvent(lhOrderCustomer).
   RELEASE OrderCustomer.
END.

add_boolean(response_toplevel_id, "", lChanged).

FINALLY:
   fCleanEventObjects().
END.
