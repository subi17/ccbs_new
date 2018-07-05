/**
 * Update Oder data (by orderId).
 * The data will be changed through msrequest.
 *
 * @input sfid;string;mandatory;salesforce id of shop or callcenter
          orderid;int;mandatory;order number to modify 
          OrderDetails;struct;mandatory;details to update
          changeType;string;mandatory;type of change applied
          contractID;string;mandatory;contractID of particular order
          reason;string;optional;reason for updating details

 * @addressDetails country;string;mandatory
                   bis;string;optional
                   block;string;optional
                   city;string;mandatory
                   coverage_token;string;mandatory
                   door;string;optional
                   floor;string;optional
                   gescal;string;mandatory
                   hand;string;optional
                   km;string;optional
                   letter;string;optional
                   region;string;mandatory
                   stair;string;optional
                   street_name;string;mandatory
                   street_number;string;mandatory
                   territory_owner;string;mandatory
                   street_type;string;mandatory
                   zip;string;mandatory
 
 * @output success;int 0 = successful;otherwise error
 */
 
USING Progress.Json.ObjectModel.*.

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/fmakemsreq.i}
 
/* Input parameters */
DEF VAR pcSalesManId      AS CHAR NO-UNDO.
DEF VAR piOrderId         AS INT  NO-UNDO.
DEF VAR pcAmendmentStruct AS CHAR NO-UNDO.
DEF VAR pcAmendmentType   AS CHAR NO-UNDO.
DEF VAR pcContractId      AS CHAR NO-UNDO.
DEF VAR pcReason          AS CHAR NO-UNDO.
DEF VAR ocResult          AS CHAR NO-UNDO.

DEF VAR gcAmendmentDetails AS CHAR NO-UNDO.
DEF VAR lcAddressData     AS CHAR NO-UNDO.
DEF VAR pcAddressId       AS CHAR NO-UNDO.
DEF VAR pcCountry         AS CHAR NO-UNDO.
DEF VAR pcBis             AS CHAR NO-UNDO.
DEF VAR pcBlock           AS CHAR NO-UNDO.
DEF VAR pcCity            AS CHAR NO-UNDO.
DEF VAR pcCoverage_token  AS CHAR NO-UNDO.
DEF VAR pcDoor            AS CHAR NO-UNDO.
DEF VAR pcFloor           AS CHAR NO-UNDO.
DEF VAR pcGescal          AS CHAR NO-UNDO.
DEF VAR pcHand            AS CHAR NO-UNDO.
DEF VAR pcKm              AS CHAR NO-UNDO.
DEF VAR pcLetter          AS CHAR NO-UNDO.
DEF VAR pcRegion          AS CHAR NO-UNDO.
DEF VAR pcStair           AS CHAR NO-UNDO.
DEF VAR pcStreet_name     AS CHAR NO-UNDO.
DEF VAR pcStreet_number   AS CHAR NO-UNDO.
DEF VAR pcTerritory_owner AS CHAR NO-UNDO.
DEF VAR pcStreet_type     AS CHAR NO-UNDO.
DEF VAR pcZip             AS CHAR NO-UNDO.

/* local variables */
DEF VAR lcCurrentDetails AS CHAR NO-UNDO.
DEF VAR lcAmendmentValue AS CHAR NO-UNDO.
DEF VAR lcAmendmentType  AS CHAR NO-UNDO.
DEF VAR lcRegion         AS CHAR NO-UNDO.

/* Eventlog parameters */

DEF NEW SHARED VAR scUser AS CHAR NO-UNDO.
scUser = "Newton".
&GLOBAL-DEFINE STAR_EVENT_USER scUser

{Func/lib/eventlog.i}

FUNCTION fGetAddressFields RETURNS LOGICAL:
   
   IF LOOKUP("country",lcAddressData) GT 0 THEN 
      pcBis = get_string(pcAmendmentStruct, "country"). 
   IF LOOKUP("bis",lcAddressData) GT 0 THEN 
      pcBis = get_string(pcAmendmentStruct, "bis").
   IF LOOKUP("block",lcAddressData) GT 0 THEN 
      pcBlock = get_string(pcAmendmentStruct, "block").     
   
   pcCity = get_string(pcAmendmentStruct, "city").
   pcCoverage_token = get_string(pcAmendmentStruct, "coverage_token").
   IF LOOKUP("address_id",lcAddressData) GT 0 THEN 
      pcAddressId = get_string(pcAmendmentStruct, "address_id").
   IF LOOKUP("door",lcAddressData) GT 0 THEN 
      pcDoor = get_string(pcAmendmentStruct, "door").
   IF LOOKUP("floor",lcAddressData) GT 0 THEN 
      pcFloor = get_string(pcAmendmentStruct, "floor").
   
   pcGescal = get_string(pcAmendmentStruct, "gescal").
   IF LOOKUP("hand",lcAddressData) GT 0 THEN 
      pcHand = get_string(pcAmendmentStruct, "hand").
   IF LOOKUP("km",lcAddressData) GT 0 THEN 
      pcKm = get_string(pcAmendmentStruct, "km").
   IF LOOKUP("letter",pcLetter) GT 0 THEN 
      pcLetter = get_string(pcAmendmentStruct, "letter").
      
   pcRegion = get_string(pcAmendmentStruct, "region").   
   IF LOOKUP("stair",pcStair) GT 0 THEN 
      pcStair = get_string(pcAmendmentStruct, "stair").
   
   pcStreet_name = get_string(pcAmendmentStruct, "street_name").
   pcStreet_number = get_string(pcAmendmentStruct, "street_number").
   pcTerritory_owner = get_string(pcAmendmentStruct, "territory_owner").
   IF LOOKUP("street_type",pcStreet_type) GT 0 THEN
   pcStreet_type = get_string(pcAmendmentStruct, "street_type").
   
   pcZip = get_string(pcAmendmentStruct, "zip").
    
END FUNCTION.

ASSIGN 
   gcAmendmentDetails = "country,bis,block,city,coverage_token,door,floor,gescal,hand,km,letter,region,stair,street_name,street_number,territory_owner,street_type,zip".

IF validate_request(param_toplevel_id, "string,int,struct,string,string,string") EQ ? THEN
   RETURN.

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcReason = get_string(param_toplevel_id, "5").
pcContractId = get_string(param_toplevel_id, "4").
pcAmendmentType = get_string(param_toplevel_id, "3").
pcAmendmentStruct = get_struct(param_toplevel_id, "2").  
piOrderId = get_int(param_toplevel_id, "1").
pcSalesManId = get_string(param_toplevel_id, "0").
scUser = "VISTA_" + pcSalesManId. /* Read from eventlog functions into eventlog.user */
Syst.Var:katun = "VISTA_" + pcSalesManId.

IF gi_xmlrpc_error NE 0 THEN RETURN.

/* validate order address struct */
lcAddressData = validate_request(pcAmendmentStruct,gcAmendmentDetails).
IF lcAddressData EQ ? THEN RETURN.

fGetAddressFields().
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* Busines logic validations */
{newton/src/findtenant.i YES ordercanal Order OrderId piOrderId}

FIND FIRST Order WHERE 
           Order.Brand EQ Syst.Var:gcBrand AND
           Order.OrderId EQ piOrderId 
           NO-LOCK NO-ERROR.
IF NOT AVAIL Order THEN 
   RETURN appl_err("OrderId is invalid").
   
FIND FIRST OrderCustomer WHERE 
           OrderCustomer.Brand   EQ Syst.Var:gcBrand AND
           OrderCustomer.OrderId EQ piOrderId        AND
           OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
           NO-LOCK NO-ERROR.                          
IF NOT AVAILABLE OrderCustomer THEN 
   RETURN appl_err("Installation address possible for convergent order only").
   
FIND FIRST OrderFusion NO-LOCK WHERE
           OrderFusion.Brand EQ Syst.Var:gcBrand AND
           OrderFusion.OrderID EQ piOrderId NO-ERROR.
IF NOT AVAIL OrderFusion THEN
   RETURN appl_err("Fixed line connection is not available for this order").
   
IF AVAIL OrderFusion THEN DO:
   IF LOOKUP(OrderFusion.FixedStatus,"CERRADA,CERRADA PARCIAL,CANCELACION EN PROCESO,CANCELADA,En proceso,EN PROCESO - NO CANCELABLE") > 0 THEN
      RETURN appl_err("Order is not in valid state to update").
END.

IF pcRegion EQ "ISLAS BALEARES" THEN lcRegion = "Baleares".
IF pcRegion EQ "TENERIFE" THEN lcRegion = "Sta.Cruz Tenerife".
IF pcRegion EQ "LLEIDA" THEN lcRegion = "Lérida".

FIND FIRST Region NO-LOCK WHERE
           Region.RgName EQ lcRegion NO-ERROR.
IF NOT AVAIL Region THEN RETURN appl_err("Given Region is Invalid"). 

ASSIGN
   lcAmendmentType = pcAmendmentType
   lcCurrentDetails = OrderCustomer.StreetType + "|" + OrderCustomer.Street + "|" + ~
       OrderCustomer.BuildingNum + "|" + OrderCustomer.Floor + "|" + OrderCustomer.Door ~
      + "|" + OrderCustomer.Letter + "|" + OrderCustomer.Stair + "|" + OrderCustomer.Block ~
      + "|" + OrderCustomer.BisDuplicate + "|" + OrderCustomer.ZipCode + "|" + OrderCustomer.PostOffice ~
      + "|" + OrderCustomer.Gescal
   lcAmendmentValue = pcStreet_type + "|" + pcStreet_name + "|" + pcStreet_number ~
      + "|" + pcFloor + "|" + pcDoor + "|" + pcLetter + "|" + pcStair + "|" + pcBlock + "|" + ~
      pcBis + "|" + pcZip + "|" + pcCity + "|" + pcGescal + "|" + pcAddressId + "|" +  pcCountry + "|" +  ~
      pcKm + "|" +    lcRegion + "|" + pcHand + "|" + pcTerritory_owner.

fUpdateInstallAddressRequest(
                             pcSalesManId,
                             piOrderId,
                             0,
                             lcAmendmentType,
                             lcAmendmentValue,
                             lcCurrentDetails,
                             pcContractId,
                             pcReason,
                             ({&REQUEST_SOURCE_NEWTON}),
                             0,
                             OUTPUT ocResult).   
                           
add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
    
END.