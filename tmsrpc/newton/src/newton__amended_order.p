/**
 * Update Installation address data (by orderId).
 * The data will be changed through msrequest.
 *
 * @input sfid;string;mandatory;salesforce id of shop or callcenter
          orderid;int;mandatory;order number for installation address change
          addressDetails;struct;mandatory;new installation address details
          contractID;string;mandatory;contractID of particular order

 * @addressDetails country;string;
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
DEF VAR pcSfid            AS CHAR NO-UNDO.
DEF VAR piOrderId         AS INT  NO-UNDO.
DEF VAR pcstruct          AS CHAR NO-UNDO.
DEF VAR pcContractId      AS CHAR NO-UNDO.

DEF VAR gcAddressDetails  AS CHAR NO-UNDO.
DEF VAR lcAddressData     AS CHAR NO-UNDO.
DEF VAR pcCountry         AS CHAR NO-UNDO.
DEF VAR pcBis             AS CHAR NO-UNDO.
DEF VAR pcBlock           AS CHAR NO-UNDO.
DEF VAR pcCity            AS CHAR NO-UNDO.
DEF VAR pcCoverage_token  AS CHAR NO-UNDO.
DEF VAR pcAddressId       AS CHAR NO-UNDO.
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

DEF VAR lcCurrentDetails  AS CHAR NO-UNDO.
DEF VAR lcCurrRemDetails  AS CHAR NO-UNDO.
DEF VAR lcNewDetails      AS CHAR NO-UNDO.
DEF VAR lcAmendamentType  AS CHAR NO-UNDO.
DEF VAR lcNewCompAddress  AS CHAR NO-UNDO.

DEF VAR ocResult AS CHAR NO-UNDO.

OUTPUT TO "/home/srvuddan/abc.txt" APPEND.

EXPORT 123.

OUTPUT CLOSE.

/* Eventlog parameters */

DEF NEW SHARED VAR scUser AS CHAR NO-UNDO.
scUser = "Newton".
&GLOBAL-DEFINE STAR_EVENT_USER scUser

{Func/lib/eventlog.i}

FUNCTION fGetAddressFields RETURNS LOGICAL:
   
   IF LOOKUP("country",lcAddressData) GT 0 THEN 
      pcBis = get_string(pcStruct, "country"). 
   IF LOOKUP("bis",lcAddressData) GT 0 THEN 
      pcBis = get_string(pcStruct, "bis").
   IF LOOKUP("block",lcAddressData) GT 0 THEN 
      pcBlock = get_string(pcStruct, "block").     
   
   pcCity = get_string(pcStruct, "city").
   pcCoverage_token = get_string(pcStruct, "coverage_token").
   IF LOOKUP("address_id",lcAddressData) GT 0 THEN 
      pcAddressId = get_string(pcStruct, "address_id").
   IF LOOKUP("door",lcAddressData) GT 0 THEN 
      pcDoor = get_string(pcStruct, "door").
   IF LOOKUP("floor",lcAddressData) GT 0 THEN 
      pcFloor = get_string(pcStruct, "floor").
   
   pcGescal = get_string(pcStruct, "gescal").
   IF LOOKUP("hand",lcAddressData) GT 0 THEN 
      pcHand = get_string(pcStruct, "hand").
   IF LOOKUP("km",lcAddressData) GT 0 THEN 
      pcKm = get_string(pcStruct, "km").
   IF LOOKUP("letter",pcLetter) GT 0 THEN 
      pcLetter = get_string(pcStruct, "letter").
      
   pcRegion = get_string(pcStruct, "region").   
   IF LOOKUP("stair",pcStair) GT 0 THEN 
      pcStair = get_string(pcStruct, "stair").
   
   pcStreet_name = get_string(pcStruct, "street_name").
   pcStreet_number = get_string(pcStruct, "street_number").
   pcTerritory_owner = get_string(pcStruct, "territory_owner").
   pcStreet_type = get_string(pcStruct, "street_type").
   pcZip = get_string(pcStruct, "zip").
    
END FUNCTION.

ASSIGN 
   gcAddressDetails = "country,bis,block,city,coverage_token,door,floor,
      gescal,hand,km,letter,region,stair,street_name,street_number,territory_owner,
      street_type,zip".

IF validate_request(param_toplevel_id, "string,int,struct,string") EQ ? THEN
   RETURN.

IF gi_xmlrpc_error NE 0 THEN RETURN.
  
pcContractId = get_string(param_toplevel_id, "3").
pcstruct = get_struct(param_toplevel_id, "2").
piOrderId = get_int(param_toplevel_id, "1").
pcSfid = get_string(param_toplevel_id, "0").
scUser = "VISTA_" + pcSfid. /* Read from eventlog functions into eventlog.user */
Syst.Var:katun = "VISTA_" + pcSfid.

IF gi_xmlrpc_error NE 0 THEN RETURN.

/* validate order address struct */
lcAddressData = validate_request(pcstruct,gcAddressDetails).
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
IF LOCKED OrderCustomer THEN 
   RETURN appl_err("Installation_address_change_failed due to ordercustomer is locked"). 

IF NOT AVAILABLE OrderCustomer THEN 
   RETURN appl_err("Installation address possible for convergent order only").
   
FIND FIRST OrderFusion NO-LOCK WHERE
           OrderFusion.Brand EQ Syst.Var:gcBrand AND
           OrderFusion.OrderID EQ piOrderId NO-ERROR.
IF NOT AVAIL OrderFusion THEN
   RETURN appl_err("Fixed line connection is not available for this order").
   
IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_CANCELLED} OR
   OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_PENDING_CANCELLED} OR
   OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_ERROR} OR
   OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_REJECTED} THEN 
   RETURN appl_err("Order is not in valid state to update").
   
ASSIGN 
   lcAmendamentType = ""
   lcCurrentDetails = OrderCustomer.StreetType + " " + OrderCustomer.Street + " " +
      OrderCustomer.BuildingNum + " " + OrderCustomer.Floor + " " + OrderCustomer.Door 
      + " " + OrderCustomer.Letter + " " + OrderCustomer.Stair + " " + OrderCustomer.Block 
      + " " + OrderCustomer.BisDuplicate
   lcCurrRemDetails = OrderCustomer.ZipCode + " " + OrderCustomer.PostOffice
      + " " + OrderCustomer.Gescal       
   lcNewDetails = pcStreet_type + " " + pcStreet_name + " " + pcStreet_number + " " +
      pcFloor + " " + pcDoor + " " + pcLetter + " " + pcStair + " " + pcBlock + " " +
      pcBis + " " + pcZip + " " + pcCity + " " + pcGescal
   lcNewCompAddress = pcAddressId + " " + pcGescal + " " + pcCountry + " " + pcDoor + " " + 
      pcZip + " " + pcStreet_type + " " + pcKm + " " + pcStair + " " + pcCity + " " + pcStreet_number
      + " " + pcRegion + " " + pcStreet_name + " " + pcLetter + " " + pcBis + " " + pcBlock + " " +
      pcFloor + " " + pcHand.
   
fUpdateInstallAddressRequest(
                             piOrderId,
                             0,
                             lcAmendamentType,
                             lcCurrentDetails,
                             lcCurrRemDetails,
                             lcNewDetails,
                             lcNewCompAddress,
                             ({&REQUEST_SOURCE_NEWTON}),
                             0,
                             OUTPUT ocResult).   
/*
fCreateFusionAddressChangeMessage(Order.OrderID,
                                  OUTPUT lcError).
                                  
   FIND FIRST OrderCustomer WHERE 
              OrderCustomer.Brand = Syst.Var:gcBrand AND 
              OrderCustomer.OrderId = piOrderId AND 
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_BILLING}
              EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL OrderCustomer THEN DO:
      ASSIGN .
         /* update order customer billing address */    
   END.                              
END. 
ELSE  
   RETURN appl_err("Installation address update failed in MM!"). 
             */
add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
    
END.
