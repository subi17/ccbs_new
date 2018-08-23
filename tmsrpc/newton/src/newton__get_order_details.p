/**
 * Get order details
 *
 * @input int;mandatory;orderid of the order for which details are requested.
 * @orders tms_id;int;tms internal order id
      status;string;order status code
      dms_status_code;string;dms status code
      dms_status_desc;string;dms status description
      order_channel;string;channel where order was made from
      contract_number;string;external order id (web side order id)
      salesman_id;string;salesman of order
      campaign;string;campaign code (reason for discount)
      payment_type;string;'order_payment' or 'credit_card'
      orderer_ip;string;IP address where order was made from
      creditcard_ref;string;credit card reference
      creation_time;datetime;time when customer accepted order price
      referee;string;msisdn of referee subscription (MGM commission)
      order_type;int;Order Type
      delivery_type;int;Delivery Type
      delivery_secure;int;Delivery Secure
      reverted_renewal_order;boolean;reverted delivered POS renewal order
      terminal_return_contracts;string;Array of contract ids associated with Order
      extension_contracts;string;Array of extension contract ids associated with Order
 *  @subscription msseq;int;subscription id
                 icc;string;subscription icc 
                 cli;string;subscription msisdn
                 subscription_type;string;subscription type (CONT,TARJ,...)
                 number_type;string;'PostPaid' or 'PrePaid'
                 old_operator;string;old operator name (MNP)
                 old_icc;string;old icc (MNP)
                 old_payment_method;string;old payment method ('PostPaid' or 'PrePaid) (MNP)
                 offer_id;string;offer id 
                 logistics_file;string;logistics file name (Dextra file) 
                 data_bundle_id;string;data bundle id for subscription_type
                 msstatus;int;mobile line status
 * @mnp status;string;mnp status (AENV,ASOL,...)
      tms_request_id;string;TMS request id
      mnp_request_id;string;MNP request id
      id;int;TMS internal request id
 * @mnpmessages;array of mnpmessage structs;mnp messages of single mnp process
      type;string;mnp message type
      from;string;origin of message "TMS" or "MNP"
      date_time;datetime;creation time
      status;string;message has change process to this status (NEW,ASOL,...)
 * @order_terminals array of order_terminal structs;order terminals
 * @order_terminal billing_item_id;string;terminal productcode
             imei;string;terminal IMEI
             delivered;datetime;order creation stamp
             discount;double;terminal discount
             amount;double;terminal or terminal offeritem amount
             vat_amount;double;always 0
             hard_book;integer;hard booking
             hard_book_state;string;hard book state
 * @order_laptops array of laptop structs;order laptops
 * @laptop      laptop;string;laptop billing item
                ordered;datetime;order creation stamp
                serial_number;string;laptop serial number
   @delivery_address street_code;string;address validation result code
             city_code;string;address validation result code
             municipality_code;string;address validation result code
             title;string;
             first_name;string;
             surname_1;string;
             surname_2;string;
             zip_code;string;
             region;string;
             kiala_code;string;
             site_name;string;
   @secure_address street_code;string;
                   address validation result code city_code;string;
                   address validation result code;string;
                   municipality_code;string;
                   address validation result code;string;
                   zip_code;string;
                   region;string;
                   city;string;
                   country;string;
 * @fusion_data  fixed_number_type;string;NEW/MNP
                 fixed_line_mnp_old_operator_name;string;
                 fixed_line_mnp_old_operator_code;string;
                 fixed_line_serial_number;string;
                 fixed_line_adsl_linkstate;string;optional;ADSL link state
                 fixed_mnp_time_of_change;string;
                 fixed_line_number;string;
                 fusion_order_status;string;
                 fixed_line_order_status;string;
                 fixed_line_product;string;fusion order product code
                 fixed_line_appointment_date;string;installation appointment date
                 customer_type;string;
                 allow_mobile_order_release;boolean;For Fusion orders
                 install_address;struct;
                 billing_address;struct;
                 update_ts;string;optional
  @install_address fname;string;
                    lname;string;
                    surname_2;string;
                    phone_number;string;
                    address;string;
                    city;string;
                    zip;string;
                    street_number;string;
                    additional_address;string;
                    region;string;
                    profession;string;
                    email;string;
                    gescal;string;
                    address_compl;string;optional;Address Complement
                    floor;string;optional;Complement for floor
                    street_type;string;optional;street type
                    bis_duplicate;string;optional;bis
                    block;string;optional;block
                    door;string;optional;door
                    letter;string;optional;letter
                    stair;string;optional;stair
                    hand;string;optional;hand
                    km;string;optional;km
  @billing_address address;string;
                    city;string;
                    zip;string;
                    street_number;string;
                    additional_address;string;
                    region;string;
 */
                 


{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:katun = "NewtonRPC".
Syst.Var:gcBrand = "1".
{Func/terminaloffer.i}
{Syst/tmsconst.i}
{Mm/fbundle.i}

DEF VAR piOrderId               AS INTEGER   NO-UNDO. 
DEF VAR top_struct              AS CHARACTER NO-UNDO. 
DEF VAR gcSubscription          AS CHARACTER NO-UNDO. 
DEF VAR gcArrayMnp              AS CHARACTER NO-UNDO. 
DEF VAR gcStructMnp             AS CHARACTER NO-UNDO. 
DEF VAR gcArrayMnpMessages      AS CHARACTER NO-UNDO. 
DEF VAR gcStructMnpMessage      AS CHARACTER NO-UNDO. 
DEF VAR lcOrderTerminals        AS CHARACTER NO-UNDO. 
DEF VAR lcOrderTerminal         AS CHARACTER NO-UNDO. 
DEF VAR gcStructDeliveryAddress AS CHARACTER NO-UNDO. 
DEF VAR gcStructSecureAddress   AS CHARACTER NO-UNDO.
DEF VAR lcFusionStruct          AS CHARACTER NO-UNDO. 
DEF VAR laptop_array            AS CHARACTER NO-UNDO.
DEF VAR term_struct             AS CHARACTER NO-UNDO.
DEF VAR accessory_struct        AS CHARACTER NO-UNDO.

DEF VAR lcPaymentType           AS CHARACTER NO-UNDO. 
DEF VAR cMnpMsgStatus           AS CHARACTER NO-UNDO. 
DEF VAR lcMNPStatus             AS CHARACTER NO-UNDO. 
DEF VAR lcTerminalCode          AS CHARACTER NO-UNDO. 
DEF VAR iTerminalOfferItemId    AS INTEGER   INIT -1 NO-UNDO. 
DEF VAR llPortTimeIsProp        AS LOGICAL  INIT TRUE NO-UNDO.
DEF VAR lcDataBundle            AS CHARACTER NO-UNDO.
DEF VAR lcBundleCLITypes        AS CHARACTER NO-UNDO.
DEF VAR lcFixedInstallAddress   AS CHAR NO-UNDO. 
DEF VAR lcFixedBillingAddress   AS CHAR NO-UNDO. 
DEF VAR lcDMSStatusCode         AS CHARACTER NO-UNDO.
DEF VAR lcDMSStatusDesc         AS CHARACTER NO-UNDO.

DEF VAR lcTermReturnContracts   AS CHARACTER NO-UNDO.
DEF VAR lcExtensionContracts    AS CHARACTER NO-UNDO.

DEF BUFFER bMsRequest FOR MsRequest.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piOrderId = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i YES ordercanal Order OrderId piOrderId}

FIND FIRST DMS NO-LOCK WHERE
           DMS.HostTable EQ {&DMS_HOST_TABLE_ORDER} AND
           DMS.HostId EQ Order.OrderId AND
           DMS.StatusTS < Func.Common:mMakeTS() NO-ERROR.
IF AVAIL DMS THEN DO:
   lcDmsStatusCode = DMS.StatusCode.
   lcDmsStatusDesc = DMS.StatusDesc.
END.

FIND OrderPayment WHERE 
     OrderPayment.Brand = Syst.Var:gcBrand AND 
     OrderPayment.OrderId = piOrderId NO-LOCK NO-ERROR.

IF AVAILABLE OrderPayment AND
             OrderPayment.Method > 0 THEN DO:
   IF OrderPayment.Method EQ {&ORDERPAYMENT_M_POD} THEN
      lcPaymentType = "on_delivery".
   ELSE IF OrderPayment.Method EQ {&ORDERPAYMENT_M_CREDIT_CARD} THEN
         lcPaymentType = "credit_card".
   ELSE IF OrderPayment.Method EQ {&ORDERPAYMENT_M_PAYPAL} THEN
         lcPaymentType = "paypal".      
     
END.

/* terminal_return_contracts - Array of contract ids associated with Order */
FOR EACH TermReturn NO-LOCK WHERE
         TermReturn.OrderId = Order.OrderId AND
       ((TermReturn.DeviceScreen = TRUE AND TermReturn.DeviceStart  = TRUE) OR
        (TermReturn.DeviceScreen = ?    AND TermReturn.DeviceStart  = ?)):
   lcTermReturnContracts = lcTermReturnContracts + "," + TermReturn.ContractId.
END.
IF lcTermReturnContracts <> "" THEN
   lcTermReturnContracts = TRIM(lcTermReturnContracts,",").

/* extension_contracts - Array of extension contract ids associated with Order */
FOR EACH bMsRequest NO-LOCK WHERE
         bMsRequest.MsSeq        = Order.MsSeq AND
         bMsRequest.ReqType      = {&REQTYPE_CONTRACT_ACTIVATION} AND
         bMsRequest.ReqStatus    = 0 AND
         bMsRequest.Reqcparam3   = "RVTERM12",
    FIRST SingleFee NO-LOCK USE-INDEX Custnum WHERE
          SingleFee.Brand        = Syst.Var:gcBrand AND
          SingleFee.Custnum      = Order.CustNum AND
          SingleFee.HostTable    = "Mobsub" AND
          SingleFee.KeyValue     = STRING(Order.MsSeq) AND
          SingleFee.SourceTable  = "DCCLI" AND
          SingleFee.SourceKey    = STRING(bMsRequest.ReqIParam3) AND
          SingleFee.CalcObj      = "RVTERM" AND
          SingleFee.OrderId      = Order.OrderId:
   lcExtensionContracts = lcExtensionContracts + "," + bMsRequest.ReqCparam4.
END.
FOR EACH DCCLI NO-LOCK WHERE
         DCCLI.Brand    = Syst.Var:gcBrand AND
         DCCLI.DCEvent  = "RVTERM12" AND
         DCCLI.MsSeq    = Order.MsSeq AND
         DCCLI.ValidTo >= TODAY,
    EACH  bMsRequest NO-LOCK USE-INDEX MsActStamp WHERE
          bMsRequest.MsSeq       = DCCLI.MsSeq AND
          bMsRequest.ReqType     = {&REQTYPE_CONTRACT_ACTIVATION} AND
          bMsRequest.ReqStat     = 2 AND
          bMsRequest.ActStamp   >= Func.Common:mMake2DT(DCCLI.ValidFrom,0) AND
          bMsRequest.ActStamp   <= Func.Common:mMake2DT(DCCLI.ValidFrom,86399) AND
          bMsRequest.Reqcparam3  = "RVTERM12",
    FIRST SingleFee NO-LOCK USE-INDEX Custnum WHERE
          SingleFee.Brand        = Syst.Var:gcBrand AND
          SingleFee.Custnum      = Order.CustNum AND
          SingleFee.HostTable    = "Mobsub" AND
          SingleFee.KeyValue     = STRING(Order.MsSeq) AND
          SingleFee.SourceTable  = "DCCLI" AND
          SingleFee.SourceKey    = STRING(bMsRequest.ReqIParam3) AND
          SingleFee.CalcObj      = "RVTERM" AND
          SingleFee.OrderId      = Order.OrderId:
   lcExtensionContracts = lcExtensionContracts + "," + bMsRequest.ReqCparam4.
END.
IF lcExtensionContracts <> "" THEN
   lcExtensionContracts = TRIM(lcExtensionContracts,",").

/* add values to the response if no error */
top_struct = add_struct(response_toplevel_id, "").

add_string(top_struct, "brand"          , BUFFER-TENANT-NAME(Order)).
add_int(   top_struct, "tms_id"         , piOrderId         ).
add_int(   top_struct, "order_type"     , Order.OrderType   ).
add_string(top_struct, "tenant"         , vcTenant          ).
add_string(top_struct, "status"         , Order.StatusCode  ).
add_string(top_struct, "dms_status_code" , lcDMSStatusCode).
add_string(top_struct, "dms_status_desc" , lcDMSStatusDesc).
add_string(top_struct, "order_channel"  , Order.OrderChannel). 
add_string(top_struct, "contract_number", Order.ContractId  ). 
add_string(top_struct, "salesman_id"    , Order.Salesman    ). 
add_string(top_struct, "campaign"       , Order.Campaign    ).
add_string(top_struct, "payment_type"   , lcPaymentType    ).
add_string(top_struct, "orderer_ip"     , Order.OrdererIP   ). 
add_string(top_struct, "creditcard_ref" , 
   (IF AVAIL OrderPayment THEN OrderPayment.CCReference ELSE "")). 
add_timestamp(top_struct, "creation_time", Order.CrStamp). 
add_string(top_struct, "referee", Order.Referee).
IF Order.RiskCode NE "" THEN 
add_string(top_struct,"risk_code",Order.RiskCode).

add_int(top_struct,"delivery_type", Order.DeliveryType).
add_int(top_struct,"delivery_secure", Order.DeliverySecure).

IF Order.PortingDate NE ? THEN
   add_date_or_time(top_struct, "requested_porting_time", Order.PortingDate, 0).
ELSE add_string(top_struct, "requested_porting_time", "").

IF Order.StatusCode = {&ORDER_STATUS_DELIVERED} AND
   CAN-FIND(FIRST MsRequest WHERE
                  MsRequest.MsSeq = Order.MsSeq AND
                  MsRequest.ReqType = {&REQTYPE_REVERT_RENEWAL_ORDER} AND
                  MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
                  MsRequest.ReqIParam1 = Order.OrderId NO-LOCK) THEN
   add_boolean(top_struct,"reverted_renewal_order",TRUE).

add_string(top_struct, "terminal_return_contracts" , lcTermReturnContracts).
add_string(top_struct, "extension_contracts" , lcExtensionContracts).

gcSubscription = add_struct(top_struct, "subscription").

add_int(   gcSubscription, "msseq"            , Order.MsSeq  ).
add_string(gcSubscription, "icc"              , Order.ICC    ).
add_string(gcSubscription, "cli"              , Order.CLI    ).
add_string(gcSubscription, "subscription_type", Order.CLIType).  
add_string(gcSubscription, "number_type"      , 
            TRIM(STRING(Order.PayType,"PrePaid/PostPaid"))).

add_string( gcSubscription , "old_operator"      , Order.CurrOper  ).
add_string( gcSubscription , "old_icc"           , Order.OldIcc           ).
add_string(gcSubscription , "old_payment_method",
            TRIM(STRING(Order.OldPayType,"PrePaid/PostPaid"))).
add_string(gcSubscription, "offer_id", Order.Offer). 
add_string(gcSubscription, "logistics_file", Order.Logistics). 

/* Get correct bundle to return correct CLIType */
lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

IF LOOKUP(Order.CliType,lcBundleCLITypes) > 0 THEN
   lcDataBundle = fGetDataBundleInOrderAction(Order.OrderID,Order.CLIType).

add_string(gcSubscription,"data_bundle_id",lcDataBundle).

FIND FIRST MobSub WHERE
           MobSub.MsSeq EQ Order.MsSeq NO-LOCK NO-ERROR.
IF AVAIL MobSub THEN add_int(gcSubscription,"msstatus",MobSub.MsStatus).

/* mnp data */

gcArrayMnp  = add_array(top_struct, "mnp").

FIND FIRST MSRequest WHERE
           MSRequest.MsSeq = Order.MsSeq AND
           MsRequest.ReqType = 13 NO-LOCK NO-ERROR.

FOR EACH MnpProcess NO-LOCK
   WHERE MnpProcess.OrderId = piOrderId BY MNPProcess.CreatedTS DESC:

   gcStructMnp = add_struct(gcArrayMnp, "").
   
   FIND TMSCodes WHERE 
        TMSCodes.TableName = "MNPProcess" AND
        TMSCodes.FieldName = "StatusCode" AND
        TMSCodes.CodeGroup = "MNP" AND
        TMSCodes.CodeValue = STRING(MNPProcess.StatusCode)
   NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN lcMNPStatus = TMSCodes.CodeName.
   ELSE lcMNPStatus = STRING(MNPProcess.StatusCode).

   add_string( gcStructMnp    , "status"            , lcMNPStatus     ).
   add_string( gcStructMnp    , "tms_request_id"    , MNPProcess.FormRequest ).
   add_string( gcStructMnp    , "mnp_request_id"    , MNPProcess.PortRequest ). 
   add_int(gcStructMnp, "id", MNPProcess.MNPSeq). 
   
   FIND LAST MNPSub WHERE MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK NO-ERROR. 
   IF AVAIL MNPSub THEN DO: 
      add_timestamp( gcStructMnp, "porting_time",MNPSub.PortingTime).
      IF AVAIL MsRequest AND MsRequest.ActStamp EQ MNPSub.PortingTime THEN llPortTimeIsProp = FALSE.
      ELSE llPortTimeIsProp = TRUE.
      add_boolean(gcStructMnp, "proposal_porting_time", llPortTimeIsProp).
   END.

   gcArrayMnpMessages = add_array(gcStructMnp, "mnpmessages").
  
   FOR EACH MnpMessage WHERE MnpMessage.MNPSeq = MnpProcess.MnpSeq NO-LOCK:

      gcStructMnpMessage = add_struct(gcArrayMnpMessages, ""). 
      /* vai 0,1,2,... "mnpmessage":n tilalle ... */
      add_string(gcStructMnpMessage, "type", MnpMessage.MessageType).
      add_string(gcStructMnpMessage, "from", 
         (IF MNPMessage.Sender = 1 THEN "TMS" ELSE "MNP")).

      add_timestamp(gcStructMnpMessage, "date_time", MnpMessage.CreatedTs).

      cMnpMsgStatus = "".
      IF MnpMessage.MessageType = "stateChanged" THEN
         cMnpMsgStatus = fGetNodeValue(MnpMessage.XMLMessage, "portabilityState").
      ELSE IF MnpMessage.MessageType = "portabilityRequest" THEN
         cMnpMsgStatus = "NEW".

      add_string(gcStructMnpMessage, "status", cMnpMsgStatus).

   END.

END.

/* order terminals */

lcOrderTerminals = add_array(top_struct, "order_terminals").

IF Order.Offer NE "" THEN
   iTerminalOfferItemId = fGetTerminalOfferItemId(Order.Offer, {&BITEM_GRP_TERMINAL}, Order.CrStamp).
IF iTerminalOfferItemId >= 0 THEN DO:
   FIND OfferItem WHERE 
        OfferItem.OfferItemId = iTerminalOfferItemId NO-LOCK NO-ERROR.
   IF AVAIL OfferItem THEN DO:
      lcTerminalCode = OfferItem.ItemKey.
      lcOrderTerminal = add_struct(lcOrderTerminals, "").
      add_string(lcOrderTerminal,"billing_item_id", lcTerminalCode). 
      add_double(lcOrderTerminal, "amount", OfferItem.Amount).
   END.
END.

FIND OrderAccessory NO-LOCK WHERE 
     OrderAccessory.Brand = Syst.Var:gcBrand AND 
     OrderAccessory.OrderId = piOrderId AND
     OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-ERROR.

IF AVAIL OrderAccessory THEN DO:
   
   /* do not overwrite offer values */
   IF lcTerminalCode EQ "" THEN DO:
      lcOrderTerminal = add_struct(lcOrderTerminals, "").
      add_string(lcOrderTerminal, "billing_item_id",OrderAccessory.ProductCode).
      add_double(lcOrderTerminal, "amount", OrderAccessory.Amount).
   END.

   add_string(lcOrderTerminal, "imei", OrderAccessory.IMEI).
   /* not in use */
   add_double(lcOrderTerminal, "vat_amount", OrderAccessory.VatAmount). 
   add_double(lcOrderTerminal, "discount", OrderAccessory.Discount).
   add_int(lcOrderTerminal, "hard_book", OrderAccessory.HardBook).
   add_string(lcOrderTerminal, "hard_book_state", OrderAccessory.HardBookState).
END.
ELSE IF lcTerminalCode NE "" THEN DO:   
   add_string(lcOrderTerminal, "imei", "").
   add_double(lcOrderTerminal, "vat_amount", 0). /* not in use */
   add_double(lcOrderTerminal, "discount", 0).
END.

laptop_array = add_array(top_struct,"order_laptops").

FIND OfferItem WHERE
     OfferItem.Brand = Syst.Var:gcBrand AND
     OfferItem.Offer = Order.Offer AND
     OfferItem.ItemType = "PerContract" AND
     OfferItem.ItemKey BEGINS "PAYTERM" AND
     OfferItem.EndStamp >= Order.CrStamp  AND
     OfferItem.BeginStamp <= Order.CrStamp NO-LOCK NO-ERROR.

FOR EACH OrderAccessory NO-LOCK WHERE 
         OrderAccessory.Brand = Order.Brand AND
         OrderAccessory.OrderID = Order.OrderID AND
         OrderAccessory.TerminalType = {&TERMINAL_TYPE_LAPTOP}:
   term_struct = add_struct(laptop_array,"").
   add_string(term_struct,"serial_number", OrderAccessory.IMEI).
   add_string(term_struct,"laptop", OrderAccessory.ProductCode). 
   add_string(term_struct,"payterm", (IF AVAIL OfferItem THEN OfferItem.ItemKey ELSE "")).
END.

/*YPR-2478*/
FIND FIRST OrderAccessory NO-LOCK WHERE
         OrderAccessory.Brand = Order.Brand AND
         OrderAccessory.OrderID = Order.OrderID AND
         OrderAccessory.TerminalType = {&TERMINAL_TYPE_ACCESSORY} NO-ERROR.
IF AVAIL OrderAccessory THEN
   add_string(top_struct,"sub_accessory", OrderAccessory.productcode).

FIND OrderCustomer WHERE 
     OrderCustomer.Brand = Syst.Var:gcBrand AND 
     OrderCustomer.OrderId = piOrderId AND 
     OrderCustomer.RowType = 4
     NO-LOCK NO-ERROR.

IF AVAIL OrderCustomer THEN DO:
   gcStructDeliveryAddress = add_struct(top_struct, "delivery_address").

   add_string(gcStructDeliveryAddress, "street_code",OrderCustomer.AddressCodC).
   add_string(gcStructDeliveryAddress, "city_code", OrderCustomer.AddressCodP).
   add_string(gcStructDeliveryAddress, "municipality_code",
                                       OrderCustomer.AddressCodM).
   add_string(gcStructDeliveryAddress, "title"     , OrderCustomer.CustTitle).
   add_string(gcStructDeliveryAddress, "first_name", OrderCustomer.FirstName). 
   add_string(gcStructDeliveryAddress, "surname_1" , OrderCustomer.SurName1 ).
   add_string(gcStructDeliveryAddress, "surname_2" , OrderCustomer.SurName2 ).
   add_string(gcStructDeliveryAddress, "zip_code"  , OrderCustomer.ZipCode  ).
   add_string(gcStructDeliveryAddress, "region"    , OrderCustomer.Region   ).
   add_string(gcStructDeliveryAddress, "kiala_code", OrderCustomer.KialaCode).
   add_string(gcStructDeliveryAddress, "site_name",  OrderCustomer.Company).
END.

FIND OrderCustomer WHERE 
     OrderCustomer.Brand = Syst.Var:gcBrand AND 
     OrderCustomer.OrderId = piOrderId AND 
     OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_LOGISTICS} 
     NO-LOCK NO-ERROR.

IF AVAIL OrderCustomer THEN DO:
   gcStructSecureAddress = add_struct(top_struct, "secure_address").
   
   add_string(gcStructSecureAddress, "street_code",OrderCustomer.AddressCodC).
   add_string(gcStructSecureAddress, "city_code",  OrderCustomer.AddressCodP).
   add_string(gcStructSecureAddress, "municipality_code",
                                                   OrderCustomer.AddressCodM).
   add_string(gcStructSecureAddress, "city",       OrderCustomer.PostOffice).
   add_string(gcStructSecureAddress, "zip_code",   OrderCustomer.ZipCode).
   add_string(gcStructSecureAddress, "region",     OrderCustomer.Region).
   add_string(gcStructSecureAddress, "country",    OrderCustomer.Country).
END.
 
IF Order.OrderChannel BEGINS "fusion" THEN DO: 
   
   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand = Order.Brand AND
              OrderFusion.OrderId = Order.OrderID NO-ERROR.

   IF AVAIL OrderFusion AND OrderFusion.FusionStatus > "" THEN DO:
   
      lcFusionStruct = add_struct(top_struct, "fusion_data").
      add_string(lcFusionStruct, "fixed_line_iua",OrderFusion.IUA).
      add_string(lcFusionStruct, "fixed_number_type",OrderFusion.FixedNumberType).
      add_string(lcFusionStruct, "fixed_line_number",OrderFusion.FixedNumber).
      add_string(lcFusionStruct, "fixed_line_mnp_old_operator_name",OrderFusion.FixedCurrOper).
      add_string(lcFusionStruct, "fixed_line_mnp_old_operator_code",OrderFusion.FixedCurrOperCode).
      add_string(lcFusionStruct, "fixed_line_serial_number",OrderFusion.SerialNumber).
      add_string(lcFusionStruct, "fixed_line_adsl_linkstate",OrderFusion.ADSLLinkState).
      add_string(lcFusionStruct, "fixed_mnp_time_of_change",OrderFusion.FixedMNPTime).
      add_string(lcFusionStruct, "fusion_order_status",OrderFusion.FusionStatus).
      add_string(lcFusionStruct, "fixed_line_order_status",OrderFusion.FixedStatus).
      add_string(lcFusionStruct, "customer_type",OrderFusion.Customer).
      add_string(lcFusionStruct, "fixed_line_product",OrderFusion.Product).
      add_string(lcFusionStruct, "update_ts", STRING(OrderFusion.UpdateTS)).
      add_string(lcFusionStruct, "fixed_line_appointment_date",OrderFusion.AppointmentDate).
      add_boolean(lcFusionStruct, "allow_mobile_order_release",
         (OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_ONGOING})).

      FIND FIRST OrderCustomer WHERE 
                 OrderCustomer.Brand = Syst.Var:gcBrand AND 
                 OrderCustomer.OrderId = piOrderId AND 
                 OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
      NO-LOCK NO-ERROR.
      IF AVAIL OrderCustomer THEN DO:
         
         lcFixedInstallAddress = add_struct(lcFusionStruct, "install_address").
         
         add_string(lcFixedInstallAddress, "fname", OrderCustomer.FirstName). 
         add_string(lcFixedInstallAddress, "lname", OrderCustomer.SurName1).
         add_string(lcFixedInstallAddress, "surname_2", OrderCustomer.SurName2).
         add_string(lcFixedInstallAddress, "phone_number", 
            (IF OrderCustomer.MobileNumber > "" THEN OrderCustomer.MobileNumber 
             ELSE OrderCustomer.FixedNumber)).
         add_string(lcFixedInstallAddress, "address", OrderCustomer.Address).
         add_string(lcFixedInstallAddress, "additional_address", OrderCustomer.AddressCompl).
         add_string(lcFixedInstallAddress, "city", OrderCustomer.PostOffice).
         add_string(lcFixedInstallAddress, "zip", OrderCustomer.ZipCode).
         add_string(lcFixedInstallAddress, "street_number", OrderCustomer.BuildingNum).
         add_string(lcFixedInstallAddress, "region", OrderCustomer.Region).
         add_string(lcFixedInstallAddress, "profession", OrderCustomer.Profession).
         add_string(lcFixedInstallAddress, "email", OrderCustomer.Email).
         add_string(lcFixedInstallAddress, "gescal", OrderCustomer.Gescal).
         add_string(lcFixedInstallAddress, "address_compl", OrderCustomer.AddressCompl).
         add_string(lcFixedInstallAddress, "floor", OrderCustomer.Floor).
         add_string(lcFixedInstallAddress, "street_type", OrderCustomer.StreetType).
         add_string(lcFixedInstallAddress, "bis_duplicate", OrderCustomer.BisDuplicate).
         add_string(lcFixedInstallAddress, "block", OrderCustomer.Block).
         add_string(lcFixedInstallAddress, "door", OrderCustomer.Door).
         add_string(lcFixedInstallAddress, "letter", OrderCustomer.Letter).
         add_string(lcFixedInstallAddress, "stair", OrderCustomer.Stair).
         add_string(lcFixedInstallAddress, "hand", OrderCustomer.Hand).
         add_string(lcFixedInstallAddress, "km", OrderCustomer.Km).
      END.
      
      FIND FIRST OrderCustomer WHERE 
                 OrderCustomer.Brand = Syst.Var:gcBrand AND 
                 OrderCustomer.OrderId = piOrderId AND 
                 OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_BILLING}
      NO-LOCK NO-ERROR.
      IF AVAIL OrderCustomer THEN DO:
         
         lcFixedBillingAddress = add_struct(lcFusionStruct, "billing_address").

         add_string(lcFixedBillingAddress, "address", OrderCustomer.Address).
         add_string(lcFixedBillingAddress, "additional_address", OrderCustomer.AddressCompl).
         add_string(lcFixedBillingAddress, "city", OrderCustomer.PostOffice).
         add_string(lcFixedBillingAddress, "zip", OrderCustomer.ZipCode).
         add_string(lcFixedBillingAddress, "region", OrderCustomer.Region).
         add_string(lcFixedBillingAddress, "street_number", OrderCustomer.BuildingNum).
      END.
      add_string(lcFusionStruct, "cancellation_reason",OrderFusion.CancellationReason).
   END.
END.

FINALLY:
   END.
