/* ----------------------------------------------------------------------
  MODULE .......: roi_history_send.p
  TASK .........: Send ROI History
  APPLICATION ..: TMS
  AUTHOR .......: rafaeldv
  CREATED ......: 
  CHANGED ......: 

  Version ......: xfera
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
{Func/heartbeat.i}
{Func/log.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{fcgi_agent/xmlrpc/xmlrpc_client.i}
{Func/dms.i}

Syst.Var:gcBrand = "1".

fSetLogFileName("/scratch/log/roi/roi_history_send.log").

DEFINE VARIABLE liLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTime     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPause    AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldRetry    AS DECIMAL   NO-UNDO INIT 30.
DEFINE VARIABLE llNagBeat  AS LOG       NO-UNDO INIT TRUE.
DEFINE VARIABLE lcNagios   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liNagios AS INTEGER NO-UNDO.  
DEFINE VARIABLE clsNagios   AS CLASS Class.nagios    NO-UNDO.
DEFINE VARIABLE lcOrderTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE cConnURL AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTimeOut AS INTEGER NO-UNDO.
DEFINE VARIABLE liNumMsgs AS INTEGER NO-UNDO.
DEFINE VARIABLE liPrintXML AS INTEGER NO-UNDO.

DEF BUFFER bOrder FOR Order.
/*For testing*/
liPrintXML = 0.
DEF STREAM sOut.
 
/*NOTE: This must be modified to match our functionality naming. */
FUNCTION fwriexml_ROI_test RETURNS CHAR /* Ilkka: change good function name and output writinc values.*/
   (icMethod AS CHAR):
   IF liPrintXML NE 0 THEN DO:
      xmlrpc_initialize(FALSE).
      OUTPUT STREAM sOut TO VALUE("/tmp/ROI_xml_" +
      REPLACE(STRING(Func.Common:mMakeTS()), ".", "_") +
      ".xml") APPEND.
      PUT STREAM sOut UNFORMATTED
         string(serialize_rpc_call( icMethod)) SKIP.
      PUT STREAM sOut "" SKIP.
      OUTPUT STREAM sOut CLOSE.
      xmlrpc_initialize(FALSE).
   END.
END.

FORM
   SKIP(1)
   " Loops: " liLoop FORMAT ">>>>>9" lcTime FORMAT "X(20)"
   SKIP(1)
WITH
   CENTERED NO-LABELS TITLE " ROI History SENDER " WIDTH 40 ROW 8
FRAME frmMain .

DISP
   liLoop 
   STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
WITH FRAME frmMain.

/* order struct */ 
FUNCTION fFillOrderStruct RETURNS LOGICAL 
         (INPUT pcStruct AS CHARACTER):

   /* mandatory */
   add_string(pcStruct,"channel", "order " +  Order.OrderChannel).
   add_string(pcStruct, "orderer_ip", Order.OrdererIP).
   add_string(pcStruct, "contractid", Order.ContractId).
   add_string(pcStruct, "cli", Order.CLI). 
   add_string(pcStruct, "subscription_type", Order.CLIType).
   add_string(pcStruct, "order_id", STRING(Order.OrderId)).
   add_string(pcStruct, "dms_status", fGetOrderDMSStatus(Order.OrderId)).
   /* optionals  */
   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = Syst.Var:gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
   IF AVAIL OrderCustomer THEN   /* BankAccount without IBAN format */
       /* note: added temporary support for old format since it's possible
          that closed retention orders are reopened */
       add_string(pcStruct, "billing_data", 
         (IF LENGTH(OrderCustomer.BankCode) > 20 
          THEN SUBSTRING(OrderCustomer.BankCode,5)
          ELSE OrderCustomer.BankCode)).

   FIND FIRST OrderPayment WHERE 
              OrderPayment.Brand = Syst.Var:gcBrand AND
              OrderPayment.OrderId = Order.OrderId NO-LOCK NO-ERROR.
   
   IF AVAIL OrderPayment THEN DO:
        IF OrderPayment.Method EQ {&ORDERPAYMENT_M_POD}  THEN 
           add_string(pcStruct, "payment_method","on_delivery").
        ELSE IF OrderPayment.Method EQ {&ORDERPAYMENT_M_PAYPAL} THEN
           add_string(pcStruct, "payment_method","PAYPAL").
        ELSE 
           add_string(pcStruct, "payment_method","credit_card").
 


        add_string(pcStruct,"payment_reference", OrderPayment.CCReference).
   END.
   
   FIND FIRST Memo WHERE 
              Memo.Brand = Syst.Var:gcBrand AND
              Memo.HostTable = "Order" AND
              Memo.KeyValue  = STRING(Order.OrderId) AND
              Memo.MemoTitle EQ "ORDER CHANNEL in Standalone model" NO-LOCK NO-ERROR.
   IF AVAIL Memo THEN 
       add_boolean(pcStruct,"check",TRUE). 
   ELSE 
       add_boolean(pcStruct,"check",FALSE). 
   
   IF Order.SMSType = 1 THEN 
      add_boolean(pcStruct,"send_sms",TRUE).
   
   add_timestamp(pcStruct,"price_selection_time", Order.CrStamp).
   
   IF Order.Offer NE "" THEN 
      add_string(pcStruct,"offer_id",Order.Offer).
   IF Order.Referee NE "" THEN 
      add_string(pcStruct,"referee",Order.Referee).

   CASE Order.OrderType :
        WHEN  0 THEN add_string(pcStruct,"number_type","new"). 
        WHEN  1 OR WHEN  3 THEN add_string(pcStruct,"number_type","mnp").
        WHEN  2 THEN add_string(pcStruct,"number_type","renewal").
   END.
   
   IF Order.CurrOper NE "" THEN DO: 
      add_string(pcStruct,"old_operator",Order.CurrOper).
      add_string(pcStruct,"old_icc",Order.OldIcc).
      IF Order.oldpaytype EQ TRUE THEN 
        add_string(pcStruct,"old_billing_category","prepaid").
      ELSE 
        add_string(pcStruct,"old_billing_category","postpaid").
   END.

   FIND FIRST OrderTopup WHERE  
              OrderTopup.Brand = Syst.Var:gcBrand AND
              OrderTopup.OrderId = Order.OrderId NO-LOCK NO-ERROR.
   IF AVAIL OrderTopup THEN 
        add_double(pcStruct,"topup",OrderTopup.Amount).
   
   IF Order.FAT NE 0 THEN DO:
       add_double(pcStruct,"fat",Order.FAT).
       add_string(pcStruct,"fatgroup",Order.FTGrp).
   END.
   
   IF Order.ICC NE "" THEN 
       add_string(pcStruct,"icc",Order.ICC).
   
   add_string(pcStruct,"salesman",Order.Salesman).
   
   IF Order.Campaign NE "" THEN 
       add_string(pcStruct,"campaign_code",Order.Campaign).
   
   FIND FIRST Memo WHERE 
              Memo.Brand = Syst.Var:gcBrand AND
              Memo.HostTable = "Order" AND
              Memo.KeyValue  = STRING(Order.OrderId) AND
              Memo.MemoTitle EQ "Info" NO-LOCK NO-ERROR.
   IF AVAIL Memo THEN 
      add_string(pcStruct,"memo",Memo.MemoText).
  
   add_string(pcStruct,"date",substr(string(Order.CrStamp,"99999999.99999"),1,8)).

  lcOrderTime = substr(string(Order.CrStamp,"99999999.99999"),10,5).
  lcOrderTime = STRING(integer(lcOrderTime),"HH:MM:SS").
  lcOrderTime = REPLACE(lcOrderTime,":","").

   add_string(pcStruct,"time",lcOrderTime).

   add_string(pcStruct,"order_status",Order.StatusCode).
   add_string(pcStruct,"risk_code",Order.RiskCode).  

   /* ADDLINE-22 Additional Line */
   IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                     CLIType.CLIType    = Order.CLIType               AND
                     CLIType.PayType    = {&CLITYPE_PAYTYPE_POSTPAID} AND
                     CLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY}) AND
      LOOKUP(Order.CLIType, {&ADDLINE_CLITYPES}) > 0 THEN DO: 
      IF CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                        OrderAction.Brand    = Syst.Var:gcBrand           AND
                        OrderAction.OrderID  = Order.OrderID     AND
                        OrderAction.ItemType = "AddLineDiscount" AND
                        LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS} + "," + {&ADDLINE_DISCOUNTS_20}) > 0) 
      THEN
         add_int(pcStruct, "C_ADDITIONAL_LINE", 1).
      ELSE
         add_int(pcStruct, "C_ADDITIONAL_LINE", 0).
   END.

   RETURN TRUE.
END.

/* Customer Struct */
FUNCTION fFillCustomerStruct RETURNS LOGICAL
         (INPUT pcStruct AS CHARACTER,
          INPUT piRowType AS INTEGER):

   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = Syst.Var:gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND 
              OrderCustomer.RowType = piRowType NO-LOCK NO-ERROR.

   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.

   add_string(pcStruct,"fname",OrderCustomer.FirstName).
   add_string(pcStruct,"lname",OrderCustomer.Surname1).
   add_string(pcStruct,"lname2",OrderCustomer.Surname2).
   add_string(pcStruct,"site_name",OrderCustomer.Company).
   add_string(pcStruct,"region",OrderCustomer.Region).
   add_int(pcStruct,"zip",INT(OrderCustomer.ZipCode)).
   add_string(pcStruct,"city",OrderCustomer.PostOffice).
   add_string(pcStruct,"country",OrderCustomer.Country).
   add_string(pcStruct,"nationality",OrderCustomer.Nationality).
   add_string(pcStruct,"email",OrderCustomer.Email).
   add_string(pcStruct,"title",OrderCustomer.custtitle).
   add_string(pcStruct,"sms_number",OrderCustomer.MobileNumber).
   add_string(pcStruct,"phone_number",OrderCustomer.FixedNumber).
   add_string(pcStruct,"street_code",OrderCustomer.AddressCodC).
   add_string(pcStruct,"city_code",OrderCustomer.AddressCodP).
   add_datetime(pcStruct,"birthday",OrderCustomer.Birthday).
   add_boolean(pcStruct,"self_employed",OrderCustomer.SelfEmployed).
   add_datetime(pcStruct,"foundation_date",OrderCustomer.FoundationDate).
   add_string(pcStruct,"language",OrderCustomer.Language).
   add_boolean(pcStruct,"mark_sms",OrderCustomer.OperSMSMarketing).
   add_boolean(pcStruct,"mark_email",OrderCustomer.OperEmailMarketing).
   add_boolean(pcStruct,"mark_post",OrderCustomer.OperPostMarketing).
   add_boolean(pcStruct,"mark_sms_3rd",OrderCustomer.OutSMSMarketing).
   add_boolean(pcStruct,"mark_email_3rd",OrderCustomer.OutEMailMarketing).
   add_boolean(pcStruct,"mark_post_3rd",OrderCustomer.OutPostMarketing).
   add_string(pcStruct,"id_type",OrderCustomer.CustIdType).
   
   /* YPRO-24 ROI History Segment field WITH Category */
   FIND FIRST CustCat NO-LOCK WHERE
              CustCat.Brand    = Syst.Var:gcBrand AND
              CustCat.Category = OrderCustomer.Category NO-ERROR.
   IF AVAILABLE CustCat THEN
      add_string(pcStruct,"segment",CustCat.Segment).

   IF OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} AND
      OrderCustomer.CustIdType = "CIF"
   THEN DO:
      add_string(pcStruct,"person_id",OrderCustomer.AuthCustId).
      add_string(pcStruct,"company_id",OrderCustomer.CustId).
   END.
   ELSE add_string(pcStruct,"person_id",OrderCustomer.CustId).

   IF OrderCustomer.BuildingNum NE "" THEN
      add_string(pcStruct,"building_number",OrderCustomer.BuildingNum).

   IF OrderCustomer.AddressCompl NE "" THEN 
      add_string(pcStruct,"complements",OrderCustomer.AddressCompl).


   IF OrderCustomer.Street NE "" THEN
      add_string(pcStruct,"street", OrderCustomer.Street).
   ELSE 
      add_string(pcStruct,"street",OrderCustomer.Address).

   add_boolean(pcStruct,"retrieved",OrderCustomer.custDataRetr).
   RETURN TRUE.

END.

/* Address Struct */
FUNCTION fFillAddressStruct RETURNS LOGICAL 
         (INPUT pcStruct AS CHARACTER):
 
   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = Syst.Var:gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND 
              OrderCustomer.RowType = 4 NO-LOCK NO-ERROR.

   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.

   add_string(pcStruct,"fname",OrderCustomer.FirstName).
   add_string(pcStruct,"lname", OrderCustomer.Surname1).
   add_string(pcStruct,"lname2",OrderCustomer.Surname2).
   add_string(pcStruct,"region",OrderCustomer.Region).
   add_int(pcStruct,"zip",INT(OrderCustomer.ZipCode)).
   add_string(pcStruct,"city",OrderCustomer.PostOffice).

   add_string(pcStruct,"street_code",OrderCustomer.AddressCodC).
   add_string(pcStruct,"city_code",OrderCustomer.AddressCodP).

   IF OrderCustomer.BuildingNum NE "" THEN
      add_string(pcStruct,"building_number",OrderCustomer.BuildingNum).

   IF OrderCustomer.AddressCompl NE "" THEN 
      add_string(pcStruct,"complements",OrderCustomer.AddressCompl).
   IF OrderCustomer.Street NE "" THEN 
      add_string(pcStruct,"street",OrderCustomer.Street).
   ELSE 
      add_string(pcStruct,"street",OrderCustomer.Address).

   IF OrderCustomer.KialaCode > "" THEN 
      add_string(pcStruct,"kiala_code",OrderCustomer.KialaCode). 

   RETURN TRUE.
END.


/* Accessory Struct */
FUNCTION fFillAccessoryStruct RETURNS LOGICAL 
         (INPUT pcStruct AS CHARACTER):

   FIND FIRST OrderAccessory NO-LOCK WHERE
              OrderAccessory.Brand = Syst.Var:gcBrand AND
              OrderAccessory.OrderId = Order.OrderId AND
              OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE}) NO-ERROR.
   IF NOT AVAIL OrderAccessory THEN RETURN FALSE.
   add_string(pcStruct,"imei",OrderAccessory.IMEI).
   IF OrderAccessory.Discount NE 0 THEN 
   add_double(pcStruct,"discount",OrderAccessory.Discount).
   
   RETURN TRUE.
END.

FUNCTION fFillPOSStruct RETURNS LOGICAL
   (INPUT pcStruct AS CHARACTER):

   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand   = Syst.Var:gcBrand   AND
              OrderCustomer.OrderId = Order.OrderId      AND 
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL} NO-LOCK NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.

   add_string(pcStruct, "gescal"       ,  OrderCustomer.Gescal       ).
   add_string(pcStruct, "street_type"  ,  OrderCustomer.StreetType   ).
   add_string(pcStruct, "street_name"  ,  OrderCustomer.Street       ).
   add_string(pcStruct, "street_number",  OrderCustomer.BuildingNum  ).
   add_string(pcStruct, "floor"        ,  OrderCustomer.Floor        ).
   add_string(pcStruct, "door"         ,  OrderCustomer.Door         ).
   add_string(pcStruct, "block"        ,  OrderCustomer.Block        ).
   add_string(pcStruct, "stair"        ,  OrderCustomer.Stair        ).
   add_string(pcStruct, "bis"          ,  OrderCustomer.BisDuplicate ).
   add_string(pcStruct, "town"         ,  OrderCustomer.PostOffice   ).
   add_string(pcStruct, "zip_code"     ,  OrderCustomer.ZipCode      ).
   add_string(pcStruct, "province"     ,  OrderCustomer.Region       ).

   RETURN TRUE.

END.

/* MAIN */

clsNagios = NEW Class.nagios().
lcNagios  = "roisend:ROI sender".

iTimeOut = 10.

cConnURL = fCParam("URL","urlROI").

IF cConnURL = ? OR cConnURL = "" THEN DO:
   MESSAGE "ROI URL not defined (URL->urlROI)" VIEW-AS ALERT-BOX.
   QUIT.
END.

PAUSE 0.
DO WHILE TRUE:

   liLoop = liLoop + 1.

   DISP
      liLoop
      STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
   WITH FRAME frmMain.
   PAUSE 0.

   initialize(cConnURL, iTimeOut).

   FOR EACH Order NO-LOCK WHERE
            Order.Brand = Syst.Var:gcBrand AND
            Order.SendToROI = 1
            liNumMsgs = 1 to 120:

       FIND FIRST bOrder WHERE
            ROWID(bOrder) = ROWID(Order) EXCLUSIVE-LOCK NO-ERROR.
       IF LOCKED(bOrder) OR NOT AVAIL bOrder THEN NEXT.
       RELEASE bOrder.

       RUN pSendROIHistory(Order.OrderId).
       IF llNagBeat = FALSE THEN LEAVE.
   END.
   
   xmlrpc_finalize().

   PUT SCREEN ROW 22 COL 1
      "F8 TO QUIT, OTHER KEYS START ROI HISTORY SEND IMMEDIATELLY".
    
   liPause = 8.
   
   IF llNagBeat THEN liNagios = clsNagios:KeepAlive(lcNagios).

   READKEY PAUSE liPause.
   
   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
      fCloseLog().
      QUIT.
   END.
   
   PUT SCREEN ROW 22 COL 1
      "SENDING ROI HISTORIES ....................................".
END.


PROCEDURE pSendROIHistory:

   DEFINE INPUT PARAMETER piOrderID AS INTEGER NO-UNDO.

   DEFINE VARIABLE lcOrderStruct AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCustomerStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcAddressStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcAccessoryStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcContactStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcRespStruct AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPOSStruct         AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcResp AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDescription AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liStatus AS INTEGER NO-UNDO INITIAL 1.  
   DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO.
   DEFINE VARIABLE liReTry AS INTEGER NO-UNDO. 

   xmlrpc_cleanup().

   FIND Order WHERE Order.Brand = Syst.Var:gcBrand AND
                    Order.OrderId = piOrderId NO-LOCK NO-ERROR.

   lcOrderStruct = add_struct(param_toplevel_id,"").
   fFillOrderStruct(lcOrderStruct).

   lcCustomerStruct = add_struct(param_toplevel_id,"").
   fFillCustomerStruct(lcCustomerStruct,1).

   lcAddressStruct = add_struct(param_toplevel_id,"").
   fFillAddressStruct(lcAddressStruct).

   lcAccessoryStruct = add_struct(param_toplevel_id,"").
   fFillAccessoryStruct(lcAccessoryStruct).

   lcContactStruct = add_struct(param_toplevel_id,"").
   fFillCustomerStruct(lcContactStruct,5).

   lcPOSStruct = add_struct(param_toplevel_id,"").
   fFillPOSStruct(lcPOSStruct).

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLog( "ROI History, ERROR Creating message: " + gc_xmlrpc_error,"NW_ERR"). 
      PUT SCREEN ROW 23 COL 4  "ERROR Creating message " + gc_xmlrpc_error.
      /* save the exception in the ErrorLog */
      ldTS = Func.Common:mMakeTS().
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand = Syst.Var:gcBrand
             ErrorLog.TableName = "Order"
             ErrorLog.KeyValue = STRING(Order.OrderId)
             ErrorLog.ActionID = "ROIHistory"
             ErrorLog.ActionTS = ldTS
             ErrorLog.ErrorMsg = "Creating message: " + gc_xmlrpc_error
             liStatus = 3.
      /* change status */
      FIND CURRENT Order EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN Order.SendToROI = liStatus.
      RELEASE Order.
      xmlrpc_cleanup().
      RETURN.
   END.
 
   fwriexml_ROI_test("ROIHistoryInterface.store_order"). 
   RUN pRPCMethodCall("ROIHistoryInterface.store_order", TRUE). 

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLog( "ROI History, ERROR Sending Message: OrderId: "  + STRING(Order.OrderId)  + gc_xmlrpc_error,"NW_ERR").
      PUT SCREEN ROW 23 COL 4  "ROI History, ERROR Sending Message: " + gc_xmlrpc_error.
      llNagBeat = FALSE.
      xmlrpc_cleanup().
      RETURN.  
   END.

   lcRespStruct = get_struct(response_toplevel_id, "0").

   IF gi_xmlrpc_error EQ 0 THEN
      lcResp = validate_request(lcRespStruct,"result!,description").

   IF gi_xmlrpc_error EQ 0 THEN DO:
      lcResult = get_string(lcRespStruct,"result").
      IF LOOKUP("description",lcResp) GT 0 THEN
      lcDescription = get_string(lcRespStruct,"description").
   END.

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLog( "ROI History, ERROR in Response: " + gc_xmlrpc_error,"NW_ERR"). 
      PUT SCREEN ROW 23 COL 4  "ROI History, ERROR in Response: " + gc_xmlrpc_error.
      llNagBeat = FALSE.
      xmlrpc_cleanup().
      LEAVE.
   END.

   IF lcResult EQ "Ok" THEN DO:
      /* set history as sended */
      PUT SCREEN ROW 23 COL 4 
         "ROI History: Order " + STRING(Order.OrderID) + " sent" + FILL(" ",50).
      liStatus = 2.
   END.
   ELSE DO:
      /* save the exception in the ErrorLog */
      ldTS = Func.Common:mMakeTS().
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand = Syst.Var:gcBrand
             ErrorLog.TableName = "Order"
             ErrorLog.KeyValue = STRING(Order.OrderId)
             ErrorLog.ActionID = "ROIHistory"
             ErrorLog.ActionTS = ldTS
             ErrorLog.ErrorMsg = "ROI Response: " + lcDescription
             liStatus = 3.
      RELEASE ErrorLog.
   END.

   /* change status */
   liReTry = 0.
   DO WHILE TRUE:
      FIND CURRENT Order EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF LOCKED Order THEN DO:
         PAUSE 1 NO-MESSAGE.
         liReTry = liReTry + 1.
         IF liReTry >= 5 THEN LEAVE.
         NEXT.
      END.  
      ELSE DO:
         ASSIGN Order.SendToROI = liStatus.
         LEAVE.
      END.
   END.

   llNagBeat = TRUE.
   xmlrpc_cleanup().
   RELEASE Order.

END.
