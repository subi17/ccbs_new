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
{Func/timestamp.i}
{Func/heartbeat.i}
{Func/log.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{fcgi_agent/xmlrpc/xmlrpc_client.i}

gcBrand = "1".

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

DEF BUFFER bOrder FOR Order.

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


FUNCTION fToUTF8 RETURNS CHARACTER 
         (INPUT pcText AS CHARACTER) :
 RETURN  CODEPAGE-CONVERT(pcText,"UTF-8",SESSION:CHARSET).
END FUNCTION. 



/* order struct */ 
FUNCTION fFillOrderStruct RETURNS LOGICAL 
         (INPUT pcStruct AS CHARACTER):

   /* mandatory */
   add_string(pcStruct,"channel", fToUTF8("order " +  Order.OrderChannel)).
   add_string(pcStruct, "orderer_ip", fToUTF8(Order.OrdererIP)).
   add_string(pcStruct, "contractid", fToUTF8(Order.ContractId)).
   add_string(pcStruct, "cli", fToUTF8(Order.CLI)). 
   add_string(pcStruct, "subscription_type", fToUTF8(Order.CLIType)).
   add_string(pcStruct, "order_id", fToUTF8(STRING(Order.OrderId))).
   /* optionals  */
   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
   IF AVAIL OrderCustomer THEN   /* BankAccount without IBAN format */
       /* note: added temporary support for old format since it's possible
          that closed retention orders are reopened */
       add_string(pcStruct, "billing_data", fToUTF8(
         (IF LENGTH(OrderCustomer.BankCode) > 20 
          THEN SUBSTRING(OrderCustomer.BankCode,5)
          ELSE OrderCustomer.BankCode))).

   FIND FIRST OrderPayment WHERE 
              OrderPayment.Brand = gcBrand AND
              OrderPayment.OrderId = Order.OrderId NO-LOCK NO-ERROR.
   
   IF AVAIL OrderPayment THEN DO:
        IF OrderPayment.Method EQ {&ORDERPAYMENT_M_POD}  THEN 
           add_string(pcStruct, "payment_method",fToUTF8("on_delivery")).
        ELSE IF OrderPayment.Method EQ {&ORDERPAYMENT_M_PAYPAL} THEN
           add_string(pcStruct, "payment_method",fToUTF8("PAYPAL")).
        ELSE 
           add_string(pcStruct, "payment_method",fToUTF8("credit_card")).
 


        add_string(pcStruct,"payment_reference", fToUTF8(OrderPayment.CCReference)).
   END.
   
   FIND FIRST Memo WHERE 
              Memo.Brand = gcBrand AND
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
      add_string(pcStruct,"offer_id",fToUTF8(Order.Offer)).
   IF Order.Referee NE "" THEN 
      add_string(pcStruct,"referee",fToUTF8(Order.Referee)).

   CASE Order.OrderType :
        WHEN  0 THEN add_string(pcStruct,"number_type",fToUTF8("new")). 
        WHEN  1 OR WHEN  3 THEN add_string(pcStruct,"number_type",fToUTF8("mnp")).
        WHEN  2 THEN add_string(pcStruct,"number_type",fToUTF8("renewal")).
   END.
   
   IF Order.CurrOper NE "" THEN DO: 
      add_string(pcStruct,"old_operator",fToUTF8(Order.CurrOper)).
      add_string(pcStruct,"old_icc",fToUTF8(Order.OldIcc)).
      IF Order.oldpaytype EQ TRUE THEN 
        add_string(pcStruct,"old_billing_category",fToUTF8("prepaid")).
      ELSE 
        add_string(pcStruct,"old_billing_category",fToUTF8("postpaid")).
   END.

   FIND FIRST OrderTopup WHERE  
              OrderTopup.Brand = gcBrand AND
              OrderTopup.OrderId = Order.OrderId NO-LOCK NO-ERROR.
   IF AVAIL OrderTopup THEN 
        add_double(pcStruct,"topup",OrderTopup.Amount).
   
   IF Order.FAT NE 0 THEN DO:
       add_double(pcStruct,"fat",Order.FAT).
       add_string(pcStruct,"fatgroup",fToUTF8(Order.FTGrp)).
   END.
   
   IF Order.ICC NE "" THEN 
       add_string(pcStruct,"icc",fToUTF8(Order.ICC)).
   
   add_string(pcStruct,"salesman",fToUTF8(Order.Salesman)).
   
   IF Order.Campaign NE "" THEN 
       add_string(pcStruct,"campaign_code",fToUTF8(Order.Campaign)).
   
   FIND FIRST Memo WHERE 
              Memo.Brand = gcBrand AND
              Memo.HostTable = "Order" AND
              Memo.KeyValue  = STRING(Order.OrderId) AND
              Memo.MemoTitle EQ "Info" NO-LOCK NO-ERROR.
   IF AVAIL Memo THEN 
      add_string(pcStruct,"memo",fToUTF8(Memo.MemoText)).
  
   add_string(pcStruct,"date",fToUTF8(substr(string(Order.CrStamp,"99999999.99999"),1,8))).

  lcOrderTime = substr(string(Order.CrStamp,"99999999.99999"),10,5).
  lcOrderTime = STRING(integer(lcOrderTime),"HH:MM:SS").
  lcOrderTime = REPLACE(lcOrderTime,":","").

   add_string(pcStruct,"time",fToUTF8(lcOrderTime)).

   add_string(pcStruct,"order_status",fToUTF8(Order.StatusCode)).
   add_string(pcStruct,"risk_code",fToUTF8(Order.RiskCode)).  

   RETURN TRUE.
END.

/* Customer Struct */
FUNCTION fFillCustomerStruct RETURNS LOGICAL
         (INPUT pcStruct AS CHARACTER,
          INPUT piRowType AS INTEGER):

   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND 
              OrderCustomer.RowType = piRowType NO-LOCK NO-ERROR.

   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.

   add_string(pcStruct,"fname",fToUTF8(OrderCustomer.FirstName)).
   add_string(pcStruct,"lname",fToUTF8(OrderCustomer.Surname1)).
   add_string(pcStruct,"lname2",fToUTF8(OrderCustomer.Surname2)).
   add_string(pcStruct,"site_name",fToUTF8(OrderCustomer.Company)).
   add_string(pcStruct,"region",fToUTF8(OrderCustomer.Region)).
   add_int(pcStruct,"zip",INT(OrderCustomer.ZipCode)).
   add_string(pcStruct,"city",fToUTF8(OrderCustomer.PostOffice)).
   add_string(pcStruct,"country",fToUTF8(OrderCustomer.Country)).
   add_string(pcStruct,"nationality",fToUTF8(OrderCustomer.Nationality)).
   add_string(pcStruct,"email",fToUTF8(OrderCustomer.Email)).
   add_string(pcStruct,"title",fToUTF8(OrderCustomer.custtitle)).
   add_string(pcStruct,"sms_number",fToUTF8(OrderCustomer.MobileNumber)).
   add_string(pcStruct,"phone_number",fToUTF8(OrderCustomer.FixedNumber)).
   add_string(pcStruct,"street_code",fToUTF8(OrderCustomer.AddressCodC)).
   add_string(pcStruct,"city_code",fToUTF8(OrderCustomer.AddressCodP)).
   add_datetime(pcStruct,"birthday",OrderCustomer.Birthday).
   add_boolean(pcStruct,"self_employed",OrderCustomer.SelfEmployed).
   add_datetime(pcStruct,"foundation_date",OrderCustomer.FoundationDate).
   add_string(pcStruct,"language",fToUTF8(OrderCustomer.Language)).
   add_boolean(pcStruct,"mark_sms",OrderCustomer.OperSMSMarketing).
   add_boolean(pcStruct,"mark_email",OrderCustomer.OperEmailMarketing).
   add_boolean(pcStruct,"mark_post",OrderCustomer.OperPostMarketing).
   add_boolean(pcStruct,"mark_sms_3rd",OrderCustomer.OutSMSMarketing).
   add_boolean(pcStruct,"mark_email_3rd",OrderCustomer.OutEMailMarketing).
   add_boolean(pcStruct,"mark_post_3rd",OrderCustomer.OutPostMarketing).
   add_string(pcStruct,"id_type",fToUTF8(OrderCustomer.CustIdType)).
 
   IF Order.OrdererId NE "" AND OrderCustomer.RowType NE 5 THEN DO:
      add_string(pcStruct,"person_id",fToUTF8(Order.OrdererId)).
      add_string(pcStruct,"company_id",fToUTF8(OrderCustomer.CustId)).
   END.
   ELSE 
      add_string(pcStruct,"person_id",fToUTF8(OrderCustomer.CustId)).

   IF OrderCustomer.BuildingNum NE "" THEN
      add_string(pcStruct,"building_number",fToUTF8(OrderCustomer.BuildingNum)).

   IF OrderCustomer.AddressCompl NE "" THEN 
      add_string(pcStruct,"complements",fToUTF8(OrderCustomer.AddressCompl)).


   IF OrderCustomer.Street NE "" THEN
      add_string(pcStruct,"street", fToUTF8(OrderCustomer.Street)).
   ELSE 
      add_string(pcStruct,"street",fToUTF8(OrderCustomer.Address)).

   add_boolean(pcStruct,"retrieved",OrderCustomer.custDataRetr).
   RETURN TRUE.

END.

/* Address Struct */
FUNCTION fFillAddressStruct RETURNS LOGICAL 
         (INPUT pcStruct AS CHARACTER):
 
   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand = gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND 
              OrderCustomer.RowType = 4 NO-LOCK NO-ERROR.

   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.

   add_string(pcStruct,"fname",fToUTF8(OrderCustomer.FirstName)).
   add_string(pcStruct,"lname", fToUTF8(OrderCustomer.Surname1)).
   add_string(pcStruct,"lname2",fToUTF8(OrderCustomer.Surname2)).
   add_string(pcStruct,"region",fToUTF8(OrderCustomer.Region)).
   add_int(pcStruct,"zip",INT(OrderCustomer.ZipCode)).
   add_string(pcStruct,"city",fToUTF8(OrderCustomer.PostOffice)).

   add_string(pcStruct,"street_code",fToUTF8(OrderCustomer.AddressCodC)).
   add_string(pcStruct,"city_code",fToUTF8(OrderCustomer.AddressCodP)).

   IF OrderCustomer.BuildingNum NE "" THEN
      add_string(pcStruct,"building_number",fToUTF8(OrderCustomer.BuildingNum)).

   IF OrderCustomer.AddressCompl NE "" THEN 
      add_string(pcStruct,"complements",fToUTF8(OrderCustomer.AddressCompl)).
   IF OrderCustomer.Street NE "" THEN 
      add_string(pcStruct,"street",fToUTF8(OrderCustomer.Street)).
   ELSE 
      add_string(pcStruct,"street",fToUTF8(OrderCustomer.Address)).

   IF OrderCustomer.KialaCode > "" THEN 
      add_string(pcStruct,"kiala_code",fToUTF8(OrderCustomer.KialaCode)). 

   RETURN TRUE.
END.


/* Accessory Struct */
FUNCTION fFillAccessoryStruct RETURNS LOGICAL 
         (INPUT pcStruct AS CHARACTER):

   FIND FIRST OrderAccessory NO-LOCK WHERE
              OrderAccessory.Brand = gcBrand AND
              OrderAccessory.OrderId = Order.OrderId AND
              OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE}) NO-ERROR.
   IF NOT AVAIL OrderAccessory THEN RETURN FALSE.
   add_string(pcStruct,"imei",fToUTF8(OrderAccessory.IMEI)).
   IF OrderAccessory.Discount NE 0 THEN 
   add_double(pcStruct,"discount",OrderAccessory.Discount).
   
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
            Order.Brand = gcBrand AND
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
   DEFINE VARIABLE lcResp AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDescription AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liStatus AS INTEGER NO-UNDO INITIAL 1.  
   DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO.
   DEFINE VARIABLE liReTry AS INTEGER NO-UNDO. 

   xmlrpc_cleanup().

   FIND Order WHERE Order.Brand = gcBrand AND
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

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLog( "ROI History, ERROR Creating message: " + gc_xmlrpc_error,"NW_ERR"). 
      PUT SCREEN ROW 23 COL 4  "ERROR Creating message " + gc_xmlrpc_error.
      /* save the exception in the ErrorLog */
      ldTS = fMakeTS().
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand = gcBrand
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
      ldTS = fMakeTS().
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand = gcBrand
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
