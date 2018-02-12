/*------------------------------------------------------------------------
  MODULE .......: digital_signature_reader.p
  TASK .........: Send Digital Signature requests to Adapter
  APPLICATION ..: TMS
  AUTHOR .......: kahannul
  CREATED ......: 09.02.18
  CHANGED ......: 
  Version ......: multibrand
-------------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN
   Syst.Var:katun   = "Cron".
  /* Syst.Var:gcBrand = "1".*/
{Syst/tmsconst.i}
{Func/profunc.i}
{Func/cparam2.i}
{fcgi_agent/xmlrpc/xmlrpc_client.i}

DEF VAR lcLogDir        AS CHAR NO-UNDO.
DEF VAR lcLog           AS CHAR NO-UNDO.
DEF VAR lcTableName     AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.

DEF VAR iTimeOut        AS INT  NO-UNDO.
DEF VAR liLogRequest    AS INT  NO-UNDO.
DEF VAR llLogRequest    AS LOG  NO-UNDO INIT TRUE.
DEF VAR lcUrlAdapter    AS CHAR NO-UNDO.

DEF BUFFER bOrder FOR Order.
DEF STREAM sLog.

FUNCTION fLogMsg RETURNS LOGICAL
   (icMessage AS CHAR):
   PUT STREAM sLog UNFORMATTED
      icMessage SKIP.
END FUNCTION.

/*Is feature active:*/
/* IF fDMSOnOff() NE TRUE THEN RETURN.*/

/*
   Function constructs data for Adapter.
*/
FUNCTION fFillCancelStruct RETURNS LOGICAL
   (iiOrderId      AS INT,
    INPUT pcStruct AS CHAR):

   RETURN TRUE.

END FUNCTION.


/*
   Function sends cancel request to
   Adapter (Signature API)
*/
FUNCTION fSendCancelMessage RETURNS CHAR
   (iiOrderId   AS INT,
    icBrand     AS CHAR):

   DEF VAR lcOrderStruct AS CHAR NO-UNDO.

   IF llLogRequest THEN fLogMsg("Construct cancel message, OrderId: " + STRING(iiOrderId)).
   xmlrpc_cleanup().
   lcOrderStruct = add_struct(param_toplevel_id,"").
   fFillCancelStruct(iiOrderId, lcOrderStruct). 

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg("ERROR Creating message: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   /* RUN pRPCMethodCall("ROIHistoryInterface.store_order", TRUE).*/ 

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg("ERROR Sending Message, OrderId: " + STRING(iiOrderId)). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   RETURN "".

END FUNCTION.


/*
   Function constructs data for Adapter.
*/
FUNCTION fFillOrderStruct RETURNS LOGICAL
   (iiOrderId      AS INT,
    INPUT pcStruct AS CHAR):

   DEF BUFFER bOrder FOR Order.
   DEF BUFFER bOrderCustomer FOR OrderCustomer.

   FOR EACH bOrder NO-LOCk WHERE
            bOrder.Brand EQ Syst.Var:gcBrand AND
            bOrder.OrderId EQ iiOrderId:
      FIND FIRST bOrderCustomer NO-LOCK WHERE
                 bOrderCustomer.Brand EQ Syst.Var:gcBrand AND
                 bOrderCustomer.OrderId EQ iiOrderId.

      IF NOT AVAIL bOrderCustomer THEN RETURN FALSE.

      add_string(pcStruct,"customerId",bOrderCustomer.CustId).

      add_string(pcStruct,"accountId",bOrder.ContractID).

      /* not mandatory */
      add_string(pcStruct,"subscriptionId",bOrder.CLI).

      add_string(pcStruct,"clientName",bOrderCustomer.FirstName).
    
      add_string(pcStruct,"clientLastName1",bOrderCustomer.SurName1).

      /* not mandatory */
      add_string(pcStruct,"clientLastName2",bOrderCustomer.SurName2).
    
      add_string(pcStruct,"clientEmail",bOrderCustomer.email).

      add_string(pcStruct,"clientPhone",bOrderCustomer.ContactNum).

      add_string(pcStruct,"clientMobile",bOrderCustomer.MobileNumber).

      /* not mandatory */
      add_string(pcStruct,"clientFax",""). /* Customer.fax if needed */

      add_string(pcStruct,"legalIdentityType",bOrderCustomer.CustIdType). /* NIF etc.??*/

      add_string(pcStruct,"legalIdentityCountry",bOrderCustomer.Country). /*??*/

      add_string(pcStruct,"legalIdentityNumber",bOrderCustomer.CustId). /*??*/

      add_string(pcStruct,"SellType", "SIM"). /*?????*/

      add_string(pcStruct,"crmId",bOrder.OrderChannel).

      add_string(pcStruct,"dealerId",bOrder.reseller).

      add_string(pcStruct,"contractId",bOrder.ContractID).

      add_string(pcStruct,"orderId",STRING(bOrderCustomer.OrderId)).

      add_string(pcStruct,"sfId",bOrder.Salesman).

      FIND FIRST OrderTimeStamp NO-LOCK WHERE
                 OrderTimeStamp.Brand EQ Syst.Var:gcBrand AND
                 OrderTimeStamp.OrderID EQ bOrder.OrderId NO-ERROR.
      IF AVAIL OrderTimeStamp THEN
         add_string(pcStruct,"orderDate",STRING(OrderTimeStamp.TimeStamp)).
      ELSE
         add_string(pcStruct,"orderDate",STRING(0)).
         
      IF llLogRequest THEN fLogMsg("Signing xml: " + STRING(pcStruct)).
   END.

   RETURN TRUE.

END FUNCTION.

/*
   Function constructs data for Adapter.
*/
FUNCTION fFillbrandStruct RETURNS LOGICAL
   (iiOrderId      AS INT,
    icBrand        AS CHAR,
    INPUT pcStruct AS CHAR):

   IF fConvertBrandToTenant(icBrand) EQ {&TENANT_YOIGO} THEN
      add_string(pcStruct,"brand","yoigo").
   ELSE IF fConvertBrandToTenant(icBrand) EQ {&TENANT_MASMOVIL} THEN
      add_string(pcStruct,"brand","masmovil").
   ELSE DO:
      fLogMsg("Error: tenant not found (Yoigo or Masmovil).").
      RETURN FALSE.
   END.
   IF llLogRequest THEN fLogMsg("Signing xml, Brand: " + STRING(pcStruct)).

   RETURN TRUE.

END FUNCTION.


/*
   Function sends signing request to
   Adapter (Signature API)
*/
FUNCTION fSendSigningMessage RETURNS CHAR
   (iiOrderId   AS INT,
    icBrand     AS CHAR):

   DEF VAR lcOrderStruct AS CHAR NO-UNDO.
   DEF VAR lcBrandStruct AS CHAR NO-UNDO.
   DEF VAR lcRespStruct  AS CHAR NO-UNDO. 
   DEF VAR lcResp        AS CHAR NO-UNDO. 
   DEF VAR lcResult      AS CHAR NO-UNDO. 
   DEF VAR lcDescription AS CHAR NO-UNDO.

   IF llLogRequest THEN fLogMsg("Start construct signing message, OrderId: " + STRING(iiOrderId)).
   xmlrpc_cleanup().
   lcBrandStruct = add_struct(param_toplevel_id,"").
   fFillBrandStruct(iiOrderId, icBrand, lcBrandStruct). 

   lcOrderStruct = add_struct(param_toplevel_id,"").
   fFillOrderStruct(iiOrderId, lcOrderStruct). 

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg("ERROR Creating message: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   fLogMsg("Call RPC Method (Adapter), OrderId: " + STRING(iiOrderId)).
   /* RUN pRPCMethodCall("ROIHistoryInterface.store_order", TRUE).*/ 

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg("ERROR Sending Message, OrderId: " + STRING(iiOrderId)). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   lcRespStruct = get_struct(response_toplevel_id, "0").
   IF llLogRequest THEN fLogMsg("Response from Adapter: " + lcrespStruct).

   IF gi_xmlrpc_error EQ 0 THEN
      lcResp = validate_request(lcRespStruct,"result!,description").

   IF gi_xmlrpc_error EQ 0 THEN DO:
      lcResult = get_string(lcRespStruct,"result").
      IF LOOKUP("description",lcResp) GT 0 THEN
         lcDescription = get_string(lcRespStruct,"description").
   END.

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg("ERROR in Response: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   IF lcResult EQ "Ok" THEN DO:
      /* set message as sent */
      IF llLogRequest THEN fLogMsg("Message to Adapter (OrderId " + STRING(iiOrderID) + ") sent successfully!").
   END.
   ELSE DO: /* anything to do? */
      /* save the exception in the ErrorLog */
      /* ldTS = Func.Common:mMakeTS().
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand = Syst.Var:gcBrand
             ErrorLog.TableName = "Order"
             ErrorLog.KeyValue = STRING(Order.OrderId)
             ErrorLog.ActionID = "ROIHistory"
             ErrorLog.ActionTS = ldTS
             ErrorLog.ErrorMsg = "ROI Response: " + lcDescription
             liStatus = 3.
      RELEASE ErrorLog.*/
   END.

   RETURN "".

END FUNCTION.

/*
   Procedure checks from ActionLog if request must be
   sent to Adapter (Signature API).
*/
PROCEDURE pCheckActionLog:

   DEF VAR lcStatus AS CHAR NO-UNDO INIT "".

DO TRANS:

   FOR EACH ActionLog WHERE
            ActionLog.Brand     EQ Syst.Var:gcBrand AND
            ActionLog.TableName EQ lcTableName      AND
            ActionLog.ActionTS  =  DEC(0) USE-INDEX Tablename:

      IF AVAIL ActionLog THEN
         IF ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN
         QUIT. /* ?? */
  
      IF AVAIL ActionLog THEN DO:
          IF ActionLog.ActionID NE "ContractStatusSent" OR
             ActionLog.ActionID NE "ContractStatusCancelled" THEN
             NEXT.
      END.
      ELSE DO:
         FIND FIRST bOrder NO-LOCK WHERE
                    bOrder.Brand EQ "1" /* Syst.Var:gcBrand*/ AND
                    STRING(bOrder.OrderId) EQ ActionLog.KeyValue NO-ERROR.
         IF NOT AVAIL bOrder THEN DO:
            fLogMsg("ERROR not found Order, OrderId: " + STRING(bOrder.OrderId)).
            NEXT.
         END.

         IF AVAIL bOrder THEN DO:
            IF llLogRequest THEN fLogMsg("Found OrderId: " + STRING(bOrder.OrderId) + 
                    ", OrderStatusCode: " + STRING(bOrder.StatusCode) + 
                    ", ActionLog.ActionID: " + STRING(ActionLog.ActionID)).
            IF bOrder.statusCode EQ {&ORDER_STATUS_DELIVERED} THEN
               /* Send for signing */
               lcStatus = fSendSigningMessage(bOrder.OrderId, bOrder.Brand).
            ELSE IF LOOKUP(bOrder.StatusCode, {&ORDER_CLOSE_STATUSES}) > 0 THEN /* 7,8,9 */
               /* Send cancel */
               lcStatus = fSendCancelMessage(bOrder.OrderId, bOrder.Brand).
            ELSE
               fLogMsg("ERROR wrong Order status, OrderId: " + STRING(bOrder.OrderId) + 
                       ", status: " + STRING(bOrder.StatusCode)).

            IF lcStatus EQ "" THEN DO:
               ASSIGN
                  ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS} /* ?? */
                  ActionLog.ActionTS     = ldCurrentTimeTS.
               RELEASE ActionLog.
            END. /* ELSE? Leave untouched and try again in next cron job */
         END.
      END.

      /* ELSE DO:
         ASSIGN
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
            ActionLog.UserCode     = Syst.Var:katun
            ActionLog.ActionTS     = ldCurrentTimeTS.

         RELEASE Actionlog.
      END.*/
   END.
END.
END PROCEDURE.


/* MAIN START */

lcTableName = "Order".
ldCurrentTimeTS = Func.Common:mMakeTS().

ASSIGN
   /*liLogRequest  = fIParam("SignatureApi", "LogRequest")*/
   llLogRequest  = LOGICAL(liLogRequest)
   /* lcLogdir        = fCParam("SignatureApi", "LogDir")*/
   lcLogDir   = "/scratch/log/digitalsignature/"
   /* lcUrlAdapter  = fCParam("SignatureApi", "UrlAdapter")*/
   /* e.g. http://217.168.2.239:7001/com-yoigo-roi-webapp/xmlrpc */.

lcLog = lcLogDir + 
        "digital_signature_request" +
        STRING(YEAR(TODAY)) +
        STRING(MONTH(TODAY),"99") +
        STRING(DAY(TODAY),"99") + ".log".
OUTPUT STREAM sLog TO VALUE(lcLog) APPEND.

IF llLogRequest THEN fLogMsg("Started by Cron at " + Func.Common:mTS2HMS(ldCurrentTimeTS)).

iTimeOut = 10.
initialize(lcUrlAdapter, iTimeOut).

RUN pCheckActionLog.

xmlrpc_finalize().
OUTPUT STREAM sLog CLOSE.

