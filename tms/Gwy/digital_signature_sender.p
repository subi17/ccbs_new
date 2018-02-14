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
DEF VAR llLogRequest    AS LOG  NO-UNDO INIT TRUE.
DEF VAR lcUrlAdapter    AS CHAR NO-UNDO.

DEF BUFFER bOrder FOR Order.
DEF STREAM sLog.

FUNCTION fLogMsg RETURNS LOGICAL
   (icMessage AS CHAR):
   PUT STREAM sLog UNFORMATTED
      icMessage SKIP.
END FUNCTION.


/*
   Function constructs brand data for Adapter.
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
      fLogMsg(STRING(iiOrderID) + "; Error: tenant not found (Yoigo or Masmovil).").
      RETURN FALSE.
   END.
   IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Signature xml, Brand: " + STRING(pcStruct)).

   RETURN TRUE.

END FUNCTION.


/*
   Function constructs data for Adapter.
*/
FUNCTION fFillCancelStruct RETURNS LOGICAL
   (iiOrderId      AS INT,
    INPUT pcStruct AS CHAR):

   DEF BUFFER bOrder FOR Order.
   DEF BUFFER bOrderCustomer FOR OrderCustomer.

   FIND FIRST bOrder NO-LOCK WHERE
              bOrder.Brand EQ Syst.Var:gcBrand AND
              bOrder.OrderId EQ iiOrderId.
   IF AVAIL bOrder THEN DO:
      FIND FIRST bOrderCustomer NO-LOCK WHERE
                 bOrderCustomer.Brand EQ Syst.Var:gcBrand AND
                 bOrderCustomer.OrderId EQ iiOrderId.

      IF NOT AVAIL bOrderCustomer THEN RETURN FALSE.

      add_string(pcStruct,"id","12341"). /* TODO: get with process id query to Adapter */
      
      add_string(pcStruct,"customerId",bOrderCustomer.CustId).

      add_string(pcStruct,"accountId",bOrder.ContractID).

      /* not mandatory */
      add_string(pcStruct,"subscriptionId",bOrder.CLI).

      add_string(pcStruct,"cancelReason","cancelled").
      
      IF llLogRequest THEN fLogMsg(STRING(bOrder.OrderId) +  "; Cancel send xml: " + STRING(pcStruct)).
   END.

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
   DEF VAR lcBrandStruct AS CHAR NO-UNDO.
   DEF VAR lcRespStruct  AS CHAR NO-UNDO. 
   DEF VAR lcResp        AS CHAR NO-UNDO. 
   DEF VAR lcResult      AS CHAR NO-UNDO. 
   DEF VAR lcDescription AS CHAR NO-UNDO.

   IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Start construct cancel message").
   xmlrpc_cleanup().
   lcBrandStruct = add_struct(param_toplevel_id,"").
   fFillBrandStruct(iiOrderId, icBrand, lcBrandStruct).

   lcOrderStruct = add_struct(param_toplevel_id,"").
   fFillCancelStruct(iiOrderId, lcOrderStruct). 

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR Creating cancel message: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   fLogMsg(STRING(iiOrderId) + "; Call RPC Method (Adapter)").
   RUN pRPCMethodCall("digitalSignature.updateProcess", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR Sending cancel Message"). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   lcRespStruct = get_struct(response_toplevel_id, "0").
   IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Response to cancel from Adapter: " + lcrespStruct).

   IF gi_xmlrpc_error EQ 0 THEN
      lcResp = validate_request(lcRespStruct,"result!,description").

   IF gi_xmlrpc_error EQ 0 THEN DO:
      lcResult = get_string(lcRespStruct,"result").
      IF LOOKUP("description",lcResp) GT 0 THEN
         lcDescription = get_string(lcRespStruct,"description").
   END.

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR in cancel Response: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   IF lcResult EQ "Ok" THEN DO:
      /* set message as sent */
      IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Cancel message to Adapter sent successfully!").
   END.
   ELSE DO: 
      /* anything to do? */   
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

   FIND FIRST bOrder NO-LOCK WHERE
            bOrder.Brand EQ Syst.Var:gcBrand AND
            bOrder.OrderId EQ iiOrderId.
   IF AVAIL bOrder THEN DO:
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

      add_string(pcStruct,"legalIdentityType",bOrderCustomer.CustIdType).

      add_string(pcStruct,"legalIdentityCountry",bOrderCustomer.Country).

      add_string(pcStruct,"legalIdentityNumber",bOrderCustomer.CustId).

      IF fIsConvergent3POnly(bOrder.CLIType) THEN
         add_string(pcStruct,"sellType","CONVERGENTE").
      ELSE IF fIsFixedOnly(bOrder.CLIType) THEN
         add_string(pcStruct,"sellType","FIXED_ONLY").
      ELSE
         add_string(pcStruct,"sellType","SIM_ONLY").

      add_string(pcStruct,"crmId",bOrder.OrderChannel).

      add_string(pcStruct,"dealerId",bOrder.Salesman).

      add_string(pcStruct,"contractId",bOrder.ContractID).

      add_string(pcStruct,"orderId",STRING(bOrderCustomer.OrderId)).

      add_string(pcStruct,"sfId",bOrder.Salesman).

      FIND FIRST OrderTimeStamp NO-LOCK WHERE
                 OrderTimeStamp.Brand EQ Syst.Var:gcBrand AND
                 OrderTimeStamp.RowType EQ bOrderCustomer.RowType AND
                 OrderTimeStamp.OrderID EQ bOrderCustomer.OrderId USE-INDEX RowType.
      IF AVAIL OrderTimeStamp THEN
         add_string(pcStruct,"orderDate",STRING(Func.Common:mTSToDate(bOrder.CrStamp ))).
      ELSE
         add_string(pcStruct,"orderDate",STRING(0)).
         
      IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Signing send xml: " + STRING(pcStruct)).
   END.

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

   IF llLogRequest THEN fLogMsg(STRING(iiOrderId) + "; Start construct signing message").
   xmlrpc_cleanup().
   lcBrandStruct = add_struct(param_toplevel_id,"").
   fFillBrandStruct(iiOrderId, icBrand, lcBrandStruct). 

   lcOrderStruct = add_struct(param_toplevel_id,"").
   fFillOrderStruct(iiOrderId, lcOrderStruct). 

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR Creating message: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   fLogMsg(STRING(iiOrderId) + "; Call RPC Method (Adapter), OrderId: ").
   RUN pRPCMethodCall("digitalSignature.registerSignProcess", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderId) + "; ERROR Sending Message"). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   lcRespStruct = get_struct(response_toplevel_id, "0").
   IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Response from Adapter: " + lcrespStruct).

   IF gi_xmlrpc_error EQ 0 THEN
      lcResp = validate_request(lcRespStruct,"result!,description").

   IF gi_xmlrpc_error EQ 0 THEN DO:
      lcResult = get_string(lcRespStruct,"result").
      IF LOOKUP("description",lcResp) GT 0 THEN
         lcDescription = get_string(lcRespStruct,"description").
   END.
   IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Result from Adapter: " + lcResult).

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR in Response: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   IF lcResult EQ "Ok" THEN DO:
      /* set message as sent */
      IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Message to Adapter sent successfully!").
   END.
   ELSE DO: 
      /* anything to do? */
   END.

   RETURN "".

END FUNCTION.

/*
   Procedure checks from ActionLog if request must be
   sent to Adapter (Signature API).
*/
PROCEDURE pCheckActionLog:

   DEFINE INPUT PARAMETER pcActionID AS CHAR NO-UNDO.

   DEF VAR lcStatus AS CHAR NO-UNDO INIT "".

   FOR EACH ActionLog EXCLUSIVE-LOCK WHERE
            ActionLog.Brand     EQ Syst.Var:gcBrand AND
            ActionLog.TableName EQ lcTableName      AND
            ActionLog.ActionID  EQ pcActionID       AND
            ActionLog.ActionTS  =  DEC(0) USE-INDEX ActionID:

      IF AVAIL ActionLog THEN
         IF ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN
            NEXT.
   DO TRANS:
      IF AVAIL ActionLog THEN DO:
          IF ActionLog.ActionID NE "ContractStatusSent" OR
             ActionLog.ActionID NE "ContractStatusCancelled" THEN
             NEXT.
      END.
      ELSE DO:
         FIND FIRST bOrder NO-LOCK WHERE
                    bOrder.Brand EQ Syst.Var:gcBrand AND
                    STRING(bOrder.OrderId) EQ ActionLog.KeyValue NO-ERROR.
         IF NOT AVAIL bOrder THEN DO:
            fLogMsg(STRING(ActionLog.KeyValue) + "; ERROR not found Order").
            NEXT.
         END.

         IF AVAIL bOrder THEN DO:
            IF llLogRequest THEN fLogMsg(STRING(bOrder.OrderId) + "; Found OrderId " + 
                    ", OrderStatusCode: " + STRING(bOrder.StatusCode) + 
                    ", ActionLog.ActionID: " + STRING(ActionLog.ActionID)).
            IF bOrder.statusCode EQ {&ORDER_STATUS_DELIVERED} THEN
               /* Send for signing */
               lcStatus = fSendSigningMessage(bOrder.OrderId, bOrder.Brand).
            ELSE IF LOOKUP(bOrder.StatusCode, {&ORDER_CLOSE_STATUSES}) > 0 THEN /* 7,8,9 */
               /* Send cancel */
               lcStatus = fSendCancelMessage(bOrder.OrderId, bOrder.Brand).
            ELSE
               fLogMsg(STRING(bOrder.OrderId) + "; ERROR wrong Order status: " + STRING(bOrder.StatusCode)).

            IF lcStatus EQ "" THEN DO:
               ASSIGN
                  ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS} /* ?? */
                  ActionLog.ActionTS     = ldCurrentTimeTS.
               RELEASE ActionLog.
            END. /* ELSE? Leave untouched and try again in next cron job */
         END.
      END.
   END. /* DO TRANS */
   END. /* FOR EACH */

END PROCEDURE.


/* MAIN START */

lcTableName = "Order".
ldCurrentTimeTS = Func.Common:mMakeTS().

ASSIGN
   IF(fIParam("SignatureApi", "LogRequest")) EQ 0 THEN
      llLogRequest  = FALSE.
   lcLogdir      = fCParam("SignatureApi", "LogDir")
   /* lcLogDir   = "/scratch/log/digitalsignature/"*/
   lcUrlAdapter  = fCParam("SignatureApi", "UrlAdapter").
   /* e.g. http://217.168.2.239:7001/com-yoigo-roi-webapp/xmlrpc */

IF lcUrlAdapter = ? OR lcUrlAdapter = "" THEN DO:
   fLogMsg("Adapter URL not defined (TMS->Adapter). QUIT program. " + Func.Common:mTS2HMS(ldCurrentTimeTS)).
   QUIT.
END.

lcLog = lcLogDir + 
        "digital_signature_request" +
        STRING(YEAR(TODAY)) +
        STRING(MONTH(TODAY),"99") +
        STRING(DAY(TODAY),"99") + ".log".
OUTPUT STREAM sLog TO VALUE(lcLog) APPEND.

IF llLogRequest THEN fLogMsg("Started by Cron at " + Func.Common:mTS2HMS(ldCurrentTimeTS)).

iTimeOut = 10.
initialize(lcUrlAdapter, iTimeOut).

RUN pCheckActionLog("ContractStatusSent").
RUN pCheckActionLog("ContractStatusCancelled").

xmlrpc_finalize().
OUTPUT STREAM sLog CLOSE.

