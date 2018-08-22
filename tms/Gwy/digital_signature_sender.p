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

{Syst/tmsconst.i}
{Func/profunc.i}
{Func/cparam2.i}
{fcgi_agent/xmlrpc/xmlrpc_client.i}

DEF VAR lcLogDir        AS CHAR NO-UNDO.
DEF VAR lcLog           AS CHAR NO-UNDO.
DEF VAR lcTableName     AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.
DEF VAR lcXmlLog        AS CHAR NO-UNDO.

DEF VAR iTimeOut        AS INT  NO-UNDO.
DEF VAR llLogRequest    AS LOG  NO-UNDO INIT TRUE.
DEF VAR lcUrlAdapter    AS CHAR NO-UNDO.

DEF BUFFER bOrder FOR Order.
DEF STREAM sLog.
DEF STREAM sOut.

/*
   Function prints XML to file.
*/
FUNCTION fXMLGenerateTest RETURNS CHAR
   (icMethod AS CHAR):
   IF llLogRequest THEN DO:
      xmlrpc_initialize(FALSE).

      lcXmlLog = lcLogDir +
         "digital_signature_xml_" +
         STRING(YEAR(TODAY)) +
         STRING(MONTH(TODAY),"99") +
         STRING(DAY(TODAY),"99") + ".xml".

      OUTPUT STREAM sOut TO VALUE(lcXmlLog) APPEND.
      PUT STREAM sOut UNFORMATTED 
         string(serialize_rpc_call(icMethod)) SKIP. 
      PUT STREAM sOut "" SKIP.   
      OUTPUT STREAM sOut CLOSE.
      xmlrpc_initialize(FALSE).
   END.   
END. 


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

   DO ON ERROR UNDO, THROW:

   add_string(pcStruct,"brand",LC(multitenancy.TenantInformation:mGetEffectiveBrand())).
       CATCH errorobj AS Progress.Lang.AppError:
          fLogMsg(STRING(iiOrderID) + "; Error: " + errorobj:GetMessage(1)).
          RETURN FALSE.
      END.
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
              bOrder.OrderId EQ iiOrderId NO-ERROR.
   IF AVAIL bOrder THEN DO:
      FIND FIRST bOrderCustomer NO-LOCK WHERE
                 bOrderCustomer.Brand EQ Syst.Var:gcBrand AND
                 bOrderCustomer.OrderId EQ iiOrderId AND
                 bOrderCustomer.RowType = 1 NO-ERROR.

      IF NOT AVAIL bOrderCustomer THEN RETURN FALSE.

      add_string(pcStruct,"customerId",bOrderCustomer.CustId).
      add_string(pcStruct,"accountId",bOrder.ContractID).
      /* not mandatory */
      add_string(pcStruct,"subscriptionId",bOrder.CLI).
      add_string(pcStruct,"cancelReason","cancelled").
      
      IF llLogRequest THEN DO:
         fLogMsg(STRING(bOrder.OrderId) +  "; Cancel send xml: " + STRING(pcStruct)).
         fLogMsg(STRING(bOrder.OrderId) +  "; Print xml to file").
         fXMLGenerateTest("sign.updateProcess").
      END.
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
   DEF VAR lcResult      AS CHAR NO-UNDO. /* httpCode */ 
   DEF VAR lcDescription AS CHAR NO-UNDO. /* info */
   DEF VAR lcCode        AS CHAR NO-UNDO. /* code */
   DEF VAR lcResultCode  AS CHAR NO-UNDO.
   DEF VAR lcResultDesc  AS CHAR NO-UNDO.
   DEF VAR ldCurrentTS   AS DEC  NO-UNDO.

   ldCurrentTS = Func.Common:mMakeTS().

   IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Start construct cancel message").
   xmlrpc_cleanup().
   lcBrandStruct = add_struct(param_toplevel_id,"").
   IF NOT fFillBrandStruct(iiOrderId, icBrand, lcBrandStruct) THEN
      RETURN "Error".

   lcOrderStruct = add_struct(param_toplevel_id,"").
   IF NOT fFillCancelStruct(iiOrderId, lcOrderStruct) THEn DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR Constructing cancel struct.").
      RETURN "Error".
   END.

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR Creating cancel message: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   /* SEND message */
   fLogMsg(STRING(iiOrderId) + "; Call RPC Method (Adapter) at " + Func.Common:mTS2HMS(ldCurrentTS)).
   RUN pRPCMethodCall("sign.updateProcess", TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
      lcResultCode = STRING(gi_xmlrpc_error).
      lcResultDesc = gc_xmlrpc_error.
      fLogMsg(STRING(iiOrderId) + "; ERROR Sending cancel Message: ResultCode: " + lcResultCode + ", ResultDesc: " + lcResultDesc + ", " + STRING(gi_xmlrpc_error)). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   lcRespStruct = get_struct(response_toplevel_id, "0").

   IF gi_xmlrpc_error EQ 0 THEN
      lcResp = validate_request(lcRespStruct,"info,code,httpCode").

   IF gi_xmlrpc_error EQ 0 THEN DO:
      lcResult = get_string(lcRespStruct,"httpCode").
      IF LOOKUP("info",lcResp) GT 0 THEN
         lcDescription = get_string(lcRespStruct,"info").
      IF LOOKUP("code",lcResp) GT 0 THEN
         lcCode = get_string(lcRespStruct,"code").
   END.
   IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Cancel Result from Adapter, httpCode: " + lcResult + ", info: " + lcDescription + ", code: " + lcCode ).

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR in cancel Response: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   IF lcResult EQ "200" THEN DO:
      /* set message as sent */
      IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Cancel message to Adapter sent successfully!").
   END.
   ELSE DO: 
      /* Seems that httpCode 500 falls here */
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

   DEF VAR lcContactMediumStruct AS CHAR NO-UNDO.
   DEF VAR lcMediumStruct        AS CHAR NO-UNDO.
   DEF VAR lcCountryIso3         AS CHAR NO-UNDO.

   DEF BUFFER bOrder FOR Order.
   DEF BUFFER bOrderCustomer FOR OrderCustomer.

   FIND FIRST bOrder NO-LOCK WHERE
            bOrder.Brand EQ Syst.Var:gcBrand AND
            bOrder.OrderId EQ iiOrderId NO-ERROR.
   IF AVAIL bOrder THEN DO:
      FIND FIRST bOrderCustomer NO-LOCK WHERE
                 bOrderCustomer.Brand EQ Syst.Var:gcBrand AND
                 bOrderCustomer.OrderId EQ iiOrderId AND
                 bOrderCustomer.RowType = 1 NO-ERROR.

      IF NOT AVAIL bOrderCustomer THEN RETURN FALSE.

      FIND FIRST Country NO-LOCk WHERE
         Country.Country EQ bOrderCustomer.Country NO-ERROR.
      IF AVAIL Country THEN
         lcCountryIso3 = Country.CountryISO3.
      ELSE
         lcCountryIso3 = "SDT". /* Sin determinal - not determined */

      add_string(pcStruct,"firstName",bOrderCustomer.FirstName).
      add_string(pcStruct,"midName",bOrderCustomer.SurName1).
      add_string(pcStruct,"lastName",bOrderCustomer.SurName2).

      /* contactMedium */
      add_string(pcStruct,"medium.emailAddress",bOrderCustomer.email).

      IF bOrderCustomer.FixedNumber NE "" THEN
         add_string(pcStruct,"medium.fixedNumber",bOrderCustomer.FixedNumber).
      ELSE
         add_string(pcStruct,"medium.fixedNumber",bOrderCustomer.MobileNumber).

      add_string(pcStruct,"medium.mobileNumber",bOrderCustomer.MobileNumber).
      /* fax not mandatory */
      add_string(pcStruct,"medium.faxNumber","").

      /* individualIdentification */
      add_string(pcStruct,"individualIdentification.type",bOrderCustomer.CustIdType).
      add_string(pcStruct,"individualIdentification.identificationId",bOrderCustomer.CustId).
      add_string(pcStruct,"individualIdentification.country",lcCountryIso3).

      /* processData */
      add_string(pcStruct,"accountId",bOrder.ContractID).
      add_string(pcStruct,"subscriptionId",bOrder.CLI).

      IF fIsConvergent3POnly(bOrder.CLIType) THEN
         add_string(pcStruct,"sellType","CONVERGENTE").
      ELSE IF fIsFixedOnly(bOrder.CLIType) THEN
         add_string(pcStruct,"sellType","FIXED_ONLY").
      ELSE
         add_string(pcStruct,"sellType","SIM_ONLY").

      add_string(pcStruct,"crmId",bOrder.OrderChannel).

      IF bOrder.Reseller NE "" THEN      
         add_string(pcStruct,"dealerId",bOrder.Reseller).
      ELSE
         add_string(pcStruct,"dealerId","web").

      add_string(pcStruct,"contractId",bOrder.ContractID).
      add_string(pcStruct,"orderId",STRING(bOrderCustomer.OrderId)).
      add_string(pcStruct,"sfId",bOrder.Salesman).
      add_string(pcStruct,"orderDate",Func.Common:mUTCTime(bOrder.CrStamp)).
      add_string(pcStruct,"paymentMethod","1"). /* postpaid */

      IF bOrderCustomer.SelfEmployed THEN DO:
         add_string(pcStruct,"customerType","3").
      END.
      ELSE DO:
         IF bOrderCustomer.CustIdType EQ "NIF" OR
            bOrderCustomer.CustIdType EQ "NIE" THEN
            add_string(pcStruct,"customerType","1").
         ELSE IF bOrderCustomer.CustIdType EQ "CIF" THEN
            add_string(pcStruct,"customerType","2").
         ELSE
            add_string(pcStruct,"customerType","0"). /* not found value */
      END.

      IF bOrder.OrderType = {&ORDER_TYPE_NEW} THEN
         add_string(pcStruct,"contractType","1").
      ELSE IF bOrder.OrderType = {&ORDER_TYPE_MNP} THEN
         add_string(pcStruct,"contractType","2").
      ELSE IF bOrder.OrderType = {&ORDER_TYPE_RENEWAL} THEN
         add_string(pcStruct,"contractType","3").
      ELSE
         add_string(pcStruct,"contractType","4"). /* STC, ... */
      /* TMS has not definition for contractType=4 (Migration) in specs */

      add_boolean(pcStruct,"financed",FALSE).
 
      IF llLogRequest THEN DO:
         fLogMsg(STRING(iiOrderID) + "; Signing send xml: " + STRING(pcStruct)).
         fLogMsg(STRING(bOrder.OrderId) +  "; Print xml to file").
         fXMLGenerateTest("sign.registerSignProcess").
      END.
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
   DEF VAR lcResult      AS CHAR NO-UNDO. /* httpCode */
   DEF VAR lcDescription AS CHAR NO-UNDO. /* info */
   DEF VAR lcCode        AS CHAR NO-UNDO. /* code */
   DEF VAR lcResultCode  AS CHAR NO-UNDO.
   DEF VAR lcResultDesc  AS CHAR NO-UNDO.
   DEF VAR ldCurrentTS   AS DEC  NO-UNDO.

   ldCurrentTS = Func.Common:mMakeTS().

   IF llLogRequest THEN fLogMsg(STRING(iiOrderId) + "; Start construct signing message").
   xmlrpc_cleanup().
   lcBrandStruct = add_struct(param_toplevel_id,"").
   IF NOT fFillBrandStruct(iiOrderId, icBrand, lcBrandStruct) THEN
      RETURN "Error".

   lcOrderStruct = add_struct(param_toplevel_id,"").
   IF NOT fFillOrderStruct(iiOrderId, lcOrderStruct) THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR Constructing order struct.").
      RETURN "Error".
   END.

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR Creating message: " + gc_xmlrpc_error). 
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   /* SEND message */
   fLogMsg(STRING(iiOrderId) + "; Call RPC Method (Adapter) at " + Func.Common:mTS2HMS(ldCurrentTS)).
   RUN pRPCMethodCall("sign.registerSignProcess", TRUE).

    lcRespStruct = get_struct(response_toplevel_id, "0").
 
   IF gi_xmlrpc_error EQ 0 THEN
      lcResp = validate_request(lcRespStruct,"info,code,httpCode").

   IF gi_xmlrpc_error EQ 0 THEN DO:
      lcResult = get_string(lcRespStruct,"httpCode").
      IF LOOKUP("info",lcResp) GT 0 THEN
         lcDescription = get_string(lcRespStruct,"info").
     IF LOOKUP("code",lcResp) GT 0 THEN
         lcCode = get_string(lcRespStruct,"code").
   END.
   IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Result from Adapter, httpCode: " + lcResult + ", info: " + lcDescription + ", code: " + lcCode ).

   IF gi_xmlrpc_error NE 0 THEN DO:
      fLogMsg(STRING(iiOrderID) + "; ERROR in Response: " + gc_xmlrpc_error).
      xmlrpc_cleanup().
      RETURN "Error".
   END.

   IF lcResult EQ "200" THEN DO:
      /* set message as sent */
      IF llLogRequest THEN fLogMsg(STRING(iiOrderID) + "; Message to Adapter sent successfully!").
   END.
   ELSE DO:
      /* Seems that httpCode 500 falls here */
      RETURN "Error".
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

   IF llLogRequest THEN fLogMsg("Start searching ActionLog, ActionID: " + pcActionID).

   FOR EACH ActionLog NO-LOCK WHERE
            ActionLog.Brand     EQ Syst.Var:gcBrand AND
            ActionLog.ActionID  EQ pcActionID       AND
            ActionLog.ActionTS  =  DEC(0) USE-INDEX ActionID:

      IF ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN
         NEXT.

      DO TRANS:      
         FIND FIRST bOrder NO-LOCK WHERE
                    bOrder.Brand EQ Syst.Var:gcBrand AND
                    bOrder.OrderId EQ INT(ActionLog.KeyValue) NO-ERROR.
         IF NOT AVAIL bOrder THEN DO:
            fLogMsg(ActionLog.KeyValue + "; ERROR not found Order").
            NEXT.
         END.

         IF AVAIL bOrder THEN DO:
            IF llLogRequest THEN fLogMsg(STRING(bOrder.OrderId) + "; Found OrderId " + 
                    ", Create StatusCode: " + ActionLog.ActionChar + 
                    ", ActionLog.ActionID: " + ActionLog.ActionID).
            IF ActionLog.ActionID EQ "dssent" THEN
               /* Send for signing */
               lcStatus = fSendSigningMessage(bOrder.OrderId, bOrder.Brand).
            ELSE IF ActionLog.ActionID EQ "dscancel" THEN
               /* Send cancel */
               lcStatus = fSendCancelMessage(bOrder.OrderId, bOrder.Brand).
            ELSE
               fLogMsg(STRING(bOrder.OrderId) + "; ERROR wrong Order status: " + STRING(bOrder.StatusCode)).

            IF lcStatus EQ "" THEN DO:
               FIND CURRENT ActionLog EXCLUSIVE-LOCK NO-ERROR.
               ASSIGN
                  ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS} /* ?? */
                  ActionLog.ActionTS     = ldCurrentTimeTS.
               RELEASE ActionLog.
            END. /* ELSE? Leave untouched and try again in next cron job */
         END.
      END. /* DO TRANS */
   END. /* FOR EACH */

END PROCEDURE.


/* MAIN START */

lcTableName = "Order".
ldCurrentTimeTS = Func.Common:mMakeTS().

IF(fIParam("SignatureApi", "LogRequest")) EQ 0 THEN
   ASSIGN
      llLogRequest  = FALSE.

ASSIGN
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

iTimeOut = 40.
initialize(lcUrlAdapter, iTimeOut).

RUN pCheckActionLog("dssent").
RUN pCheckActionLog("dscancel").

OUTPUT STREAM sLog CLOSE.

FINALLY:
   xmlrpc_finalize().
END FINALLY.

