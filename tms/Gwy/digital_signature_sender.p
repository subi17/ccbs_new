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
{Func/cparam2.i}
{fcgi_agent/xmlrpc/xmlrpc_client.i}

DEF VAR lcLogDir        AS CHAR NO-UNDO.
DEF VAR lcLog           AS CHAR NO-UNDO.
DEF VAR ldaFReadDate    AS DATE NO-UNDO.
DEF VAR lcActionID      AS CHAR NO-UNDO INIT "".
DEF VAR lcTableName     AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.

DEF VAR iTimeOut        AS INT  NO-UNDO.
DEF VAR lcHost          AS CHAR NO-UNDO.
DEF VAR liPort          AS INT  NO-UNDO.
DEF VAR lcUserIdDS      AS CHAR NO-UNDO.
DEF VAR lcpasswordDS    AS CHAR NO-UNDO.
DEF VAR lcUriPath       AS CHAR NO-UNDO.
DEF VAR lcUriPathCancel AS CHAR NO-UNDO.
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
   Function sends cancel request to
   Adapter (Signature API)
*/
FUNCTION fSendCancelMessage RETURNS CHAR
   (iiOrderId   AS INT
    /*icBrand AS CHAR*/):

   DEF VAR lcOrderStruct AS CHAR NO-UNDO. /* example...*/

   fLogMsg("Construct cancel message, Order: " + STRING(iiOrderId)).
   xmlrpc_cleanup().
   lcOrderStruct = add_struct(param_toplevel_id,"").

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
   Function sends signing request to
   Adapter (Signature API)
*/
FUNCTION fSendSigningMessage RETURNS CHAR
   (iiOrderId   AS INT
    /*icBrand AS CHAR*/):

   DEF VAR lcOrderStruct AS CHAR NO-UNDO. /* example...*/
   DEF VAR lcRespStruct  AS CHAR NO-UNDO. 
   DEF VAR lcResp        AS CHAR NO-UNDO. 
   DEF VAR lcResult      AS CHAR NO-UNDO. 
   DEF VAR lcDescription AS CHAR NO-UNDO.

   fLogMsg("Construct signing message, Order: " + STRING(iiOrderId)).
   xmlrpc_cleanup().
   lcOrderStruct = add_struct(param_toplevel_id,"").

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

   lcRespStruct = get_struct(response_toplevel_id, "0").

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
      fLogMsg("Message for Order " + STRING(iiOrderID) + " sent successfully!").
   END.
   ELSE DO:
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
            fLogMsg("Found OrderId: " + STRING(bOrder.OrderId) + 
                    ", OrderStatusCode: " + STRING(bOrder.StatusCode) + 
                    ", ActionLog.ActionID: " + STRING(ActionLog.ActionID)).
            IF bOrder.statusCode EQ {&ORDER_STATUS_DELIVERED} THEN
               /* Send for signing */
               lcStatus = fSendSigningMessage(bOrder.OrderId).
            ELSE IF LOOKUP(bOrder.StatusCode, {&ORDER_CLOSE_STATUSES}) > 0 THEN /* 7,8,9 */
               /* Send cancel */
               lcStatus = fSendCancelMessage(bOrder.OrderId).
            ELSE
               fLogMsg("ERROR wrong Order status, OrderId: " + STRING(bOrder.OrderId) + 
                       ", status: " + STRING(bOrder.StatusCode)).

            IF lcStatus EQ "" THEN DO:
               ASSIGN
                  ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS} /* ?? */
                  ActionLog.UserCode     = Syst.Var:katun /* aseta orderfunc.i ?? */
                  ActionLog.ActionTS     = ldCurrentTimeTS.
               RELEASE ActionLog.
            END. /* ELSE? Leave untouched and try again next cron job?? */
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
    /* lcHost        = fCParam("SignatureApi", "Host")
    liPort        = fIParam("SignatureApi", "Port")
    lcUriPath     = "/masmovil-test-staging-environment/api/v1/digitalSignature/yoigo/registerSignProcess" /* "/digitalSignature/1/registerSignProcess"*/
    /* lcUriPath       = fCParam("SignatureApi", "UriPath")*/
    lcUriPathCancel = fCParam("SignatureApi", "UriPathCancel")
    liLogRequest  = fIParam("SignatureApi", "LogRequest")*/
    llLogRequest  = LOGICAL(liLogRequest)
    /* lcLogdir        = fCParam("SignatureApi", "LogDir")*/
    lcLogDir   = "/scratch/log/digitalsignature/".
    /* lcUrlAdapter  = fCParam("SignatureApi", "UrlAdapter")*/
    /* e.g. http://217.168.2.239:7001/com-yoigo-roi-webapp/xmlrpc */

ldaFReadDate = TODAY.
lcLog = lcLogDir + 
        "digital_signature_request" +
        STRING(YEAR(ldaFReadDate)) +
        STRING(MONTH(ldaFReadDate),"99") +
        STRING(DAY(ldaFReadDate),"99") + ".log".
OUTPUT STREAM sLog TO VALUE(lcLog) APPEND.

fLogMsg("Started by Cron at " + Func.Common:mTS2HMS(ldCurrentTimeTS)).

IF lcUriPath = ? OR lcUriPath = "" OR 
   lcUriPathCancel = ? OR lcUriPathCancel = "" THEN DO:
   fLogMsg("ERROR Digital Signature URI not defined, check cparam. QUIT."). 
   QUIT.
END.
IF lcUrlAdapter = ? OR lcUrlAdapter = "" THEN DO:
   fLogMsg("ERROR Digital Signature URL not defined, check cparam. QUIT."). 
   QUIT.
END.

iTimeOut = 10.
initialize(lcUrlAdapter, iTimeOut).

RUN pCheckActionLog.

xmlrpc_finalize().
OUTPUT STREAM sLog CLOSE.

