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
   Procedure checks from ActionLog if request must be
   sent to Adapter (Signature API).
*/
PROCEDURE pSendRequest:

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
               .
            ELSE IF LOOKUP(bOrder.StatusCode, {&ORDER_CLOSE_STATUSES}) > 0 THEN /* 7,8,9 */
               /* send cancel */
               .
            ELSE
               fLogMsg("ERROR wrong Order status, OrderId: " + STRING(bOrder.OrderId) + 
                       ", status: " + STRING(bOrder.StatusCode)).

           ASSIGN
              ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS} /* ?? */
              ActionLog.UserCode     = Syst.Var:katun /* aseta orderfunc.i ?? */
              ActionLog.ActionTS     = ldCurrentTimeTS.
           RELEASE ActionLog.
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

ASSIGN
   /* lcLogDir   = fCParam("SignatureApi","LogDir").*/
   lcLogDir   = "/scratch/log/digitalsignature/".

lcTableName = "Order".
ldCurrentTimeTS = Func.Common:mMakeTS().

ldaFReadDate = TODAY.
lcLog = lcLogDir + 
        "digital_signature_request" +
        STRING(YEAR(ldaFReadDate)) +
        STRING(MONTH(ldaFReadDate),"99") +
        STRING(DAY(ldaFReadDate),"99") + ".log".
OUTPUT STREAM sLog TO VALUE(lcLog) APPEND.

fLogMsg("Started by Cron at " + STRING(ldCurrentTimeTS)).
RUN pSendRequest.
OUTPUT STREAM sLog CLOSE.

