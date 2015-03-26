/* ----------------------------------------------------------------------
  MODULE .......: mnp_common.i
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 01.09.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

&IF "{&MNP_COMMON_I}" NE "YES"
&THEN

&GLOBAL-DEFINE MNP_COMMON_I YES

{xmlrpc/xmlrpc_access.i}
{commpaa.i}
katun = "MNP-ADAPTER".
gcBrand = "1".
{date.i}
{mnp.i}
{xmlfunction.i}
{msreqfunc.i}
{tmsconst.i}
{log.i}
DEFINE SHARED VARIABLE lcrequest AS LONGCHAR NO-UNDO.

DEF VAR pcStruct AS CHAR NO-UNDO.
DEF VAR lcFields AS CHARACTER NO-UNDO. 
DEF VAR pcSolicitud AS CHARACTER NO-UNDO. 
DEF VAR pcSolicituds AS CHARACTER NO-UNDO. 
DEF VAR pcNotificacions AS CHARACTER NO-UNDO. 
DEF VAR pcNotificacion AS CHARACTER NO-UNDO. 
DEF VAR lcSolicitudFields AS CHARACTER NO-UNDO. 
DEF VAR liStatus AS INTEGER NO-UNDO. 
DEF VAR lcRespArray AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRespStruct AS CHARACTER NO-UNDO. 

DEFINE VARIABLE liMsisdnCounter AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcMSISDNArray AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcICCIDArray AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcMSISDNStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcICCStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcMsisdnBegin AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcMsisdnENd AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsisdn AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMsisdnEnd AS INTEGER NO-UNDO. 
DEFINE VARIABLE lii AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 

/* structs */
DEFINE VARIABLE pcStatusCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liLang AS INTEGER NO-UNDO. 

DEFINE VARIABLE liOrderQty AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCounter AS INTEGER NO-UNDO. 

DEFINE VARIABLE llBuzonCreated AS LOGICAL NO-UNDO INIT FALSE.
DEF BUFFER MNPBuzon FOR MNPOperation.

DEFINE TEMP-TABLE ttInput NO-UNDO
   FIELD cli AS CHAR
   FIELD icc AS CHAR
   FIELD cancelProposalRef AS CHAR
   FIELD cancelProposalFile AS CHAR
   FIELD FileLocation AS CHAR
   FIELD nodoCentralTS AS DECIMAL
   FIELD createdTS AS DECIMAL
   FIELD statusTS AS DECIMAL
   FIELD portingTime AS DECIMAL
   FIELD portRequest AS CHAR
   FIELD statusCode AS CHAR
   FIELD statusReason AS CHAR
   FIELD FormRequest AS CHAR
   FIELD contentType AS CHAR
   FIELD NotificationCode AS CHAR
   FIELD CustIdType AS CHAR
   FIELD CustId AS CHAR
   FIELD Company AS CHAR
   FIELD firstName AS CHAR
   FIELD lastname AS CHAR
   FIELD lastname2 AS CHAR
   FIELD nationality AS CHAR
   FIELD receptorCode AS CHAR
   FIELD receptorNRN AS CHAR
   FIELD portingTimeFromCustomer AS LOGICAL
   FIELD DonorCode AS CHAR
   FIELD RequestedTS AS DECIMAL
   FIELD StatusLimitTS AS DECIMAL
   FIELD DonorExtraOrdinary AS LOGICAL
INDEX i IS PRIMARY portRequest. 

DEFINE TEMP-TABLE ttMultipleMSISDN NO-UNDO
   FIELD portRequest AS CHAR
   FIELD cli AS CHAR
   FIELD icc AS CHAR
INDEX i IS PRIMARY portrequest cli. 
   
FUNCTION fCreateBuzonMessage RETURNS LOGICAL
(icMessageType AS CHARACTER):
   
   IF llBuzonCreated THEN RETURN FALSE.
   
   CREATE MNPBuzon.
   ASSIGN
      MNPBuzon.MNPOperationID = NEXT-VALUE(MNPOperSeq)
      MNPBuzon.CreatedTS = {&nowts}
      MNPBuzon.MessageType = icMessageType
      MNPBuzon.Sender = 3.
   
   llBuzonCreated = TRUE.

   IF icMessageType EQ "obtenerSolicitudesCancelacionPortabilidadMovilIniciadasPorDonantePendientesLectura" THEN DO:
   
      DEFINE VARIABLE liBase64IndexEnd AS INTEGER NO-UNDO. 
      DEFINE VARIABLE liBase64Index AS INTEGER NO-UNDO. 
      DEFINE VARIABLE lcBuzonRequest AS LONGCHAR NO-UNDO. 
      COPY-LOB lcRequest to lcBuzonRequest.

      liBase64IndexEnd = 1.
      liBase64Index = 1.

      /* do not store base64-encoded binary data */
      DO WHILE TRUE:
         liBase64Index = index(lcBuzonRequest,"<base64>",liBase64Index + 1).
         if liBase64Index > 0 then do:
            liBase64IndexEnd = index(lcBuzonRequest,"</base64>",liBase64Index + 1).
            IF liBase64IndexEnd > 0 THEN
               lcBuzonRequest = SUBSTRING(lcBuzonRequest, 1, liBase64Index + 7) +
                  "#DATA" + SUBSTRING(lcBuzonRequest,liBase64IndexEnd).
         end.
         else leave.
      END.
   
      copy-lob from lcBuzonRequest to MNPBuzon.XMLRequest.
   END.
   ELSE COPY-LOB from lcRequest to MNPBuzon.XMLRequest.

   RETURN TRUE.

END FUNCTION. 

FUNCTION fCreateMNPObtenerMessage RETURNS LOGICAL
(icMessageType AS CHARACTER):
  
   IF NOT llBuzonCreated THEN
      fCreateBuzonMessage(icMessageType).

   CREATE MNPOperation.
   ASSIGN MNPOperation.MNPOperationID = NEXT-VALUE(MNPOperSeq)
          MNPOperation.CreatedTS = {&nowts} /*ttInput.statusTS */
          MNPOperation.Sender = 2
          MNPOperation.SentTS = {&nowts} /* tms receive time */
          MNPOperation.MessageType = icMessageType
          MNPOperation.XMLSeq = MNPBuzon.MNPOperationID
          MNPOperation.StatusCode = {&MNP_MSG_HANDLING}
          MNPOperation.ErrorHandled = {&MNP_ERRORHANDLED_NO}
          MNPOperation.ErrorCode = {&MNP_ERRORCODE_HANDLE}. /* set error by def. */

   RETURN TRUE.

END FUNCTION. 

FUNCTION fErrorHandle RETURNS LOGICAL
(icDesc AS CHAR):

   ASSIGN
      MNPOperation.StatusCode = {&MNP_MSG_HANDLING}
      MNPOperation.ErrorCode  = {&MNP_ERRORCODE_HANDLE}
      MNPOperation.ErrorHandled = {&MNP_ERRORHANDLED_NO}
      MNPOperation.ErrorDesc = icDesc.

END FUNCTION. 

FUNCTION fErrorParse RETURNS LOGICAL
(icDesc AS CHAR):

   ASSIGN
      MNPOperation.StatusCode = {&MNP_MSG_PARSING}
      MNPOperation.ErrorCode = {&MNP_ERRORCODE_PARSE}
      MNPOperation.ErrorHandled = {&MNP_ERRORHANDLED_NO}
      MNPOperation.ErrorDesc = icDesc.

END FUNCTION. 

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

&ENDIF
