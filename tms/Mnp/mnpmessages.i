/* ----------------------------------------------------------------------
  MODULE .......: mnpmessages.i
  TASK .........: MNP XML message creation functions
  APPLICATION ..: TMS
  AUTHOR .......: as
  CREATED ......: 28.8.2009 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&IF "{&MNPMESSAGES_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE MNPMESSAGES_I YES

{fcgi_agent/xmlrpc/xmlrpc.i &SERIALIZE_ONLY=1}
{fcgi_agent/xmlrpc/xmlrpc_access.i &TOGETHER=1 &UTF8CONVERT=1}

DEFINE TEMP-TABLE ttPortabilityQuery NO-UNDO
   FIELD ResultsPerPage AS INT INIT 10
   FIELD MsgCreatedStart AS DEC FORMAT "99999999.99999"
   FIELD MsgCreatedEnd AS DEC  FORMAT "99999999.99999"
   FIELD PortReq AS CHAR format "x(24)"
   FIELD MNPCreatedStart AS DEC FORMAT "99999999.99999"
   FIELD MNPCreatedEnd AS DEC FORMAT "99999999.99999"
   FIELD DonorOperatorCode AS CHAR
   FIELD ReceptorOperatorCode AS CHAR
   FIELD PortabilityStatus AS CHAR
   FIELD OrgId AS CHAR format "x(12)"
   FIELD CustIdType AS CHAR
   FIELD FirstName AS CHAR format "x(20)" 
   FIELD Surname1 AS CHAR format "x(20)"
   FIELD Surname2 AS CHAR format "x(20)"
   FIELD Company AS CHAR
   FIELD Nationality AS CHAR
   FIELD NRNReceptor AS CHAR
   FIELD ChangeWindowDateStart AS DEC FORMAT "99999999.99999"
   FIELD ChangeWindowDateEnd AS DEC FORMAT "99999999.99999"
   FIELD MsisdnRangeStart AS CHAR FORMAT "x(12)"
   FIELD MsisdnRangeEnd AS CHAR FORMAT "x(12)"
   FIELD FormRequest AS CHAR format "x(12)"
   FIELD IsSinglePorting AS LOGICAL INIT ?
   FIELD IsOperatorOnline AS LOGICAL INIT ?.

DEFINE TEMP-TABLE ttNumberTermQuery NO-UNDO
   FIELD ResultsPerPage AS INT INIT 10
   FIELD MsgCreatedStart AS DEC FORMAT "99999999.99999"
   FIELD MsgCreatedEnd AS DEC  FORMAT "99999999.99999"
   FIELD PortReq AS CHAR format "x(24)"
   FIELD MNPCreatedStart AS DEC FORMAT "99999999.99999"
   FIELD MNPCreatedEnd AS DEC FORMAT "99999999.99999"
   FIELD PortabilityStatus AS CHAR
   FIELD MsisdnRangeStart AS CHAR FORMAT "x(12)"
   FIELD MsisdnRangeEnd AS CHAR FORMAT "x(12)"
   FIELD DonorOperatorCode AS CHAR
   FIELD ReceptorOperatorCode AS CHAR.

DEFINE TEMP-TABLE ttPageQuery NO-UNDO
   FIELD Operation AS CHAR format "x(50)"
   FIELD PageCode AS CHAR  format "x(50)"
   FIELD PageNumber AS INT init 1 
   FIELD Cases AS INT.

DEFINE TEMP-TABLE ttMigrationRequest NO-UNDO
   FIELD StartDate AS DATE FORMAT "99-99-9999" INIT TODAY
   FIELD NumbersTotal AS INTEGER INIT 1
   FIELD ServiceOperatorsFrom AS CHAR FORMAT "x(40)"
   FIELD NetworkOperatorsFrom AS CHAR FORMAT "x(40)"
   FIELD OperatorTo AS CHAR INIT "005"
   FIELD Scenario AS CHAR FORMAT "x(40)".

DEFINE TEMP-TABLE ttMigrationNumberRequest NO-UNDO
   FIELD PortReq AS CHAR FORMAT "x(23)"
   FIELD ServiceOperatorFrom AS CHAR FORMAT "x(3)"
   FIELD NetworkOperatorFrom AS CHAR FORMAT "x(3)"
   FIELD ServiceOperatorTo AS CHAR FORMAT "x(3)"
   FIELD NetworkOperatorTo AS CHAR FORMAT "x(3)"
   FIELD NRN AS CHAR FORMAT "X(6)" INIT "005"
   FIELD MSISDN AS CHAR FORMAT "x(9)".

DEFINE TEMP-TABLE ttNumberRangesQuery NO-UNDO
   FIELD ResultsPerPage AS INT INIT 10
   FIELD MsgCreatedStart AS DEC FORMAT "99999999.99999"
   FIELD MsgCreatedEnd AS DEC FORMAT "99999999.99999"
   FIELD Msisdn AS CHAR FORMAT "x(12)"
   FIELD OperatorCodePrimary AS CHAR FORMAT "x(3)"
   FIELD OperatorCodeSecondary AS CHAR FORMAT "x(3)".

FUNCTION fMNPOperation RETURNS LOGICAL
  (INPUT piMNPSeq   AS INTEGER,
   INPUT pcXML      AS CHARACTER,
   INPUT lcType     AS CHARACTER):

   DEFINE VARIABLE lcLongXML AS LONGCHAR NO-UNDO. 
   lcLongXML = pcXML.
      
   DEF BUFFER MNPOperation FOR MNPOperation.

   DO TRANS:
      CREATE MNPOperation.
      MNPOperation.MNPOperationID = NEXT-VALUE(MNPOperSeq).
      
      ASSIGN
         MNPOperation.CreatedTS   = Func.Common:mMakeTS()
         MNPOperation.MNPSeq      = piMNPSeq
         MNPOperation.Sender      = 1 /* TMS */
         MNPOperation.StatusCode  = 1 /* Waiting send */
         MNPOperation.MessageType = lcType.
            
      COPY-LOB lcLongXML TO MNPOperation.XMLRequest.
   END.
   
   RETURN TRUE.      

END.

/* TODO requires MNPProcess buffer */
FUNCTION fMnpXMLSerialize RETURNS LOGICA
   (INPUT icMethod AS CHARACTER,
   INPUT icXML AS CHARACTER):

   xmlrpc_initialize(FALSE).
   icXML = serialize_rpc_call("mnp." + icMethod).
   xmlrpc_finalize(). 
   RETURN fMNPOperation(MNPProcess.MNPSeq, icXML, icMethod).

END FUNCTION. 

/* 1.10 Confirm portability activation request query */
FUNCTION fSendConfirmation RETURNS LOGICAL
  (INPUT pcPortReq AS CHARACTER):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 
   DEF VAR lcArray AS CHAR NO-UNDO. 
   DEF BUFFER MNPProcess FOR MNPProcess.
   DEF BUFFER MNPSub FOR MNPSub.

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoReferencia", pcPortReq).

   FIND MNPProcess NO-LOCK WHERE
        MNPProcess.PortRequest = pcPortReq NO-ERROR.
   IF AVAIL MNPProcess THEN DO:
      FIND MNPSub NO-LOCK WHERE
           MNPSub.MNPSeq = MNPProcess.MNPSeq NO-ERROR.
      IF AMBIGUOUS MNPSub THEN DO:
         lcArray = add_array(lcReqStruct, "confirmedMSISDN").
         FOR EACH MNPSub NO-LOCK WHERE
                  MNPSub.MNPSeq = MNPProcess.Mnpseq:
            IF MNPSub.StatusReason EQ "" OR
               MNPSub.StatusReason EQ "CONFIRM" THEN
               add_string(lcArray,"",MNPSub.CLI).
         END.
      END.
   END. 

   RETURN fMnpXMLSerialize("confirmarSolicitudAltaPortabilidadMovil", lcReqStruct).
   
END.

/* 1.11 Reject portability activation request */
FUNCTION fSendRejection RETURNS LOGICAL
  (INPUT pcPortReq AS CHARACTER,
   INPUT pcRejectReason AS CHARACTER,
   OUTPUT ocError AS CHAR):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 
   
   IF LOOKUP(pcRejectReason,"RECH_IDENT,RECH_ICCID,RECH_BNUME,RECH_FMAYO,RECH_PERDI") = 0 THEN DO:
      ocError = "Unknown rejection reason: " + pcRejectReason.
      RETURN FALSE.
   END.

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoReferencia", pcPortReq).
   add_string(lcReqStruct, "causaEstado", pcRejectReason).
   
   RETURN fMnpXMLSerialize("rechazarSolicitudAltaPortabilidadMovil", lcReqStruct).

END.

/* 1.12 - Cancel Portability activation request */
FUNCTION fSendCancellation RETURNS LOGICAL
  (INPUT pcPortReq AS CHARACTER,
   INPUT pcCancelReason AS CHARACTER,
   INPUT plInitiatedByDonor AS LOGICAL,
   OUTPUT ocError AS CHAR):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   IF LOOKUP(pcCancelReason,"CANC_ABONA,CANC_TECNI,CANC_ERROR") = 0 THEN DO:
      ocError = "Unknown cancellation reason: " + pcCancelReason.
      RETURN FALSE.
   END.

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoReferencia", pcPortReq).
   add_string(lcReqStruct, "causaEstado", pcCancelReason).
   add_boolean(lcReqStruct, "cancelacionIniciadaPorDonante",plInitiatedByDonor).
   
   RETURN fMnpXMLSerialize("cancelarSolicitudAltaPortabilidadMovil", lcReqStruct).

END.

/* 1.15 - Mobile portability activation requests query  */
/* TODO: input validating */
/* all search parametres are optional */

FUNCTION fSendPortabilityActivationRequestsQuery RETURNS LOGICAL
  (INPUT TABLE ttPortabilityQuery):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMsgCreationRange AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMnpCreationRange AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcPortingRange AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcModifiedRange AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMsisdnRange AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcAbonado AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDocumentoIdentification AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDatosPersonales AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
  
   IF ttPortabilityQuery.ResultsPerPage > 0 THEN 
      add_string(lcReqStruct, "registrosPorPagina", string(ttPortabilityQuery.ResultsPerPage)). /* system level set */
   
   IF ttPortabilityQuery.MsgCreatedStart > 0 OR ttPortabilityQuery.MsgCreatedEnd > 0 THEN DO:
      lcMsgCreationRange = add_struct(lcReqStruct, "rangoFechasCreacion").
      add_timestamp(lcMsgCreationRange, "valorInicial",ttPortabilityQuery.MsgCreatedStart).
      add_timestamp(lcMsgCreationRange, "valorFinal",ttPortabilityQuery.MsgCreatedEnd).
   END.
   
   IF ttPortabilityQuery.MNPCreatedEnd > 0 OR ttPortabilityQuery.MNPCreatedStart > 0 THEN DO:
      lcMNPCreationRange = add_struct(lcReqStruct, "rangoFechasSolicitudPorAbonado").
      add_timestamp(lcMnpCreationRange, "valorInicial",ttPortabilityQuery.MNPCreatedStart).
      add_timestamp(lcMnpCreationRange, "valorFinal",ttPortabilityQuery.MNPCreatedEnd).
   END.
   
   IF ttPortabilityQuery.PortReq NE "" THEN
      add_string(lcReqStruct, "codigoReferencia", ttPortabilityQuery.PortReq).
   
   IF ttPortabilityQuery.MsisdnRangeStart NE "" OR ttPortabilityQuery.MsisdnRangeEnd NE "" THEN DO:
      lcMsisdnRange = add_struct(lcReqStruct, "rangoMsisdn").
      add_string(lcMsisdnRange, "valorInicial",ttPortabilityQuery.MsisdnRangeStart).
      add_string(lcMsisdnRange, "valorFinal",ttPortabilityQuery.MsisdnRangeEnd).
   END.
 
   IF ttPortabilityQuery.DonorOperatorCode NE "" THEN
      add_string(lcReqStruct, "codigoOperadorDonante", ttPortabilityQuery.DonorOperatorCode).

   IF ttPortabilityQuery.ReceptorOperatorCode NE "" THEN
      add_string(lcReqStruct, "codigoOperadorReceptor", ttPortabilityQuery.ReceptorOperatorCode).
   
   IF ttPortabilityQuery.PortabilityStatus NE "" THEN
      add_string(lcReqStruct, "estado", ttPortabilityQuery.PortabilityStatus).
   
   IF ttPortabilityQuery.NRNReceptor NE "" THEN
      add_string(lcReqStruct, "NRNReceptor", ttPortabilityQuery.NRNReceptor).

   IF ttPortabilityQuery.ChangeWindowDateStart > 0 OR ttPortabilityQuery.ChangeWindowDateEnd > 0 THEN DO:
      lcPortingRange = add_struct(lcReqStruct, "rangoFechasVentanaCambio").
      add_timestamp(lcPortingRange,"valorInicial",ttPortabilityQuery.ChangeWindowDateStart).
      add_timestamp(lcPortingRange, "valorFinal",ttPortabilityQuery.ChangeWindowDateEnd).
   END.

   IF ttPortabilityQuery.FormRequest NE "" THEN
      add_string(lcReqStruct, "codigoContrato", ttPortabilityQuery.FormRequest).
   
   IF ttPortabilityQuery.OrgId NE "" THEN DO:
      lcAbonado = add_struct(lcReqStruct,"abonado").
      lcDocumentoIdentification = add_struct(lcAbonado, "documentoIdentificacion").
      add_string(lcDocumentoIdentification, "tipo", 
         (IF ttPortabilityQuery.CustIdType = "PassPort" THEN "PAS" ELSE ttPortabilityQuery.CustIdType)).
      
      add_string(lcDocumentoIdentification, "documento", ttPortabilityQuery.OrgId).

      lcDatosPersonales = add_struct(lcAbonado, "datosPersonales").

      IF ttPortabilityQuery.CustIdType = "CIF" THEN 
         add_string(lcDatosPersonales, "razonSocial", ttPortabilityQuery.Company).
      ELSE DO:
         IF ttPortabilityQuery.Nationality NE "NIF" THEN
            add_string(lcDatosPersonales, "nacionalidad", ttPortabilityQuery.Nationality).
         add_string(lcDatosPersonales, "nombre", ttPortabilityQuery.FirstName).
         add_string(lcDatosPersonales, "primerApellido", ttPortabilityQuery.Surname1).
         IF ttPortabilityQuery.SurName2 NE "" THEN 
         add_string(lcDatosPersonales, "segundoApellido", ttPortabilityQuery.Surname2).
      END.
   END.   

   IF ttPortabilityQuery.IsSinglePorting NE ? THEN
      add_string(lcReqStruct, "tipoSolicitudAltaPortabilidadMovile",
         STRING(ttPortabilityQuery.IsSinglePorting,"INDIVIDUAL/MULTIPLE")).
   
   IF ttPortabilityQuery.IsOperatorOnline NE ? THEN
   add_string(lcReqStruct, "estadoOperadorDonante",
      TRIM(STRING(ttPortabilityQuery.IsOperatorOnline,"ALTA_ORDINARIA/ALTA_EXTRAORDINARIA"))).
   
   RETURN fMnpXMLSerialize("consultarSolicitudesAltaPortabilidadMovil", lcReqStruct).

END.

/* 2.3 - Mobile numbering migration request */
FUNCTION fSendMigrationRequest RETURNS LOGICAL
  (INPUT TABLE ttMigrationRequest):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   DEFINE VARIABLE i AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcServiceOperatorArray AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcNetworkOperatorArray AS CHARACTER NO-UNDO. 

   IF LOOKUP(ttMigrationRequest.Scenario,"CAMBIO_OPERADOR_RED,CONVERSION_OPERADOR_COMPLETO,FUSION_MISMO_OPERADOR_RED,ABSORCION_MISMO_OPERADOR_RED,FUSION_DISTINTOS_OPERADORES_RED") = 0 THEN RETURN False.
   
   lcReqStruct = add_struct(param_toplevel_id, "").
  
   add_date_or_time(lcReqStruct, "fechaInicio", ttMigrationRequest.StartDate, 50400).
   add_string(lcReqStruct, "volumenNumeracion", STRING(ttMigrationRequest.NumbersTotal)).
   
   lcServiceOperatorArray = add_array(lcReqStruct,"codigoOperadorPrestadorServicioOrigen").
   DO i = 1 TO NUM-ENTRIES(ttMigrationRequest.ServiceOperatorsFrom):
      add_string(lcServiceOperatorArray, "", ENTRY(i, ttMigrationRequest.ServiceOperatorsFrom)).
   END.  

   lcNetworkOperatorArray = add_array(lcReqStruct,"codigoOperadorRedOrigen").
   DO i = 1 TO NUM-ENTRIES(ttMigrationRequest.ServiceOperatorsFrom):
      add_string(lcNetworkOperatorArray, "", ENTRY(i, ttMigrationRequest.NetworkOperatorsFrom)).
   END.  
   
   add_string(lcReqStruct, "codigoOperadorRedDestino", ttMigrationRequest.OperatorTo).
   add_string(lcReqStruct, "escenario", ttMigrationRequest.Scenario).
   
   RETURN fMnpXMLSerialize("crearSolicitudMigracionNumeracionMovil", lcReqStruct).
   
END.

/* 2.4 - Finalize Mobile numbering migration request */
FUNCTION fSendMigrationFinalizeRequest RETURNS LOGICAL
  (INPUT pcPortReq AS CHAR):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
  
   add_string(lcReqStruct, "codigoReferencia", pcPortReq).
   
   RETURN fMnpXMLSerialize(
      "finalizarSolicitudMigracionNumeracionMovil",
      lcReqStruct).
   
END.

/* 2.5 - Numbering query for Mobile numbering migration request */
FUNCTION fSendMigrationNumberRequest RETURNS LOGICAL
  (INPUT TABLE ttMigrationNumberRequest):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
  
   add_string(lcReqStruct, "codigoReferenciaSolicitudMigracionNumeracionMovil", ttMigrationNumberRequest.PortReq).
   add_string(lcReqStruct, "codigoOperadorRedOrigen", ttMigrationNumberRequest.NetworkOperatorFrom).
   add_string(lcReqStruct, "codigoOperadorRedDestino", ttMigrationNumberRequest.NetworkOperatorto).
   add_string(lcReqStruct, "codigoOperadorPrestadorServicioOrigen", ttMigrationNumberRequest.ServiceOperatorFrom).

   IF ttMigrationNumberRequest.ServiceOperatorTo NE "" THEN
      add_string(lcReqStruct, "codigoOperadorPrestadorServicioDestino", ttMigrationNumberRequest.ServiceOperatorTo).
   
   add_string(lcReqStruct, "NRN", ttMigrationNumberRequest.NRN).
   add_string(lcReqStruct, "MSISDN", ttMigrationNumberRequest.MSISDN).
   
   RETURN fMnpXMLSerialize(
      "crearSolicitudNumeracionMigracionNumeracionMovil",
      lcReqStruct).
   
END.

/* 2.6 - Numbering Termination request */
FUNCTION fSendNumberingTerminationRequest RETURNS LOGICAL
   (INPUT pcMSISDN  AS CHARACTER):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 
   lcReqStruct = add_struct(param_toplevel_id, "").
   
   add_timestamp(lcReqStruct, "fechaBajaAbonado", MNPProcess.CreatedTS).
   add_string(lcReqStruct, "MSISDN", pcMSISDN).
   
   RETURN fMnpXMLSerialize(
      "crearSolicitudBajaNumeracionMovil",
      lcReqStruct).
END.

/* 2.7 - Numbering Termination request query */
FUNCTION fSendNumberingTerminationRequestsQuery RETURNS LOGICAL
  (INPUT TABLE ttNumberTermQuery):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMsgCreationRange AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMnpCreationRange AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMsisdnRange AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
  
   IF ttNumberTermQuery.ResultsPerPage > 0 THEN
      add_string(lcReqStruct, "registrosPorPagina", string(ttNumberTermQuery.ResultsPerPage)).
   
   IF ttNumberTermQuery.MsgCreatedStart > 0 OR ttNumberTermQuery.MsgCreatedEnd > 0 THEN DO:
      lcMsgCreationRange = add_struct(lcReqStruct, "rangoFechasCreacion").
      add_timestamp(lcMsgCreationRange, "valorInicial",ttNumberTermQuery.MsgCreatedStart).
      add_timestamp(lcMsgCreationRange, "valorFinal",ttNumberTermQuery.MsgCreatedEnd).
   END.
   
   IF ttNumberTermQuery.PortReq NE "" THEN
      add_string(lcReqStruct, "codigoReferencia", ttNumberTermQuery.PortReq).
   
   IF ttNumberTermQuery.MNPCreatedEnd > 0 OR ttNumberTermQuery.MNPCreatedStart > 0 THEN DO:
      lcMNPCreationRange = add_struct(lcReqStruct, "rangoFechasBajaAbonado").
      add_timestamp(lcMnpCreationRange, "valorInicial",ttNumberTermQuery.MNPCreatedStart).
      add_timestamp(lcMnpCreationRange, "valorFinal",ttNumberTermQuery.MNPCreatedEnd).
   END.
   
   IF ttNumberTermQuery.PortabilityStatus NE "" THEN
      add_string(lcReqStruct, "estado", ttNumberTermQuery.PortabilityStatus).
   
   IF ttNumberTermQuery.MsisdnRangeStart NE "" OR ttNumberTermQuery.MsisdnRangeEnd NE "" THEN DO:
      lcMsisdnRange = add_struct(lcReqStruct, "rangoMsisdn").
      add_string(lcMsisdnRange, "valorInicial",ttNumberTermQuery.MsisdnRangeStart).
      add_string(lcMsisdnRange, "valorFinal",ttNumberTermQuery.MsisdnRangeEnd).
   END.
 
   IF ttNumberTermQuery.DonorOperatorCode NE "" THEN
      add_string(lcReqStruct, "codigoOperadorDonante", ttNumberTermQuery.DonorOperatorCode).

   IF ttNumberTermQuery.ReceptorOperatorCode NE "" THEN
      add_string(lcReqStruct, "codigoOperadorReceptor", ttNumberTermQuery.ReceptorOperatorCode).

   RETURN fMnpXMLSerialize("consultarSolicitudesBajaNumeracionMovil", lcReqStruct).
   
END.

/* 2.8 - Numbering Termination request detail */
FUNCTION fSendNumberTerminationDetail RETURNS LOGICAL
   (INPUT pcPortReq AS CHARACTER):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoReferencia", pcPortReq).

   RETURN fMnpXMLSerialize("obtenerSolicitudBajaNumeracionMovil", lcReqStruct).

END.

/* 2.9 - Cancel Numbering Termination request */
FUNCTION fSendNumberTerminationCancel RETURNS LOGICAL
   (INPUT pcPortReq AS CHARACTER):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoReferencia", pcPortReq).
   add_string(lcReqStruct, "causaEstado", "CANC_ABONA").

   RETURN fMnpXMLSerialize("cancelarSolicitudBajaNumeracionMovil", lcReqStruct).

END.

/* 2.10 - Portabilities requests in donor role pending to be confirmed/rejected */
FUNCTION fSendRequestedQuery RETURNS LOGICAL
   (input piPages AS int):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoOperadorObjeto", "005").

   RETURN fMnpXMLSerialize("obtenerSolicitudesAltaPortabilidadMovilComoDonantePendientesConfirmarRechazar", lcReqStruct).

END.

/* 2.11 - Check Numbering ranges */
FUNCTION fSendNumberRangesQuery RETURNS LOGICAL
  (INPUT TABLE ttNumberRangesQuery):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMsgCreationRange AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
  
   IF ttNumberRangesQuery.ResultsPerPage > 0 THEN
      add_string(lcReqStruct, "registrosPorPagina", string(ttNumberRangesQuery.ResultsPerPage)). /* system level set ? */
   
   IF ttNumberRangesQuery.MsgCreatedStart > 0 OR ttNumberRangesQuery.MsgCreatedEnd > 0 THEN DO:
      lcMsgCreationRange = add_struct(lcReqStruct, "rangoFechasCreacion").
      add_timestamp(lcMsgCreationRange, "valorInicial",ttNumberRangesQuery.MsgCreatedStart).
      add_timestamp(lcMsgCreationRange, "valorFinal",ttNumberRangesQuery.MsgCreatedEnd).
   END.
   
   IF ttNumberRangesQuery.Msisdn NE "" THEN
      add_string(lcReqStruct, "MSISDN",ttNumberRangesQuery.MSISDN).
 
   IF ttNumberRangesQuery.OperatorCodePrimary NE "" THEN
      add_string(lcReqStruct, "codigoOperadorPropietario", ttNumberRangesQuery.OperatorCodePrimary).

   IF ttNumberRangesQuery.OperatorCodeSecondary NE "" THEN
      add_string(lcReqStruct, "codigoOperadorSubasignatario", ttNumberRangesQuery.OperatorCodeSecondary).
   
   RETURN fMnpXMLSerialize("consultarRangosNumeracion", lcReqStruct).

END.

/* 2.12- Mobile portability activation requests detail */
FUNCTION fSendPortabilityDetailQuery RETURNS LOGICAL
  (INPUT pcPortReq AS CHARACTER):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoReferencia", pcPortReq).

   RETURN fMnpXMLSerialize("obtenerSolicitudAltaPortabilidadMovil", lcReqStruct).

END.

/* 2.13- Cancellation detail by donor */
FUNCTION fSendCancellationDetailQuery RETURNS LOGICAL
  (INPUT pcCancellationRef AS CHARACTER):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoSolicitudCancelacionPortabilidadMovilIniciadaPorDonante", pcCancellationRef).

   RETURN fMnpXMLSerialize("obtenerSolicitudCancelacionPortabilidadMovilIniciadaPorDonante", lcReqStruct).

END.

/* 2.14 - Mobile numbering migration request detail */
FUNCTION fSendMigrationDetailRequest RETURNS LOGICAL
  (INPUT pcPortReq AS CHARACTER):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoReferencia", pcPortReq).

   RETURN fMnpXMLSerialize("obtenerSolicitudMigracionNumeracionMovil", lcReqStruct).
   
END.

/* 2.15 - Numbering query for Mobile numbering migration request detail */
FUNCTION fSendMigrationNumberDetailRequest RETURNS LOGICAL
  (INPUT pcPortReq AS CHARACTER):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoReferencia", pcPortReq).

   RETURN fMnpXMLSerialize("obtenerSolicitudNumeracionMigracionNumeracionMovil", lcReqStruct).
   
END.

FUNCTION fSendPaginaQuery RETURNS LOGICAL
  (INPUT TABLE ttPageQuery):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoPeticionPaginada", ttPageQuery.PageCode).
   add_string(lcReqStruct, "registrosPorPagina", string(ttPageQuery.Cases)). 
   add_string(lcReqStruct, "indicePagina", string(ttPageQuery.PageNumber)). 

   RETURN fMnpXMLSerialize("obtenerPaginaResultados" + 
      CAPS(SUBSTRING(ttPageQuery.Operation,1,1)) + SUBSTRING(ttPageQuery.Operation,2),
         lcReqStruct).

END.

FUNCTION fSendNumberRangesDetail RETURNS LOGICAL
  (INPUT pcNumberRangeId AS CHAR):
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_string(lcReqStruct, "codigoRangoNumeracion", pcNumberRangeId).
  
   RETURN fMnpXMLSerialize("obtenerRangoNumeracion", lcReqStruct).
END.

&ENDIF
