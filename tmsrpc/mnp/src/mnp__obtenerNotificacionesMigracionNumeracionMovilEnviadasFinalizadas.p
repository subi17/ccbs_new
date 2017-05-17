/**
 * mnp.obtenerNotificacionesMigracionNumeracionMovilEnviadasFinalizadas.p
 *
 * 2.1 Mobile numbering migration notifications Sent/Finalized 
 *
 * @input codigoRespuesta;string;mandatory;
   descripcion;string;optional;
   campoErroneo;array;optional;
   codigoPeticionPaginada;string;optional;
   totalRegistros;int;optional;
   ultimaPagina;boolean;mandatory;
   notificacion;array of notification_elem;
 * @notification_elem fechaCreacion;datetime;mandatory;
   sincronizada;boolean;mandatory;
   codigoNotificacion;string;mandatory;
   solicitud;array of solicitud_elem;optional;
 * @soliticud_elem fechaCreacion;datetime;mandatory;creation time
   fechaEstado;datetime;mandatory;status change time
   codigoReferencia;string;mandatory;mnp process code
   estado;string;mandatory;status (ACON, AREC)
 * @output codigoNotificacion;array of string;codigoNotificacion codes of handled messages
 */

{mnp/src/mnp_common.i}
{mnp/src/mnp_notification.i}

/* input paramers parsing */
DO liCounter = 0 TO get_paramcount(pcNotificacions) - 1:
   
   pcNotificacion = get_struct(pcNotificacions, STRING(liCounter)).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   IF validate_struct(pcNotificacion, "fechaCreacion!,sincronizada!,codigoNotificacion!,solicitud!") = ? THEN RETURN.

   pcSolicitud = get_struct(pcNotificacion, "solicitud").
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   lcSolicitudFields = validate_struct(pcSolicitud,
      "fechaCreacion!," + 
      "fechaEstado!," + 
      "codigoReferencia!," + 
      "escenario!," +
      "estado!," + 
      "fechaInicio!," + 
      "fechaMaximaFinalizacion!," +
      "volumenNumeracion!," + 
      "codigoOperadorPrestadorServicioOrigen," + 
      "codigoOperadorRedOrigen!," + 
      "codigoOperadorRedDestino!").

   IF gi_xmlrpc_error NE 0 THEN RETURN.

   CREATE ttInput.
   ASSIGN
      ttInput.createdTS   = get_timestamp(pcSolicitud, "fechaCreacion")
      ttInput.statusTS    = get_timestamp(pcSolicitud, "fechaEstado")
      ttInput.statusCode  = get_string(pcSolicitud, "estado")
      ttInput.PortRequest = get_string(pcSolicitud, "codigoReferencia").
  
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   ttInput.NotificationCode = get_string(pcNotificacion,"codigoNotificacion").

END.

lcRespStruct = add_struct(response_toplevel_id, "").
lcRespArray = add_array(lcRespStruct, "codigoNotificacion").

MESSAGE_LOOP:
FOR EACH ttInput NO-LOCK:   
   
   fCreateMNPObtenerMessage("obtenerNotificacionesMigracionNumeracionMovilEnviadasFinalizadas").
         
   FIND MNPProcess WHERE
        MNPProcess.PortRequest = ttInput.PortRequest EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL MNPProcess THEN DO:
      lcError = {&MNP_ERROR_UNKNOWN_PROCESS}.
      fErrorHandle(lcError).
      fLogError(lcError + ":" + ttInput.PortRequest). 
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.
   
   MNPOperation.MNPSeq = MNPProcess.MNPSeq.
   
   IF MNPProcess.MNPType NE {&MNP_TYPE_MIGRATION} THEN DO:
      lcError = {&MNP_ERROR_WRONG_TYPE}.
      fErrorHandle(lcError).
      fLogError(lcError + ":" +  ttInput.PortRequest). 
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.

/*   MNPProcess.StatusCode =    */
   MNPProcess.MNPUpdateTS = ttInput.statusTS.
   MNPProcess.UpdateTS = {&nowts}.
   MNPOperation.StatusCode = {&MNP_MSG_HANDLED}.
   MNPOperation.ErrorHandled = 0.
   MNPOperation.ErrorCode = "".
   add_string(lcRespArray, "", ttInput.NotificationCode).

END.

IF AVAIL MNPBuzon THEN MNPBuzon.StatusCode = 10.

FINALLY:
   EMPTY TEMP-TABLE ttInput.
   EMPTY TEMP-TABLE ttMultipleMSISDN.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
