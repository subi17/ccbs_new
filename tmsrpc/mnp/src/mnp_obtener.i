/* ----------------------------------------------------------------------
  MODULE .......: mnp_obtener.i
  TASK .........: Common header for buzon notification rpcs 
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 01.09.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{mnp_common.i}

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* lcFields = validate_struct(pcStruct, "codigoRespuesta!,descripcion,campoErroneo,codigoPeticionPaginada!,totalRegistros!,ultimaPaginada,notificacion"). */
lcFields = validate_struct(pcStruct, "codigoRespuesta!,descripcion,campoErroneo,codigoPeticionPaginada,totalRegistros,ultimaPagina,notificacion"). 

IF gi_xmlrpc_error NE 0 THEN RETURN.

/* should not happen  */
IF LOOKUP("notificacion", lcFields) = 0 THEN DO:
   RETURN. /* nothing to do */
END.

pcNotificacions = get_array(pcStruct,"notificacion").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* input paramers parsing */
DO liCounter = 0 TO get_paramcount(pcNotificacions) - 1:
   
   pcNotificacion = get_struct(pcNotificacions, STRING(liCounter)).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   IF validate_struct(pcNotificacion, "fechaCreacion!,sincronizada!,codigoNotificacion!,solicitud!") = ? THEN RETURN.

   pcSolicitud = get_struct(pcNotificacion, "solicitud").
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   {mnp_solicitud.i}
   
   ttInput.NotificationCode = get_string(pcNotificacion,"codigoNotificacion").

END.

lcRespStruct = add_struct(response_toplevel_id, "").
lcRespArray = add_array(lcRespStruct, "codigoNotificacion").
