/* ----------------------------------------------------------------------
  MODULE .......: mnp_notification.i
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

lcFields = validate_struct(pcStruct, "codigoRespuesta!,descripcion,campoErroneo,codigoPeticionPaginada,totalRegistros,ultimaPagina,notificacion"). 

IF gi_xmlrpc_error NE 0 THEN RETURN.

/* should not happen  */
IF LOOKUP("notificacion", lcFields) = 0 THEN DO:
   RETURN. /* nothing to do */
END.

pcNotificacions = get_array(pcStruct,"notificacion").
IF gi_xmlrpc_error NE 0 THEN RETURN.
