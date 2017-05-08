/* ----------------------------------------------------------------------
  MODULE .......: mnp_obtener_solicitudes.i 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 02.09.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{mnp/src/mnp_common.i}
/* lcFields = validate_struct(pcStruct, "codigoRespuesta!,descripcion,campoErroneo,codigoPeticionPaginada!,totalRegistros!,ultimaPaginada,solicitud"). */
lcFields = validate_struct(pcStruct, "codigoRespuesta!,descripcion,campoErroneo,codigoPeticionPaginada,totalRegistros,ultimaPagina,solicitud"). 
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* should not happen  */
IF LOOKUP("solicitud", lcFields) = 0 THEN DO:
   add_int(response_toplevel_id, "", 0).
   RETURN. /* nothing to do */
END.

pcSolicituds = get_array(pcStruct,"solicitud").
IF gi_xmlrpc_error NE 0 THEN RETURN.
