/* ----------------------------------------------------------------------
MODULE .......: mnpcontingency.i
TASK .........: MNP contingency interface client message type defs.
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 01.11.09
Version ......: xfera
----------------------------------------------------------------------- */

DEFINE VARIABLE liContLoop AS INTEGER NO-UNDO. 
DEFINE TEMP-TABLE ttContingency
   FIELD operation AS CHAR
   FIELD xmlToken AS CHAR
   FIELD fileToken AS CHAR
INDEX i IS PRIMARY UNIQUE operation. 

DEFINE VARIABLE lcContingencyMessageTypes AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcContingencyMessageType AS CHARACTER NO-UNDO. 

lcContingencyMessageTypes =   
   "obtenerSolicitudesAltaPortabilidadMovilComoDonantePendientesConfirmarRechazar:alta_receptor_confrech:," +
   "crearSolicitudIndividualAltaPortabilidadMovil:alta:formrequest," +
   "confirmarSolicitudAltaPortabilidadMovil:confirmar:portrequest," + 
   "rechazarSolicitudAltaPortabilidadMovil:rechazar:portrequest," +
   "cancelarSolicitudAltaPortabilidadMovil:cancelacion_alta_receptor:portrequest," + 
   "crearSolicitudBajaNumeracionMovil:baja:msisdn," +
   "cancelarSolicitudBajaNumeracionMovil:cancelacion_baja:portrequest".

DO liContLoop = 1 TO NUM-ENTRIES(lcContingencyMessageTypes):
   lcContingencyMessageType = ENTRY(liContLoop, lcContingencyMessageTypes).

   CREATE ttContingency.
   ASSIGN
      ttContingency.operation = entry(1,lcContingencyMessageType,":")
      ttContingency.fileToken = entry(2,lcContingencyMessageType,":")
      ttContingency.xmlToken = entry(3,lcContingencyMessageType,":").
END.
