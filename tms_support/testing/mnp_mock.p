/* ----------------------------------------------------------------------
MODULE .......: mnp_mock.p
TASK .........: Handle MNP messages 
APPLICATION ..: TMS
AUTHOR .......: Janne Tourunen 
CREATED ......: 26.9.2012
CHANGED ......:
Version ......: Yoigo
----------------------------------------------------------------------- */

{commpaa.i}
ASSIGN
   katun = "Qvantel"
      gcBrand = "1".
{cparam2.i}
{xmlrpc/xmlrpc_client.i}

DEFINE VARIABLE ocResponse AS CHAR NO-UNDO.

DEFINE VARIABLE lcMnpRefId AS CHAR NO-UNDO.
DEFINE VARIABLE lcMnpReqId AS CHAR NO-UNDO.
DEFINE VARIABLE lcCLI AS CHAR NO-UNDO.
DEFINE VARIABLE liMnpStCode AS INT NO-UNDO.
DEFINE VARIABLE lcMnpReason AS CHAR NO-UNDO.
DEFINE VARIABLE lcConURL AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNotifyStruct AS CHAR NO-UNDO.
DEFINE VARIABLE lcRequestStruct AS CHAR NO-UNDO.
DEFINE VARIABLE lcFieldStruct AS CHAR NO-UNDO.
DEFINE VARIABLE lcNotifyArray AS CHAR NO-UNDO.
DEFINE VARIABLE lcSubscriptionStruct AS CHAR NO-UNDO.
DEFINE VARIABLE lcIdDataStruct AS CHAR NO-UNDO.
DEFINE VARIABLE lcPersondata AS CHAR NO-UNDO.
DEFINE VARIABLE lcarray AS CHAR NO-UNDO.
DEFINE VARIABLE lcstruct AS CHAR NO-UNDO.
DEFINE VARIABLE lcInitInfo AS CHAR NO-UNDO INITIAL "123".
DEFINE VARIABLE lcFinResult AS CHAR NO-UNDO.
DEFINE VARIABLE lcReasons AS CHARACTER NO-UNDO. 
     
DEFINE {&SHARED} VARIABLE response_toplevel_id AS CHAR NO-UNDO.
DEFINE {&SHARED} VARIABLE gi_xmlrpc_error AS INT NO-UNDO.
DEFINE {&SHARED} VARIABLE gc_xmlrpc_error AS CHAR NO-UNDO.

lcConURL = fCParam("URL","UrlMnpMock").
IF lcConURL = ? OR lcConURL = "" THEN DO:
   ocResponse = "ERROR:Connection URL not defined".
   RETURN.
END.

FORM
    "MNP Reference Id........:" lcMnpRefId FORMAT "x(24)" HELP "Enter MNP Reference Id" SKIP
    "MSISDN..................:" lcCLI FORMAT "x(11)" SKIP
    "MNP New Status..........:" liMnpStCode FORMAT "9" HELP "Enter Status Code 4-Reject OR 5-Confirm OR 7-Cancel" SKIP
    "MNP Reason for Rejection:" lcMnpReason FORMAT "x(12)"
    WITH OVERLAY ROW 4 centered
    TITLE " MNP MOCK TOOL "
    NO-LABELS
    FRAME lis.

RUN pUserInput.

/* IF  User Wanted TO Cancel this Change TRANSACTION */
IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
KEYLABEL(lastkey) = "F4" THEN UNDO, RETURN.

RUN pAddRequestStructElement(
   INPUT liMnpStCode, 
   INPUT lcMnpReason,
   INPUT lcMnpRefId).

MESSAGE "Result" ocResponse VIEW-AS ALERT-BOX.
DEF VAR ufkey  AS LOG NO-UNDO INIT TRUE.

PROCEDURE pUserInput:

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      DISP  lcMnpRefId
            lcCLI
            liMnpStCode
            lcMnpReason WITH FRAME lis.

      UPDATE
            lcMnpRefId
            liMnpStCode
            lcMnpReason WITH FRAME lis EDITING:

            IF ufkey THEN DO:
               ASSIGN ehto = 9. RUN ufkey.p.
               ufkey = false.
            END.

            READKEY.

            nap = keylabel(lastkey).

            IF LOOKUP(nap,poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "lcMnpRefId" THEN DO:
                  FIND FIRST MNPProcess WHERE
                             MNPProcess.PortRequest = INPUT lcMnpRefId
                  NO-LOCK NO-ERROR.

                  IF NOT AVAIL MNPProcess OR INPUT lcMnpRefId EQ "" then do:
                     MESSAGE "MNP Reference number" lcMnpRefId "not found"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  end.

                  FIND FIRST MNPSub NO-LOCK WHERE
                             MNPSub.MNPSeq = MNPProcess.MNPSeq NO-ERROR.

                  ASSIGN
                        lcMnpReqId = MNPProcess.FormRequest
                        lcCLI = MNPSub.CLI.
                  DISPLAY
                        lcCLI WITH FRAME lis.
               END.

               ELSE IF FRAME-FIELD = "liMnpStCode" THEN DO:

                  IF LOOKUP(STRING(INPUT liMnpStCode),"4,5,7") = 0  THEN DO:
                        MESSAGE "MNP STATUS CAN BE WHETHER 4-REJECTED OR 5-CONFIRMED OR 7-CANCELLED" VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  IF INPUT liMNPStCode EQ 7 THEN DO: 
                     IF MNPProcess.StatusCode EQ 6 THEN DO:
                        MESSAGE "MNP process is already PORTED, cancellation is not allowed"
                        VIEW-AS ALERT-BOX.
                        NEXT.
                     END.
                     IF MNPProcess.MNPType EQ 1 THEN DO:
                        MESSAGE "Current request is MNP IN, ACAN(7) is only for MNP OUT"
                        VIEW-AS ALERT-BOX.
                        NEXT.
                     END.
                  END.

                  CASE INPUT liMNPStCode:
                     WHEN 4 THEN lcReasons = "AREC ENUME,RECH_BNUME,AREC EXIST,RECH_IDENT,RECH_ICCID".
                     WHEN 7 THEN lcReasons = "CANC_ABONA,CANC_TECNI,CANC_ERROR".
                     OTHERWISE lcReasons = "".
                  END.
                  
               END.
               ELSE IF FRAME-FIELD = "lcMnpReason" THEN DO:
                  IF INPUT liMnpStCode = 5 AND
                     INPUT lcMnpReason > "" THEN DO:
                     MESSAGE "IN CASE OF CONFIRMATION NO REASON REQUIRED"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                 
                  IF (INPUT liMnpStCode = 4 OR
                      INPUT liMnpStCode = 7) THEN DO:

                     IF INPUT lcMnpReason = "" THEN DO:
                        MESSAGE "REASON IS MANDATORY" SKIP
                           "Should be one of the following values" SKIP
                           lcReasons
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.

                     IF LOOKUP(INPUT lcMNPReason, lcReasons) = 0 THEN DO:
                        MESSAGE "REASON IS INCORRECT" SKIP
                           "Should be one of the following values" SKIP
                           lcReasons
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.
               END. 
            END.

         APPLY LASTKEY.
      END.
      LEAVE.
   END.
END PROCEDURE.

PROCEDURE pAddRequestStructElement:
      DEF INPUT PARAMETER iiStatusCode AS INT NO-UNDO.
      DEF INPUT PARAMETER icStatusReason AS CHAR NO-UNDO.
      DEF INPUT PARAMETER icMnpRefId AS CHAR NO-UNDO.

   DEFINE VARIABLE lcMethodName AS CHARACTER NO-UNDO. 

   FIND MNPProcess NO-LOCK WHERE
        MNPProcess.PortRequest = icMnpRefId NO-ERROR.

   FIND FIRST MNPDetails NO-LOCK WHERE
              MNPDetails.MNPSeq = MNPProcess.MNPSeq NO-ERROR.
   IF NOT AVAILABLE MNPDetails THEN DO:
        MESSAGE "MNPDetails record not found, Check the correct MNP".
        RETURN.
   END.

   FIND FIRST MNPSub NO-LOCK WHERE
              MNPSub.MNPSeq = MNPProcess.MNPSeq NO-ERROR.

   IF NOT AVAILABLE MNPSub THEN DO:
        MESSAGE "MNPSub record not found, Check the correct MNP".
        RETURN.
   END.

   initialize(lcConURL, 5).

   /* Main Struct */
   lcFieldStruct = add_struct(param_toplevel_id, "").
   add_string(lcFieldStruct, "codigoRespuesta","abc").

   /* Notify Array  */
   lcNotifyArray = add_array(lcFieldStruct, "notificacion").

   /* Notify Struct */
   lcNotifyStruct = add_struct(lcNotifyArray, "").
   add_datetime(lcNotifyStruct, "fechaCreacion", TODAY).
   add_boolean(lcNotifyStruct, "sincronizada", FALSE).
   add_string(lcNotifyStruct, "codigoNotificacion", lcInitInfo).

   /* Request Struct */
   lcRequestStruct = add_struct(lcNotifyStruct, "solicitud").

   add_timestamp(lcRequestStruct, "fechaCreacion", MNPProcess.CreatedTS).
   add_timestamp(lcRequestStruct, "fechaEstado", MNPProcess.UpdateTS).

   add_string(lcRequestStruct, "codigoReferencia", MNPProcess.PortRequest).
   add_string(lcRequestStruct, "estado", (IF iiStatusCode = 4 THEN "AREC" ELSE IF iiStatusCode = 5 THEN "ACON" ELSE "ACAN")).
   add_string(lcRequestStruct, "causaEstado", icStatusReason).
   add_timestamp(lcRequestStruct, "fechaVentanaCambio", MNPProcess.PortingTime).
   add_string(lcRequestStruct, "MSISDN", MNPSub.CLI).
   add_string(lcRequestStruct, "ICCID", MNPSub.ICC).
   add_string(lcRequestStruct, "codigoContrato", MNPProcess.FormRequest).


   add_boolean(lcRequestStruct, "fechaVentanaCambioPorAbonado", MNPDetails.PortingTimeFromCustomer).
   add_string(lcRequestStruct, "NRNReceptor", MNPDetails.ReceptorNRN).
   add_string(lcRequestStruct, "codigoOperadorReceptor", MNPDetails.ReceptorCode).
   add_string(lcRequestStruct, "codigoOperadorDonante", MNPDetails.DonorCode).
   add_timestamp(lcRequestStruct, "fechaSolicitudPorAbonado", MNPDetails.RequestedTS).
   add_timestamp(lcRequestStruct, "fechaLimiteCambioEstado", MNPDetails.StatusLimitTS).
   add_boolean(lcRequestStruct, "operadorDonanteAltaExtraordinaria", MNPDetails.DonorExtraOrdinary).

   /* Subscription Struct */
   lcSubscriptionStruct = add_struct(lcRequestStruct, "abonado").

   /* IdData Struct */
   lcIdDataStruct = add_struct(lcSubscriptionStruct, "documentoIdentificacion").
   add_string(lcIdDataStruct,"tipo", MNPDetails.CustIdType).
   add_string(lcIdDataStruct,"documento", MNPDetails.CustId).

   lcPersondata = add_struct(lcSubscriptionStruct, "datosPersonales").
   add_string(lcPersondata, "nombre", MNPDetails.FirstName).
   add_string(lcPersondata, "primerApellido", MNPDetails.SurName1).



   IF gi_xmlrpc_error NE 0 THEN DO:
   ocResponse = SUBST("ERROR: XML creation: &1", gc_xmlrpc_error).
   RETURN.
   END.

   IF iiStatusCode EQ 7 THEN lcMethodName = "mnp.obtenerNotificacionesAltaPortabilidadMovilComoDonanteCanceladas".
   ELSE lcMethodName = "mnp.obtenerNotificacionesAltaPortabilidadMovilComoReceptorConfirmadasRechazadas".

   RUN pRPCMethodCall(lcMethodName, TRUE).

   IF gi_xmlrpc_error NE 0 THEN DO:
   /* from TMS point of view this error is OK */
   IF TRIM(gc_xmlrpc_error) EQ "user not found" THEN
      ocResponse = SUBST("OK: &1", gc_xmlrpc_error).
      
   ELSE
      ocResponse = SUBST("ERROR: &1", gc_xmlrpc_error).
      RETURN.
   END.


   lcstruct = get_struct(response_toplevel_id, "").
   lcarray = get_array(lcstruct, "codigoNotificacion").
   lcFinResult = get_string(lcarray, "0").

   IF gi_xmlrpc_error NE 0 THEN DO:
   ocResponse = SUBST("ERROR: XML response parsing: &1", gc_xmlrpc_error).
   RETURN.
   END.

   IF lcFinResult = lcInitInfo
   THEN ocResponse = "OK - VERIFICATION SUCCESS".
   ELSE ocResponse = SUBST("ERROR: Response &1 NOT equal with sent &2",lcFinResult, lcInitInfo).

   FINALLY:
   xmlrpc_cleanup().
   xmlrpc_finalize().
   END.


END PROCEDURE. /* PROCEDURE pAddRequestStructElement */


