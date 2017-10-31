/**
 * mnp.obtenerSolicitudesAltaPortabilidadMovilComoDonantePendientesConfirmarRechazar
 *
 * 1.4 Notifications of portabilities in donor role to be confirmed/rejected (used with buzon interface only)
 *
 * @input codigoRespuesta;string;mandatory;
   descripcion;string;optional;
   campoErroneo;array;optional;
   codigoPeticionPaginada;string;optional;
   totalRegistros;int;optional;
   ultimaPaginada;boolean;mandatory;
   solicitud;array of solicitud_elem;
 * @soliticud_elem fechaCreacion;datetime;mandatory;creation time
   fechaEstado;datetime;mandatory;status change time
   codigoReferencia;string;mandatory;mnp process code
   fechaMarcaLectura;datetime;optional;message read time
   estado;string;mandatory;status (ASOL)
   causaEstado;string;optional;status reason
   fechaLimiteCambioEstado;datetime;optional;status change time limit
   fechaSolicitudPorAbonado;datetime;mandatory;mnp start date
   codigoOperadorDonante;string;mandatory;donator operator code
   operadorDonanteAltaExtraordinaria;boolean;mandatory;donator operator in extraordinary status
   codigoOperadorReceptor;string;mandatory;receptor operator code
   abonado;string;mandatory;person data
   codigoContrato;string;mandatory;CCBS mnp process code
   NRNReceptor;string;mandatory;NRN of receptor
   fechaVentanaCambio;datetime;mandatory;porting time
   fechaVentanaCambioPorAbonado;boolean;mandatory;porting time is defined by customer
   codigoSolicitudCancelacionPortabilidadMovilIniciadaPorDonante;string;optional;cancellation code of customer initiated cancellation
   MSISDN;string;optional;
   ICCID;string;optional;
   rangoMSISDN;string;optional;
   ICCIDRelativoMSISDN;string;optional;
 * @output codigoReferencia;array of string;codigoReferencia codes of handled messages
 */

{mnp/src/mnp_obtener_solicitudes.i}
DO liCounter = 0 TO get_paramcount(pcSolicituds) - 1:
   
   pcSolicitud = get_struct(pcSolicituds, STRING(liCounter)).
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   {mnp/src/mnp_solicitud.i}
   
END.

FOR EACH ttInput NO-LOCK:
   IF ttInput.statusCode NE "ASOL" THEN 
      RETURN appl_err("Incorrect statuscode (should be ASOL): " +
         ttInput.statusCode).
END.
   
lcRespStruct = add_struct(response_toplevel_id, "").
lcRespArray = add_array(lcRespStruct, "codigoReferencia").

/* prevalidation */
FOR EACH ttInput NO-LOCK:   
   {mnp/src/mnp_findtenant.i NO common MNPProcess PortRequest ttInput.PortRequest}
END.

MESSAGE_LOOP:
FOR EACH ttInput NO-LOCK:   
   
   {mnp/src/mnp_findtenant.i NO common MNPProcess PortRequest ttInput.PortRequest}

   /* create mnpmessage record */
   fCreateMNPObtenerMessage("obtenerSolicitudesAltaPortabilidadMovilComoDonantePendientesConfirmarRechazar").

   IF NOT AVAIL MNPProcess THEN DO:   
      
      CREATE MNPProcess.
      ASSIGN
         MNPProcess.UpdateTS    = {&nowts} 
         MNPProcess.CreatedTS   = ttInput.CreatedTS
         MNPProcess.MNPUpdateTS = ttInput.StatusTS
         MNPProcess.Brand       = Syst.CUICommon:gcBrand
         MNPProcess.MNPType     = {&MNP_TYPE_OUT} /* mnp out */
         MNPProcess.MNPSeq      = next-value(m2mrequest)
         MNPProcess.PortingTime = ttInput.portingTime
         MNPProcess.FormRequest = ttInput.FormRequest 
         MNPProcess.PortRequest = ttInput.PortRequest
         MNPProcess.OperCode    = ttInput.ReceptorCode
         MNPProcess.StatusCode  = {&MNP_ST_ASOL} 
         MNPProcess.StatusReason = ttInput.StatusReason
         MNPProcess.UserCode    = Syst.CUICommon:katun.
     
      FOR EACH ttMultipleMSISDN WHERE 
               ttMultipleMSISDN.portrequest = ttInput.portRequest NO-LOCK:

         FIND MobSub WHERE
              MobSub.CLI = ttMultipleMSISDN.cli NO-LOCK NO-ERROR.
   
         CREATE MNPSub.
         ASSIGN
            MNPSub.MNPSeq = MNPProcess.MNPSeq
            MNPSub.PortingTime = ttInput.portingTime
            MNPSub.CLI = ttMultipleMSISDN.CLI
            MNPSub.ICC = ttMultipleMSISDN.ICC
            MNPSub.NRN = ttInput.ReceptorNRN
            MNPSub.MsSeq = MobSub.MsSeq WHEN AVAIL MobSub.
      END.
      
      /* MNP information */
      FIND MNPDetails EXCLUSIVE-LOCK WHERE
           MNPDetails.MNPSeq = MNPProcess.MNPSeq NO-ERROR.
      IF NOT AVAIL MNPDetails THEN DO:
         CREATE MNPDetails.
         ASSIGN MNPDetails.MNPSeq = MNPProcess.MNPSeq.
      END.
      ASSIGN
         MNPDetails.CustId = ttInput.CustId
         MNPDetails.CustIdType = (IF ttInput.CustIdType EQ "PAS" THEN "passport"
                                  ELSE ttInput.CustIdType)
         MNPDetails.FirstName  = ttInput.FirstName
         MNPDetails.Surname1 = ttInput.LastName
         MNPDetails.Surname2 = ttInput.LastName2
         MNPDetails.Nationality = ttInput.Nationality
         MNPDetails.ReceptorCode = ttInput.ReceptorCode
         MNPDetails.CompanyName = ttInput.Company
         MNPDetails.ReceptorNRN = ttInput.ReceptorNRN
         MNPDetails.DonorCode = ttInput.DonorCode
         MNPDetails.RequestedTS = ttInput.RequestedTS
         MNPDetails.StatusLimitTS = ttInput.StatusLimitTS
         MNPDetails.DonorExtraOrdinary = ttInput.DonorExtraOrdinary
         MNPDetails.PortingTimeFromCustomer = ttInput.PortingTimeFromCustomer.
   END.
   
   MNPOperation.MNPSeq = MNPProcess.MNPSeq.

   IF NOT NEW MNPProcess THEN DO:
      lcError = "MNP process already exists".
      fErrorHandle(lcError).
      fLogError(lcError + ":" + ttInput.portRequest).
   END.
   ELSE DO:
      MNPOperation.StatusCode = {&MNP_MSG_HANDLED}.
      MNPOperation.ErrorCode = "".
      MNPOperation.ErrorHandled = 0.
   END.

   add_string(lcRespArray, "", ttInput.PortRequest).

END.

IF AVAIL MNPBuzon THEN MNPBuzon.StatusCode = 10.

FINALLY:
   EMPTY TEMP-TABLE ttInput.
   EMPTY TEMP-TABLE ttMultipleMSISDN.
   END.
