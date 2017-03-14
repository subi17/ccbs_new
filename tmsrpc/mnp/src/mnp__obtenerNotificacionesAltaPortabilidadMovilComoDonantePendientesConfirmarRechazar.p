/**
 * mnp.obtenerNotificacionesAltaPortabilidadMovilComoDonantePendientesConfirmarRechazar
 *
 * 1.4 Notifications of portabilities in donor role to be confirmed/rejected
 *
 * @input codigoRespuesta;string;mandatory;
   descripcion;string;optional;
   campoErroneo;array;optional;
   codigoPeticionPaginada;string;optional;
   totalRegistros;int;optional;
   ultimaPaginada;boolean;mandatory;
   notificacion;array of notification_elem;
 * @notification_elem fechaCreacion;datetime;mandatory;
   sincronizada;boolean;mandatory;
   codigoNotificacion;string;mandatory;
   solicitud;array of solicitud_elem;optional;
 * @soliticud_elem fechaCreacion;datetime;mandatory;creation time
   fechaEstado;datetime;mandatory;status change time
   codigoReferencia;string;mandatory;mnp process code
   fechaMarcaLectura;datetime;optional;message read time
   estado;string;mandatory;status (ACON, AREC)
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
 * @output codigoNotificacion;array of string;codigoNotificacion codes of handled messages
 */

{mnp/src/mnp_obtener.i}

FOR EACH ttInput NO-LOCK:
   IF ttInput.statusCode NE "ASOL" THEN 
      RETURN appl_err("Incorrect statuscode (should be AENV): " +
         ttInput.statusCode).
END.

MESSAGE_LOOP:
FOR EACH ttInput NO-LOCK:   
   
   {mnp/src/mnp_findtenant.i NO common MNPProcess PortRequest ttInput.PortRequest}

   /* create mnpmessage record */
   fCreateMNPObtenerMessage("obtenerNotificacionesAltaPortabilidadMovilComoDonantePendientesConfirmarRechazar").
   
   /* check in case of duplicate messages */
   FIND FIRST MNPProcess NO-LOCK WHERE
              MNPProcess.PortRequest = ttInput.PortRequest NO-ERROR.
   
   IF NOT AVAIL MNPProcess THEN DO:   
      
      CREATE MNPProcess.
      ASSIGN
         MNPProcess.UpdateTS    = {&nowts} 
         MNPProcess.CreatedTS   = ttInput.CreatedTS
         MNPProcess.MNPUpdateTS = ttInput.StatusTS
         MNPProcess.Brand       = gcBrand
         MNPProcess.MNPType     = {&MNP_TYPE_OUT} /* mnp out */
         MNPProcess.MNPSeq      = next-value(m2mrequest)
         MNPProcess.PortingTime = ttInput.portingTime
         MNPProcess.FormRequest = ttInput.FormRequest 
         MNPProcess.PortRequest = ttInput.PortRequest
         MNPProcess.OperCode    = ttInput.ReceptorCode
         MNPProcess.StatusCode  = {&MNP_ST_ASOL} 
         MNPProcess.StatusReason = ttInput.StatusReason
         MNPProcess.UserCode    = katun.
     
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

   add_string(lcRespArray, "", ttInput.NotificationCode).

END.

IF AVAIL MNPBuzon THEN MNPBuzon.StatusCode = 10.

FINALLY:
   EMPTY TEMP-TABLE ttInput.
   EMPTY TEMP-TABLE ttMultipleMSISDN.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
