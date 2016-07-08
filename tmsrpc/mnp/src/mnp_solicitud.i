{mnp/src/mnp_common.i}

   lcSolicitudFields = validate_struct(pcSolicitud, "fechaCreacion!,fechaEstado!,codigoReferencia!,fechaMarcaLectura,estado!,causaEstado,fechaLimiteCambioEstado,fechaSolicitudPorAbonado!,codigoOperadorDonante!,operadorDonanteAltaExtraordinaria!,codigoOperadorReceptor!,abonado!,codigoContrato!,NRNReceptor!,fechaVentanaCambio!,fechaVentanaCambioPorAbonado!,codigoSolicitudCancelacionPortabilidadMovilIniciadaPorDonante,ICCID,MSISDN,rangoMSISDN,ICCIDRelativoMSISDN").
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   CREATE ttInput.
   ASSIGN
      ttInput.createdTS   = get_timestamp(pcSolicitud, "fechaCreacion")
      ttInput.statusTS    = get_timestamp(pcSolicitud, "fechaEstado")
      ttInput.statusCode  = get_string(pcSolicitud, "estado")
      ttInput.statusReason = get_string(pcSolicitud, "causaEstado") WHEN
         LOOKUP("causaEstado", lcSolicitudFields) > 0
      ttInput.portingTime = get_timestamp(pcSolicitud, "fechaVentanaCambio")
      ttInput.cli         = get_string(pcSolicitud, "MSISDN") WHEN lookup("MSISDN",lcSolicitudFields) > 0
      ttInput.icc         = get_string(pcSolicitud, "ICCID") WHEN lookup("ICCID",lcSolicitudFields) > 0
      ttInput.FormRequest = get_string(pcSolicitud, "codigoContrato")
      ttInput.PortingTimeFromCustomer = get_bool(pcSolicitud, "fechaVentanaCambioPorAbonado")
      ttInput.ReceptorNRN = get_string(pcSolicitud, "NRNReceptor")
      ttInput.receptorCode = get_string(pcSolicitud,"codigoOperadorReceptor")
      ttInput.DonorCode   = get_string(pcSolicitud,"codigoOperadorDonante")
      ttInput.RequestedTS  = get_timestamp(pcSolicitud,"fechaSolicitudPorAbonado")
      ttInput.StatusLimitTS = get_timestamp(pcSolicitud,"fechaLimiteCambioEstado") WHEN LOOKUP("fechaLimiteCambioEstado", lcSolicitudFields) > 0
      ttInput.DonorExtraOrdinary = get_bool(pcSolicitud, "operadorDonanteAltaExtraordinaria")
      ttInput.cancelProposalRef = get_string(pcSolicitud, "codigoSolicitudCancelacionPortabilidadMovilIniciadaPorDonante") WHEN LOOKUP("codigoSolicitudCancelacionPortabilidadMovilIniciadaPorDonante", lcSolicitudFields) > 0
      ttInput.PortRequest = get_string(pcSolicitud, "codigoReferencia").
   
   
   DEF VAR pcSubscription AS CHARACTER NO-UNDO. 
   DEF VAR pcPersondata AS CHARACTER NO-UNDO. 
   DEF VAR pcIdData AS CHARACTER NO-UNDO. 
   DEF VAR lcPersonData AS CHARACTER NO-UNDO. 

   pcSubscription = get_struct(pcSolicitud,"abonado").
   pcIdData = get_struct(pcSubscription,"documentoIdentificacion").
   ttInput.CustIdType = get_string(pcIdData,"tipo").
   ttInput.CustId = get_string(pcIdData,"documento").

   pcPersondata = get_struct(pcSubscription,"datosPersonales"). 
   IF ttInput.CustIdType = "CIF" THEN DO:
      ttInput.Company = get_string(pcPersonData,"razonSocial").
   END.
   ELSE DO:
      
      lcPersondata = validate_struct(pcPersonData, "nombre!,primerApellido!,segundoApellido,nacionalidad").
      IF gi_xmlrpc_error NE 0 THEN RETURN.
      
      ttInput.FirstName = get_string(pcPersonData,"nombre").
      ttInput.LastName = get_string(pcPersonData,"primerApellido").
      ASSIGN
         ttInput.LastName2 = get_string(pcPersonData,"segundoApellido") WHEN
            LOOKUP("segundoApellido", lcPersondata) > 0
         ttInput.Nationality = get_string(pcPersonData,"nacionalidad") WHEN
            LOOKUP("nacionalidad", lcPersondata) > 0.
   END.
  
   IF lookup("MSISDN",lcSolicitudFields) > 0  THEN DO:
      CREATE ttMultipleMSISDN.
      ASSIGN 
         ttMultipleMSISDN.icc = get_string(pcSolicitud, "ICCID") WHEN lookup("ICCID",lcSolicitudFields) > 0
         ttMultipleMSISDN.cli = get_string(pcSolicitud, "MSISDN")
         ttMultipleMSISDN.PortRequest = ttInput.portRequest.
   END.
   /* multiple msisdn handling */
   else IF lookup("MSISDN",lcSolicitudFields) = 0 THEN DO:
      
      pcMSISDNArray = get_array(pcSolicitud, "rangoMSISDN").
      IF gi_xmlrpc_error NE 0 THEN RETURN.
      
      DO liMsisdnCounter = 0 TO get_paramcount(pcMsisdnArray) - 1:

         pcMSISDNStruct = get_struct(pcMsisdnArray, STRING(liMsisdnCounter)).

         pcMsisdnBegin = get_string(pcMSISDNStruct,"valorInicial").
         pcMsisdnEnd = get_string(pcMSISDNStruct,"valorFinal").
         IF gi_xmlrpc_error NE 0 THEN RETURN.
         
         IF pcMsisdnBegin ne pcMsisdnEnd THEN DO:
            
            liMsisdn = INT(pcMsisdnBegin) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN appl_err("Incorrect MSISDN format: " + pcMsisdnBegin).
            
            liMsisdnEnd = INT(pcMsisdnEnd) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN appl_err("Incorrect MSISDN format: " + pcMsisdnEnd).
            
            DO lii = liMsisdn TO liMsisdnEnd:
               CREATE ttMultipleMSISDN.
               ASSIGN 
                  ttMultipleMSISDN.PortRequest = ttInput.portRequest
                  ttMultipleMSISDN.cli = STRING(lii).
            END.

         END.
         ELSE DO:
            CREATE ttMultipleMSISDN.
            ASSIGN ttMultipleMSISDN.cli = pcMsisdnBegin
                  ttMultipleMSISDN.PortRequest = ttInput.portRequest.
         END.
      END.
      
   END.
   
   IF lookup("ICCIDRelativoMSISDN",lcSolicitudFields) > 0 THEN DO:
      
      pcICCIDArray = get_array(pcSolicitud, "ICCIDRelativoMSISDN").
      IF gi_xmlrpc_error NE 0 THEN RETURN.
   
      DO liMsisdnCounter = 0 TO get_paramcount(pcICCIDArray) - 1:
         
         pcICCStruct = get_struct(pcICCIDArray, STRING(liMsisdnCounter)).

         pcMsisdnBegin = get_string(pcICCStruct,"MSISDN").
         
         FIND FIRST ttMultipleMSISDN WHERE
            ttMultipleMSISDN.portRequest = ttInput.portRequest and
            ttMultipleMSISDN.cli = pcMSISDNBegin EXCLUSIVE-LOCK NO-ERROR.

         IF NOT AVAIL ttMultipleMSISDN THEN CREATE ttMultipleMSISDN.
         
         ASSIGN
            ttMultipleMSISDN.icc = get_string(pcICCStruct,"ICCID")
            ttMultipleMSISDN.portRequest = ttInput.portRequest
            ttMultipleMSISDN.cli = pcMSISDNBegin.
         IF gi_xmlrpc_error NE 0 THEN RETURN.

      END.
   
   END.

   IF gi_xmlrpc_error NE 0 THEN RETURN.
