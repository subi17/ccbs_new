/**
 * mnp.obtenerNotificacionesAltaPortabilidadMovilComoReceptorConfirmadasRechazadas
 *
 * 1.4 Notifications of portabilities in receiver role confirmed/rejected
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
{Func/orderfunc.i}
{Func/ordercancel.i}

DEF VAR lcSMS AS CHAR NO-UNDO. 

MESSAGE_LOOP:
FOR EACH ttInput NO-LOCK:   
   
   /* create mnpmessage record */
   fCreateMNPObtenerMessage("obtenerNotificacionesAltaPortabilidadMovilComoReceptorConfirmadasRechazadas").

   FIND MNPProcess WHERE
        MNPProcess.PortRequest = ttInput.PortRequest EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL MNPProcess THEN DO:
      lcError = {&MNP_ERROR_UNKNOWN_PROCESS} + ":" + ttInput.PortRequest.
      MNPOperation.MNPSeq = {&MNP_PROCESS_DUMMY_IN}.
      fErrorHandle(lcError).
      fLogError(lcError). 
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.
   
   MNPOperation.MNPSeq = MNPProcess.MNPSeq.
   
   IF MNPProcess.MNPType NE {&MNP_TYPE_IN} THEN DO:
      lcError = {&MNP_ERROR_WRONG_TYPE}.
      fErrorHandle(lcError).
      fLogError(lcError + ":" + MNPProcess.PortRequest). 
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.

   FIND Order WHERE
        Order.Brand = gcBrand AND
        Order.OrderId = MNPProcess.OrderId EXCLUSIVE-LOCK NO-ERROR.
   
   IF NOT AVAIL Order THEN DO:
      lcError = "Unknown Order " + STRING(MNPProcess.OrderId).
      fErrorHandle(lcError).
      fLogError(lcError + ":" + MNPProcess.PortRequest). 
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.

   FIND FIRST OrderCustomer WHERE
            OrderCustomer.Brand   = Order.Brand   AND
            OrderCustomer.OrderId = Order.OrderId AND
            OrderCustomer.RowType = 1 NO-LOCK.
   
   liLang = INT(OrderCustomer.Language) NO-ERROR.
            
   /* Actual message handling starts */
   IF ttInput.StatusCode = "AREC" THEN DO:

      IF MNPProcess.StatusCode NE {&MNP_ST_AREC} THEN DO:
            
      IF LOOKUP(STRING(MNPProcess.StatusCode),"1,2") = 0 THEN DO:
         lcError = "Wrong process status".
         fErrorHandle(lcError).
         fLogError(lcError + ":" + ttInput.PortRequest).
         add_string(lcRespArray, "", ttInput.NotificationCode).
         NEXT MESSAGE_LOOP.
      END.
      
      lcSMS = "".

      IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 THEN DO:
         IF ttInput.StatusReason EQ "RECH_IDENT" THEN lcSMS = "MNPIdentDirect".
         ELSE lcSMS = "MNPReject".
      END.
      ELSE IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 THEN DO:
         IF ttInput.StatusReason EQ "AREC ENUME" THEN lcSMS = "MNPEnumePOS".
         IF ttInput.StatusReason EQ "RECH_ICCID" THEN lcSMS = "MNPIccidPOS".
      END.

      IF lcSMS > "" THEN
      fMNPCallAlarm(lcSMS,
                   0.0,
                   MNPProcess.FormRequest,
                   Order.CLI,
                   Order.CustNum,
                   liLang,
                   "800622111",
                   Order.OrderId).
   
      ASSIGN
         MNPProcess.StatusCode = {&MNP_ST_AREC}.
         Order.MNPStatus = {&MNP_ST_AREC} + 1.

      fSetOrderStatus(Order.OrderId,"73").
      
      END.
   END.
   /* 
      Rules:
      - Create subscription creation request
      - Send SMS messages to customer
   */
   ELSE IF ttInput.StatusCode = "ACON" THEN DO:
   
      IF MNPProcess.StatusCode NE {&MNP_ST_ACON} THEN DO:
      
      IF LOOKUP(STRING(MNPProcess.StatusCode),"1,2") = 0 THEN DO:
         lcError = "Wrong process status".
         fErrorHandle(lcError).
         fLogError(lcError + ":" + ttInput.PortRequest).
         add_string(lcRespArray, "", ttInput.NotificationCode).
         NEXT MESSAGE_LOOP.
      END.
            
      /* just in case check if porting time has been changed */
      FOR EACH ttMultipleMSISDN WHERE 
         ttMultipleMSISDN.portrequest = MNPProcess.portRequest NO-LOCK:

         FIND MNPSub WHERE
              MNPSub.MNPSeq = MNPProcess.MNPSeq AND
              MNPSub.CLI = ttMultipleMSISDN.cli NO-LOCK NO-ERROR.
         
         IF AVAIL MNPSub AND MNPSub.PortingTime NE ttInput.PortingTime THEN DO:
            FIND CURRENT MNPSub EXCLUSIVE-LOCK.
            MNPSub.PortingTime = ttInput.portingTime.
            MNPProcess.PortingTime = ttInput.portingTime.
         END.
      END.
     
      /* double check activation */
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq = Order.MsSeq AND
                 MsRequest.ReqType = ({&REQTYPE_SUBSCRIPTION_CREATE})
      NO-LOCK NO-ERROR.
      IF NOT AVAIL MsRequest OR Order.OrderType EQ 3 THEN DO:
  
         RUN Gwy/ordersender.p(MNPProcess.OrderId,
                         OUTPUT liOrderQty).        
         
      END.

      fMNPCallAlarm("MNPConf",
                 ttInput.PortingTime,
                 MNPProcess.FormRequest,
                 Order.CLI,
                 Order.CustNum,
                 liLang,
                 "800622600",
                 Order.OrderId).

      lcSMS = (IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0
               THEN "MNPFinRemPos" ELSE "MNPFinRemDirect").
               
      fMNPCallAlarm(lcSMS,
                 ttInput.PortingTime,
                 MNPProcess.FormRequest,
                 Order.CLI,
                 Order.CustNum,
                 liLang,
                 "800622600",
                 Order.OrderId).

       ASSIGN
         MNPProcess.StatusCode = {&MNP_ST_ACON}
         Order.MNPStatus = {&MNP_ST_ACON} + 1. 
      END.
   END.           

   ELSE DO:
      lcError = {&MNP_ERROR_WRONG_STATUS} + " " + ttInput.StatusCode.
      fErrorHandle(lcError).
      fLogError(lcError + ":" + ttInput.PortRequest).
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.

   ASSIGN
      MNPOperation.StatusCode = {&MNP_MSG_HANDLED}
      MNPOperation.ErrorCode = ""
      MNPOperation.ErrorHandled = 0
      MNPProcess.StatusReason = ttInput.StatusReason
      MNPProcess.UpdateTS = {&nowts}
      MNPProcess.MNPUpdateTS = ttInput.statusTS.
      
   /* YOT-924 - AREC IDENT automatic cancellation 
      YDR-2506 - no automatic cancellation for convergent tarifs */
   IF Order.StatusCode = "73" AND
      LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
      ttInput.StatusReason EQ "RECH_IDENT" AND
      NOT fIsConvergenceTariff(Order.CliType) THEN DO:
      RUN Mc/closeorder.p(Order.OrderId,TRUE).
      IF RETURN-VALUE EQ "" THEN fReleaseIMEI(Order.Orderid).
   END.   
   
   add_string(lcRespArray, "", ttInput.NotificationCode).

END.

IF AVAIL MNPBuzon THEN MNPBuzon.StatusCode = 10.

FINALLY:
   EMPTY TEMP-TABLE ttInput.
   EMPTY TEMP-TABLE ttMultipleMSISDN.
   IF llDoEvent THEN fCleanEventObjects().
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
