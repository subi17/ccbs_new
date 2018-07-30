/**
 * mnp.obtenerNotificacionesAltaPortabilidadMovilComoDonanteCanceladas
 *
 * 1.8 Notifications of portabilities in donor role cancelled by receiver operator 
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
 * @output codigoNotificacion;array of string;codigoNotificacion codes of handled messages
 */

{mnp/src/mnp_obtener.i} 
{Syst/tmsconst.i}
{Func/orderchk.i}
{Func/orderfunc.i}
{Func/fsubstermreq.i}
{Func/add_lines_request.i}

DEF BUFFER bMNPProcess FOR MNPProcess.
DEF BUFFER bMNPSub FOR MNPSub.
DEF BUFFER bOrder  FOR Order.
DEF BUFFER lbMobSub FOR MobSub.
DEF BUFFER bSTCMsRequest FOR MsRequest. 

DEF VAR lcMNPSMSText       AS CHAR  NO-UNDO.
DEF VAR ldaSecSIMTermDate  AS DATE  NO-UNDO.
DEF VAR liSecSIMTermTime   AS INT   NO-UNDO.
DEF VAR ldeSecSIMTermStamp AS DEC   NO-UNDO.
DEF VAR liRequest          AS INT   NO-UNDO.
DEF VAR liQuarTime         AS INT   NO-UNDO.
DEF VAR liSimStat          AS INT   NO-UNDO.
DEF VAR liMSISDNStat       AS INT   NO-UNDO.

MESSAGE_LOOP:
FOR EACH ttInput NO-LOCK:   
   
   fCreateMNPObtenerMessage("obtenerNotificacionesAltaPortabilidadMovilComoDonanteCanceladas").

   FIND MNPProcess WHERE MNPProcess.PortRequest = ttInput.PortRequest EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL MNPProcess THEN 
   DO:
      lcError = {&MNP_ERROR_UNKNOWN_PROCESS} + " " + ttInput.PortRequest.
      MNPOperation.MNPSeq = {&MNP_PROCESS_DUMMY_OUT}.
      fErrorHandle(lcError).
      fLogError(lcError). 
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.

   MNPOperation.MNPSeq = MNPProcess.MNPSeq.

   IF MNPProcess.MNPType NE {&MNP_TYPE_OUT} THEN DO:
      lcError = {&MNP_ERROR_WRONG_TYPE}.
      fErrorHandle(lcError).
      fLogError(lcError). 
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.

   IF ttInput.statusCode NE "ACAN" THEN DO:
      lcError = {&MNP_ERROR_WRONG_STATUS} + " " + ttInput.StatusCode.
      fErrorHandle(lcError).
      fLogError(lcError + ":" + ttInput.portRequest). 
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.
   
   /* duplicate message check */
   IF MNPProcess.StatusCode = {&MNP_ST_ACAN} THEN DO:
      ASSIGN
         MNPOperation.StatusCode = {&MNP_MSG_HANDLED}
         MNPOperation.ErrorCode = ""
         MNPOperation.ErrorHandled = 0.
      add_string(lcRespArray, "", ttInput.NotificationCode).
      NEXT MESSAGE_LOOP.
   END.

   /* Cancel termination requests */         
   FOR EACH MNPSub WHERE
      MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK:

      lcMNPSMSText = "".

      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq = MNPSub.MsSeq AND
                 MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                 MsRequest.ReqStatus = {&REQUEST_STATUS_NEW}
      NO-LOCK NO-ERROR.
      IF AVAIL MsRequest THEN DO:
         fReqStatus({&REQUEST_STATUS_CANCELLED},"Cancelled MNP Process").
         
         fAddLineSTCCancellation(MsRequest.MsRequest, MsRequest.CustNum).
          
      END.
   
      /* Cancel possible SMS messages */
      FOR EACH CallAlarm WHERE
               CallAlarm.Brand = Syst.Var:gcBrand AND
               CallAlarm.CLI = MNPSub.CLI AND
               CallAlarm.DeliStat = 1 AND
               CallAlarm.CreditType = {&SMSTYPE_MNP} EXCLUSIVE-LOCK:
         CallAlarm.DeliStat = 4. /* CANCELLED */
      END.
      
      FIND MobSub WHERE
         MobSub.MSSeq = MNPSub.MsSeq NO-LOCK NO-ERROR.

      IF AVAIL MobSub THEN 
         FIND Customer WHERE
              Customer.Custnum = MobSub.Custnum 
              NO-LOCK NO-ERROR.
   
      /* Release possible number return process that is on hold */
      FOR EACH bMNPSub WHERE
               bMNPSub.CLI = MNPSub.CLI,
         FIRST bMNPProcess WHERE
            bMNPProcess.MNPSeq = bMNPSub.MNPSeq AND
            bMNPProcess.MNPType = {&MNP_TYPE_TERMINATION} AND
            bMNPProcess.StatusCode = {&MNP_ST_BDET} EXCLUSIVE-LOCK:

         ASSIGN
            bMNPProcess.UpdateTS = Func.Common:mMakeTS()
            bMNPProcess.StatusCode = {&MNP_ST_BNOT}.
         RELEASE bMNPProcess.
      END.

      /* Now move Retention order into correct queue */
      IF AVAIL MobSub AND AVAIL Customer THEN DO:
         fRetention(Mobsub.MsSeq,
                    Customer.Language,
                    Customer.Custnum,
                    MNPProcess.FormRequest,
                    MNPSub.CLI).

         /* YDR-819 - Create CONTM termination request */
         IF MobSub.MultiSIMId > 0 AND
            MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} THEN DO:
            FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
                       lbMobSub.Brand  = Syst.Var:gcBrand AND
                       lbMobSub.MultiSimID = MobSub.MultiSimID AND
                       lbMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                       lbMobSub.Custnum = MobSub.Custnum NO-ERROR.
            IF NOT AVAIL lbMobSub THEN DO:
               FIND FIRST TermMobSub NO-LOCK USE-INDEX MultiSIM WHERE
                          TermMobSub.Brand  = Syst.Var:gcBrand AND
                          TermMobSub.MultiSimID = MobSub.MultiSimID AND
                          TermMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                          TermMobSub.Custnum = MobSub.Custnum NO-ERROR.
               IF NOT AVAIL TermMobSub THEN
                  ldaSecSIMTermDate = TODAY.
               ELSE DO:
                  FIND FIRST Msowner WHERE
                             Msowner.MsSeq = TermMobsub.MsSeq NO-LOCK NO-ERROR.
                  IF AVAIL Msowner THEN
                     Func.Common:mSplitTS(Msowner.TSEnd,OUTPUT ldaSecSIMTermDate,
                              OUTPUT liSecSIMTermTime).
                  ELSE ldaSecSIMTermDate = TODAY.
               END. /* ELSE DO: */

               fTermAdditionalSim(MobSub.Msseq,
                                  MobSub.CLI,
                                  MobSub.CustNum,
                                  {&SUBSCRIPTION_TERM_REASON_MULTISIM},
                                  ldaSecSIMTermDate,
                                  {&REQUEST_SOURCE_EXTERNAL_API},
                                  0,
                                  OUTPUT lcError).

            END. /* IF NOT AVAIL lbMobSub THEN DO: */
         END. /* IF MobSub.MultiSIMId > 0 AND */
         ELSE IF CAN-FIND(
            FIRST CLIType NO-LOCK WHERE
                  CLIType.Brand = Syst.Var:gcBrand AND
                  CLIType.CLIType = (IF MobSub.TariffBundle > ""
                                     THEN MobSub.TariffBundle
                                     ELSE MobSub.CLIType) AND
                  CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL}) THEN DO:
           
            IF NOT fIsMainLineSubActive(Customer.CustidType,
                                        Customer.OrgId) THEN 
               fTermAdditionalSim(MobSub.MsSeq,
                                  MobSub.CLI,
                                  MobSub.CustNum,
                                  {&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM},
                                  TODAY,
                                  {&REQUEST_SOURCE_EXTERNAL_API},
                                  0,
                                  OUTPUT lcError).
           
         END.
      END. /* IF AVAIL MobSub AND AVAIL Customer THEN DO: */

      IF lcMNPSMSText = "" THEN
         fMNPCallAlarm("MNPCancel",
                       {&nowts},
                       MNPProcess.FormRequest,
                       MNPSub.CLI,
                       (IF AVAIL MobSub THEN MobSub.Custnum ELSE 0),
                       (IF AVAIL Customer THEN Customer.Language ELSE 1),
                       "800622111",
                       0).

   END.
   
   IF ttInput.CancelProposalRef NE "" THEN
   FOR EACH MNPCancelProposal EXCLUSIVE-LOCK WHERE
      MNPCancelProposal.MNPSeq = MNPProcess.MNPSeq AND
      MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_NEW}:
      MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_CONFIRMED}.
   END.
   ELSE DO:
      FOR EACH MNPCancelProposal EXCLUSIVE-LOCK WHERE
         MNPCancelProposal.MNPSeq = MNPProcess.MNPSeq AND
         MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_NEW}:
         MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_CANCELLED}.
      END.
   END.

   ASSIGN
      MNPOperation.StatusCode = {&MNP_MSG_HANDLED}
      MNPOperation.ErrorCode = ""
      MNPOperation.ErrorHandled = 0
      MNPProcess.StatusCode = {&MNP_ST_ACAN}
      MNPProcess.StatusReason = ttInput.StatusReason
      MNPProcess.UpdateTS = {&nowts}
      MNPProcess.MNPUpdateTS = ttInput.statusTS.
   
   RELEASE MNPOperation.
   RELEASE MNPProcess.
   
   add_string(lcRespArray, "", ttInput.NotificationCode).

END.

IF AVAIL MNPBuzon THEN MNPBuzon.StatusCode = 10.

FINALLY:
   EMPTY TEMP-TABLE ttInput.
   EMPTY TEMP-TABLE ttMultipleMSISDN.
END.
