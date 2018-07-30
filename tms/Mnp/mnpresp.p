/* ----------------------------------------------------------------------
  MODULE .......: mnpresp.p
  TASK .........: Analyses MNP send message responses 
  APPLICATION ..: TMS
  AUTHOR .......: as
  CREATED ......: 24.8.2009 
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */
ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "MNP".

{Func/heartbeat.i}
{Mnp/mnp.i}
{Func/cparam2.i}
{fcgi_agent/xmlrpc/xmlrpc_client.i}
{Syst/tmsconst.i}
{Func/msreqfunc.i}
{Func/fsubstermreq.i}
{Func/log.i}
/* {mnptms_common.i}*/
{Syst/tmsconst.i}
{Func/orderfunc.i}
{Func/msisdn.i}
{Func/ordercancel.i}
{Func/msisdn_prefix.i}
{Func/orderchk.i}
{Func/add_lines_request.i}
{Func/fgettxt.i}
{Func/fixedlinefunc.i}
{Func/multitenantfunc.i}

DEFINE VARIABLE liLoop       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTime       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPause      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liPrevTime   AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldNow        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liNumMsgs    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcLine       AS CHARACTER NO-UNDO.
DEFINE VARIABLE llNagBeat    AS LOG       NO-UNDO INIT TRUE.
DEFINE VARIABLE liOrderQty   AS INTEGER   NO-UNDO.
DEFINE VARIABLE llOrderClosed AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liHandled AS INTEGER NO-UNDO. 
DEFINE VARIABLE liErrors AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeSMSStamp  AS DECIMAL NO-UNDO.

DEFINE STREAM sNagios.

DEF BUFFER messagebuf for mnpoperation.
DEF BUFFER bMNPProcess for MNPProcess.

FORM
   SKIP(1)
   " Loops..: " liLoop    FORMAT ">>>>>>>9" lcTime FORMAT "X(20)" SKIP
   " Handled: " liHandled FORMAT ">>>>>>>9" SKIP
   " Errors.: " liErrors  FORMAT ">>>>>>>9" 
   SKIP(1)
WITH
   CENTERED NO-LABELS TITLE " MNP MESSAGE RESPONSE HANDLER " WIDTH 45 ROW 8
FRAME frmMain.
PAUSE 0.

xmlrpc_initialize(FALSE).

DO WHILE TRUE
   ON ERROR UNDO, THROW:

   liLoop = liLoop + 1.

   DISP
      liLoop
      liHandled
      liErrors
      STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
   WITH FRAME frmMain.
   PAUSE 0.

   PUT SCREEN ROW 22 COL 1
      "Processing messages ...                                       ".
   
   FOR EACH MNPOperation NO-LOCK WHERE 
            MNPOperation.Sender = 1 AND
            MNPOperation.StatusCode = {&MNP_MSG_WAITING_RESPONSE_HANDLE}
          TENANT-WHERE TENANT-ID() >= 0 
       liNumMsgs = 1 to 1000 ON ERROR UNDO, THROW:

      PUT SCREEN ROW 2 COL 2 STRING(RECID(MNPOperation)).

      IF NOT fsetEffectiveTenantForAllDB(BUFFER-TENANT-NAME(MNPOperation)) THEN
          UNDO, THROW NEW Progress.Lang.AppError("Unable to change tenant. Abort!",1). 

      RUN pHandleQueue(RECID(MNPOperation)).
      
      RELEASE Order.
      RELEASE messagebuf.
      RELEASE mnpprocess.
            
   END.
   
   PUT SCREEN ROW 22 COL 1
      "Pausing 5 seconds...                                          ".
   PUT SCREEN ROW 21 COL 1
      "F8 TO QUIT, OTHER KEYS START HANDLER IMMEDIATELLY".

   ASSIGN liPause  = 5.
  
   IF llNagBeat THEN fKeepAlive("mnpresp:MNP Response Handler"). 
   
   READKEY PAUSE liPause.
   
   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
      xmlrpc_cleanup().
      xmlrpc_finalize().
      fCloseLog().
      QUIT.
   END.
 
END.

FUNCTION fErrorHandle RETURNS LOGICAL
(icDesc AS CHAR):

   ASSIGN
      MessageBuf.StatusCode = {&MNP_MSG_PARSING}
      MessageBuf.ErrorCode  = {&MNP_ERRORCODE_PARSE}
      MessageBuf.ErrorHandled = {&MNP_ERRORHANDLED_NO}
      MessageBuf.ErrorDesc = icDesc.

   liErrors = liErrors + 1.

END FUNCTION. 

FUNCTION fErrorParse RETURNS LOGICAL
(icDesc AS CHAR):

   ASSIGN
      MessageBuf.StatusCode = {&MNP_MSG_HANDLING}
      MessageBuf.ErrorCode = {&MNP_ERRORCODE_HANDLE}
      MessageBuf.ErrorHandled = {&MNP_ERRORHANDLED_NO}
      MessageBuf.ErrorDesc = icDesc.

   liErrors = liErrors + 1.

END FUNCTION. 

/* YDR-161 */
FUNCTION fArecExistCheck RETURNS LOGICAL
   (icResponseDesc AS CHAR,
    BUFFER ibMNPProcess FOR MNPProcess,
    OUTPUT ocRefCode AS CHAR):
   
   DEF VAR lcRefCode AS CHAR NO-UNDO. 
   DEF VAR lcContrato AS CHAR NO-UNDO. 
   DEF VAR liTextPos AS INT NO-UNDO. 
   DEF VAR lcMSISDN AS CHAR NO-UNDO. 
               
   liTextPos = index(icResponseDesc, "MSISDN ").
   IF liTextPos = 0 THEN RETURN FALSE.
   lcMSISDN = substring(icResponseDesc, liTextPos + 7, 9).
   IF NOT CAN-FIND(FIRST MNPSub WHERE
                         MNPSub.MNPSeq = ibMNPProcess.MNPSeq AND
                         MNPSub.CLI = lcMSISDN) THEN RETURN FALSE.
   
   liTextPos = index(icResponseDesc, "contrato ").
   IF liTextPos = 0 THEN RETURN FALSE.
   lcContrato = substring(icResponseDesc, liTextPos + 9, 11).
   IF NOT lcContrato BEGINS "005" THEN RETURN FALSE.
   IF ibMNPProcess.FormRequest NE lcContrato THEN RETURN FALSE.
   
   liTextPos = index(icResponseDesc, "referencia ").
   IF liTextPos = 0 THEN RETURN FALSE. 
   lcRefCode = substring(icResponseDesc, liTextPos + 11, 23).
   IF NOT lcRefCode BEGINS "005" THEN RETURN FALSE.
   IF ibMNPProcess.PortRequest NE lcRefCode AND
      CAN-FIND(FIRST MNPProcess WHERE
                     MNPProcess.PortRequest = lcRefCode) THEN RETURN FALSE.

   ocRefCode = lcRefCode.
   RETURN TRUE.

END FUNCTION. 

PROCEDURE pHandleQueue:      

   DEFINE INPUT PARAMETER pRecId AS RECID NO-UNDO.

   DEFINE VARIABLE lcPortCode AS CHAR    NO-UNDO.
   DEFINE VARIABLE ldActTS    AS DECIMAL NO-UNDO.
   DEFINE VARIABLE liLang     AS INTEGER NO-UNDO INIT 1.
   DEFINE VARIABLE lcRespXML  AS LONGCHAR NO-UNDO. 
   DEFINE VARIABLE lcResponseCode AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcResponseDesc AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcSolicitudStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcValidateFields AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcFields AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcXMLStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcPDFFolder AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcCancelProposalRefCode AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcSMS AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liMNPProCount AS INT NO-UNDO. 
   DEFINE VARIABLE liRespLength AS INT NO-UNDO. 
   DEFINE VARIABLE lcNewOper    AS CHAR NO-UNDO. 
   DEFINE VARIABLE llConfirm AS LOG NO-UNDO. 
   DEFINE VARIABLE lcOperName AS CHAR NO-UNDO. 
   
   FIND MessageBuf WHERE RECID(MessageBuf) = pRecId EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF ERROR-STATUS:ERROR OR LOCKED(MessageBuf) THEN 
      RETURN.

   FIND MNPProcess WHERE MNPProcess.MNPSeq = MessageBuf.MNPSeq EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF ERROR-STATUS:ERROR OR LOCKED MNPProcess THEN 
      RETURN.
   
   /* order should always exist with MNP IN processes */
   IF MNPProcess.MNPType = {&MNP_TYPE_IN} THEN 
   DO:
      FIND Order WHERE Order.Brand = Syst.Var:gcBrand AND Order.Orderid = MNPProcess.OrderID EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF LOCKED Order THEN 
         RETURN.

      IF NOT AVAIL Order THEN 
      DO:
         lcResponseDesc = "Order was not found".
         fErrorHandle(lcResponseDesc).
         fLogError(lcResponseDesc + ":" + MNPProcess.FormRequest). 
         LEAVE.
      END.

      FIND OrderCustomer OF Order WHERE OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
      IF NOT AVAIL OrderCustomer THEN 
      DO:
         lcResponseDesc = "Order customer was not found".
         fErrorHandle(lcResponseDesc).
         fLogError(lcResponseDesc + ":" + MNPProcess.FormRequest). 
         LEAVE.
      END.
   END.

   FIX-CODEPAGE(lcRespXML) = "UTF-8".
   COPY-LOB MNPOperation.XMLResponse TO lcRespXML.
   
   xmlrpc_cleanup().
   parse(lcRespXML).

   IF gi_xmlrpc_error NE ? AND gi_xmlrpc_error NE 0 THEN DO:
      IF INDEX(lcRespXML,"<fault>") > 0 THEN DO:
         ASSIGN
            MessageBuf.StatusCode = {&MNP_MSG_ADAPTER}
            MessageBuf.ErrorCode  = {&MNP_ERRORCODE_ADAPTER}
            MessageBuf.ErrorHandled = {&MNP_ERRORHANDLED_NO}
            MessageBuf.ErrorDesc = gc_xmlrpc_error.
         LEAVE.
      END.
      lcResponseDesc = gc_xmlrpc_error.
      fErrorParse(lcResponseDesc).
      fLogError(lcResponseDesc + ":" + MNPProcess.FormRequest). 
      LEAVE.
   END.

   /* top level struct should exist in every message type */
   IF validate_struct("response_toplevel_id","struct") = ? THEN DO:
      lcResponseDesc = gc_xmlrpc_error.
      fErrorParse(lcResponseDesc).
      fLogError(lcResponseDesc + ":" + MNPProcess.FormRequest). 
      LEAVE.
   END.
   
   lcXMLStruct = get_struct(response_toplevel_id,"0").
         
   /* common error handling */
   IF LOOKUP(MessageBuf.MessageType,
      "rechazarSolicitudAltaPortabilidadMovil," +
      "cancelarSolicitudBajaNumeracionMovil," + 
      "finalizarSolicitudMigracionNumeracionMovil," +
      "obtenerSolicitudNumeracionMigracionNumeracionMovil," +
      "obtenerSolicitudBajaNumeracionMovil," +
      "obtenerSolicitudCancelacionPortabilidadMovilIniciadaPorDonante," +
      "cancelarSolicitudAltaPortabilidadMovil," +
      "obtenerSolicitudAltaPortabilidadMovil," +
      "confirmarSolicitudAltaPortabilidadMovil," +
      "obtenerSolicitudMigracionNumeracion," +
      
      "crearSolicitudBajaNumeracionMovil," +
      
      "crearSolicitudIndividualAltaPortabilidadMovil," +
      "crearSolicitudNumeracionMigracionNumeracionMovil," + 

      "crearSolicitudCancelacionPortabilidadMovilIniciadaPorDonante," +
      
      "crearSolicitudMigracionNumeracionMovil") > 0 THEN DO:
      
      IF LOOKUP(MessageBuf.MessageType,
               "crearSolicitudIndividualAltaPortabilidadMovil," +
               "crearSolicitudNumeracionMigracionNumeracionMovil") > 0 THEN 
         lcValidateFields = "codigoRespuesta!,descripcion,campoErroneo,codigoReferencia,fechaVentanaCambio".
      ELSE IF MessageBuf.MessageType = "crearSolicitudBajaNumeracionMovil" THEN DO:
         lcValidateFields = "codigoRespuesta!,descripcion,campoErroneo,codigoReferencia!".
      END.
      ELSE IF MessageBuf.MessageType = "crearSolicitudCancelacionPortabilidadMovilIniciadaPorDonante" THEN DO:
         lcValidateFields = validate_struct(lcXMLStruct,"codigoRespuesta!,descripcion,campoErroneo,codigoSolicitudCancelacionPortabilidadMovilIniciadaPorDonante").
      END.
      ELSE IF MessageBuf.MessageType = "crearSolicitudMigracionNumeracionMovil" THEN
         lcValidateFields = "codigoRespuesta!,descripcion,campoErroneo,codigoReferencia!,fechaMaximaFinalizacion!".
      ELSE lcValidateFields = "codigoRespuesta!,descripcion,campoErroneo,solicitud".
      
      lcFields = validate_struct(lcXMLStruct,lcValidateFields).
      
      ASSIGN
         lcResponseCode = get_string(lcXMLStruct,"codigoRespuesta")
         
         lcPortCode = get_string(lcXMLStruct,"codigoReferencia")
         WHEN LOOKUP("codigoReferencia", lcFields) > 0
         
         ldActTS = MNPProcess.PortingTime
         ldActTS = get_timestamp(lcXMLStruct,"fechaVentanaCambio")
         WHEN LOOKUP("fechaVentanaCambio", lcFields) > 0
         
         lcResponseDesc = get_string(lcXMLStruct,"descripcion")
            WHEN LOOKUP("descripcion", lcFields) > 0
         
         lcCancelProposalRefCode = get_string(lcXMLStruct,"codigoSolicitudCancelacionPortabilidadMovilIniciadaPorDonante")
            WHEN LOOKUP("codigoSolicitudCancelacionPortabilidadMovilIniciadaPorDonante", lcFields) > 0.

      IF gi_xmlrpc_error NE ? AND gi_xmlrpc_error NE 0 THEN DO:
         lcResponseDesc = gc_xmlrpc_error.
         fErrorParse(lcResponseDesc).
         fLogError(lcResponseDesc). 
         LEAVE.
      END.
         
      IF lcResponseCode NE "0000 00000" THEN DO: 

         IF MessageBuf.MessageType = 
            "crearSolicitudIndividualAltaPortabilidadMovil" AND
            LOOKUP(lcResponseCode,"AREC ENUME,AREC FORMA,AREC EXIST,AREC CUPO1,AREC FMAYO") > 0
            THEN DO:
            
            IF lcResponseCode NE "AREC EXIST" OR 
               NOT fArecExistCheck(lcResponseDesc, 
                                   BUFFER MNPProcess,
                                   OUTPUT lcPortCode) THEN DO:
               ASSIGN
                  MNPProcess.UpdateTS = Func.Common:mMakeTS()
                  MNPProcess.StatusCode = {&MNP_ST_AREC}
                  MNPProcess.StatusReason = lcResponseCode
                  MessageBuf.StatusCode = {&MNP_MSG_HANDLED}
                  Order.MNPStatus = MNPProcess.StatusCode + 1.
                  fSetOrderStatus(Order.OrderId,"73").
                  /* Mark the timestamp */
                  fMarkOrderStamp(Order.OrderID,"Change",0.0).
                  liHandled = liHandled + 1.

               lcSMS = "".

               IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 THEN DO:
                  IF lcResponseCode EQ "RECH_IDENT" THEN lcSMS = "MNPIdentDirect".
                  ELSE lcSMS = "MNPReject".
               END.
               ELSE IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 THEN DO:
                  IF lcResponseCode EQ "AREC ENUME" THEN lcSMS = "MNPEnumePOS".
                  ELSE IF lcResponseCode EQ "RECH_ICCID" THEN lcSMS = "MNPIccidPOS".
               END.

               liMNPProCount = 0.
               
               /* Count no of MNP Process request sent */
               FOR EACH bMNPProcess NO-LOCK WHERE 
                        bMNPProcess.OrderID    = Order.OrderID  AND
                        bMNPProcess.MNPType    = {&MNP_TYPE_IN}:
                  liMNPProCount = liMNPProCount + 1. 
               END.         

               /* YDR-2147 */
               /* Resend MNP IN request to Nodal Center automatically 
                  In case received response is WITH rejected case by 
                  providing new operator code */
               IF lcResponseCode EQ "AREC ENUME" THEN DO: 
                  
                  ASSIGN liRespLength    = 0
                         lcNewOper       = "".

                  IF liMNPProCount = 1 AND 
                     lcResponseDesc MATCHES "El MSISDN * no pertenece al operador donante *, pertenece al operador *" THEN DO:
                     
                     ASSIGN 
                        liRespLength = LENGTH(lcResponseDesc) 
                        lcNewOper    = TRIM(SUBSTRING(lcResponseDesc,liRespLength - 2,liRespLength)).
                    
                     lcOperName = fGetMNPOperatorName(lcNewOper).

                     IF lcNewOper EQ "005" THEN .
                     ELSE IF lcOperName > "" THEN DO:

                        IF llDoEvent THEN DO:
                           DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
                           lhOrder = BUFFER Order:HANDLE.
                           RUN StarEventInitialize(lhOrder).
                           RUN StarEventSetOldBuffer(lhOrder).
                        END.
                        
                        Order.CurrOper = lcOperName.

                        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).
                        
                        fSetOrderStatus(Order.OrderId,"3").
                        
                        lcSMS = "".
                     END.
                     ELSE 
                        fLogError("New Operator code not OK: " + lcNewOper).
                  END.
               END.
             
               /* YDR-2183 */
               /* Relaunching the MNP IN request, if response xml from 
                  Nodal Center recieves error message AREC CUPO1.
                  This request has to be relaunched irrespecitve of 
                  Error message recieved in response xml */
               IF lcResponseCode EQ "AREC CUPO1" AND 
                  liMNPProCount = 1              THEN DO: 
                  
                  IF llDoEvent THEN DO:
                     DEFINE VARIABLE llhOrder AS HANDLE NO-UNDO.
                     llhOrder = BUFFER Order:HANDLE.
                     RUN StarEventInitialize(llhOrder).
                     RUN StarEventSetOldBuffer(llhOrder).
                  END.
                  
                  fSetOrderStatus(Order.OrderId,"3").

                  IF llDoEvent THEN 
                     RUN StarEventMakeModifyEvent(llhOrder).

                  lcSMS = "".

               END.

               IF lcSMS > "" THEN DO:
                  IF AVAIL OrderCustomer THEN
                     liLang = INT(OrderCustomer.Language) NO-ERROR.
                  ELSE liLang = 1.

                  fMNPCallAlarm(lcSMS,
                               0.0,
                               MNPProcess.FormRequest,
                               Order.CLI,
                               Order.CustNum,
                               liLang,
                               "800622111",
                               Order.OrderId).
               END.
               
               LEAVE.

            END.

         END.
         /* YOT-3421 CONF COEST case automatic handling */
         ELSE IF MessageBuf.MessageType =
            "confirmarSolicitudAltaPortabilidadMovil" AND
            lcResponseCode = "CONF COEST" AND
            MNPProcess.StatusCode = {&MNP_ST_ASOL} AND
            lcResponseDesc = 
            'El estado de la solicitud de alta de portabilidad movil debe ser "ASOL" para poder ser confirmada. El estado de la solicitud es "ACON"'
            THEN DO:
               RUN pHandleFromASOL2ACON.
               ASSIGN
                  MessageBuf.StatusCode = {&MNP_MSG_NC}
                  MessageBuf.ErrorCode = lcResponseCode
                  MessageBuf.ErrorHandled = {&MNP_ERRORHANDLED_NO}
                  MessageBuf.ErrorDesc = lcResponseDesc.
                  RELEASE MessageBuf.
               fLogError("Response code not OK: " + lcResponseCode). 
               LEAVE.
            END.

         /* YOT-3421 RECH REEST case automatic handling */
         ELSE IF MessageBuf.MessageType =
            "confirmarSolicitudAltaPortabilidadMovil" AND
            lcResponseCode = "RECH REEST" AND
            MNPProcess.StatusCode = {&MNP_ST_ASOL} AND
            lcResponseDesc = 
            'El estado de la solicitud de alta de portabilidad movil debe ser "ASOL" para poder ser rechazar. El estado de la solicitud es "AREC"'
            THEN DO:
               RUN pHandleFromASOL2AREC.
               ASSIGN
                  MessageBuf.StatusCode = {&MNP_MSG_NC}
                  MessageBuf.ErrorCode = lcResponseCode
                  MessageBuf.ErrorHandled = {&MNP_ERRORHANDLED_NO}
                  MessageBuf.ErrorDesc = lcResponseDesc.
                  RELEASE MessageBuf.
               fLogError("Response code not OK: " + lcResponseCode). 
               LEAVE.
            END.
        
         ELSE DO:
            ASSIGN
               MessageBuf.StatusCode = {&MNP_MSG_NC}
               MessageBuf.ErrorCode = lcResponseCode
               MessageBuf.ErrorHandled = {&MNP_ERRORHANDLED_NO}
               MessageBuf.ErrorDesc = lcResponseDesc.

            IF lcResponseDesc EQ "AREC CUPO4" THEN DO:
               fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_MNP_PENDING}).
               fMarkOrderStamp(Order.OrderID,"Change",0.0).
            END.

            fLogError("Response code not OK: " + lcResponseCode). 
            LEAVE.
         END.
      
      END.
   END.
   

   CASE MessageBuf.MessageType:

      WHEN "crearSolicitudIndividualAltaPortabilidadMovil" THEN DO:
         
         /* update subscription porting times */
         FOR EACH MNPSub EXCLUSIVE-LOCK WHERE
            MNPSub.MNPSeq = MNPProcess.MNPSeq AND
            MNPSub.PortingTime NE ldActTS:
            MNPSub.PortingTime = ldActTS.
         END.

         MNPProcess.Portingtime = ldActTS.
         
         /* Do not send SMS messages twice */
         IF MNPProcess.StatusCode NE {&MNP_ST_ASOL} THEN DO:

            liLang = INT(OrderCustomer.Language) NO-ERROR.

            fMNPCallAlarm("MNPConfTime",
                      ldActTS,
                      MNPProcess.FormRequest,
                      Order.CLI,
                      Order.CustNum,
                      liLang,
                      "800622600",
                      Order.OrderId).

         END.
                     
         FIND MNPDetails NO-LOCK WHERE
              MNPDetails.MNPSeq = MNPProcess.MNPSeq NO-ERROR.
         
         IF lcPortCode BEGINS "A05" THEN lcNewOper = "005".
         ELSE lcNewOper = SUBSTRING(lcPortCode,4,3).

         IF AVAIL MNPDetails AND
                  MNPDetails.DonorCode NE lcNewOper THEN
            lcOperName = fGetMNPOperatorName(lcNewOper).
         ELSE lcOperName = "".

         /* CIFM-52 */
         IF lcOperName > "" THEN DO:

            IF llDoEvent THEN RUN StarEventSetOldBuffer((BUFFER Order:HANDLE)).
            Order.CurrOper = lcOperName.
            IF llDoEvent THEN RUN StarEventMakeModifyEvent((BUFFER Order:HANDLE)).

            FIND CURRENT MNPDetails EXCLUSIVE-LOCK.
            IF llDoEvent THEN RUN StarEventSetOldBuffer((BUFFER MNPDetails:HANDLE)).
            ASSIGN
               MNPDetails.DonorCode = lcNewOper
               MNPProcess.opercode = lcNewOper.
            IF llDoEvent THEN RUN StarEventMakeModifyEvent((BUFFER MNPDetails:HANDLE)).
            RELEASE MNPDetails.
         END.
         
         ASSIGN
            MNPProcess.UpdateTS = Func.Common:mMakeTS()
            MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
            MNPProcess.PortRequest = lcPortCode
            MNPProcess.StatusCode = {&MNP_ST_ASOL}
            Order.MNPStatus = MNPProcess.StatusCode + 1.
            MessageBuf.StatusCode = 10.
      END.
      
      /* 2.12- Mobile portability activation requests detail */
      WHEN "obtenerSolicitudAltaPortabilidadMovil" THEN DO:
         /* no actions needed */ 
      END.
      
      /* 2.13- Cancellation detail by donor */
      WHEN "obtenerSolicitudCancelacionPortabilidadMovilIniciadaPorDonante" THEN DO:
         
         DEFINE VARIABLE lcAttachmentStruct AS CHARACTER NO-UNDO. 

         lcSolicitudStruct = get_struct(lcXMLStruct,"solicitud").
         lcAttachmentStruct = get_struct(lcSolicitudStruct, "acreditacionAbonado").
         validate_struct(lcAttachmentStruct,"referencia,content,contentType").
         
         IF gi_xmlrpc_error NE ? AND gi_xmlrpc_error NE 0 THEN DO:
            lcResponseDesc = gc_xmlrpc_error.
            fErrorParse(lcResponseDesc).
            fLogError(lcResponseDesc). 
            LEAVE.
         END.
         
         lcPDFFolder = fCParam("MNP","AttachmentFileFolder").
         IF lcPDFFolder = ? OR lcPDFFolder = "" THEN DO:
            lcResponseDesc = "PDF file folder not configured".
            fErrorHandle(lcResponseDesc).
            fLogError(lcResponseDesc + ":" + MNPProcess.PortRequest). 
            LEAVE.
         END.

         store_file(lcAttachmentStruct,"content", "lcPDFFolder" + MNPProcess.portrequest + "_" + string(messagebuf.createdts) + ".zip").
         
         IF gi_xmlrpc_error NE ? AND gi_xmlrpc_error NE 0 THEN DO:
            fErrorParse(gc_xmlrpc_error).
            fLogError(gc_xmlrpc_error). 
            LEAVE.
         END.

      END.
      
      WHEN "crearSolicitudBajaNumeracionMovil" THEN DO:

         FIND FIRST MNPSub WHERE
            MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK.
         
         FIND msisdn where
            msisdn.brand = Syst.Var:gcBrand and
            msisdn.cli = MNPSub.CLI AND
            msisdn.statuscode = {&MSISDN_ST_WAITING_RETURN} and
            msisdn.validto > Func.Common:mMakeTS() NO-LOCK NO-ERROR.

         IF NOT AVAIL msisdn THEN DO:
            lcResponseDesc = "MSISDN was not found or it is in wrong status".
            fErrorHandle(lcResponseDesc).
            fLogError(lcResponseDesc + ":" + MNPProcess.PortRequest). 
            LEAVE.
         END.
         
         ASSIGN
            MNPProcess.UpdateTS = Func.Common:mMakeTS()
            MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
            MNPProcess.PortRequest = lcPortCode
            MNPProcess.StatusCode = {&MNP_ST_BNOT}.
         
         fMakeMsidnHistoryTS(RECID(msisdn),MNPProcess.UpdateTS).
         Msisdn.StatusCode = {&MSISDN_ST_RETURN_NOTICE_SENT}.
         RELEASE Msisdn. 
         
      END.
      
      /* 2.9 - Cancel Numbering Termination request */
      WHEN "cancelarSolicitudBajaNumeracionMovil" THEN DO:

         FIND FIRST MNPSub WHERE
            MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK NO-ERROR.

         IF AVAIL MNPSub THEN DO:

            FIND FIRST MSISDN WHERE
                       MSISDN.Brand = Syst.Var:gcBrand AND
                       MSISDN.CLI = MNPSub.CLI
            USE-INDEX CLI NO-LOCK NO-ERROR.

            IF AVAIL MSISDN AND
                     MSISDN.StatusCode = {&MSISDN_ST_RETURN_NOTICE_SENT} AND
                     MSISDN.ValidTo > 99999999 THEN DO:

               fMakeMsidnHistory(recid(msisdn)).
               msisdn.statuscode = {&MSISDN_ST_WAITING_RETURN}. 
               release msisdn.
            END.

            /* Now handle pending reactivation request */
            FIND FIRST MsRequest WHERE
                       MsRequest.MsSeq   = MNPSub.MsSeq AND
                       MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_REACTIVATION} AND
                       MsRequest.ReqStatus = {&REQUEST_STATUS_CONFIRMATION_PENDING}
                 NO-LOCK NO-ERROR.
            IF AVAIL MsRequest THEN fReqStatus(0,"").
         END.

         ASSIGN
            MNPProcess.UpdateTS = Func.Common:mMakeTS()
            MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
            MNPProcess.StatusCode = {&MNP_ST_BCAN}.
      END.
      
      WHEN "obtenerSolicitudBajaNumeracionMovil" THEN DO:
         /* no actions required */
      END.
     
      /* NUMBER MIGRATION OPERATIONS STARTS */
      
      /* 2.5 - Numbering query for Mobile numbering migration request */
      WHEN "crearSolicitudNumeracionMigracionNumeracionMovil" THEN DO:
         
         ASSIGN
            MNPProcess.UpdateTS = Func.Common:mMakeTS()
            MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
            MNPProcess.PortRequest = lcPortCode
            MNPProcess.StatusCode = {&MNP_ST_NENV}.
      END.
      
      /* 2.3 - Mobile numbering migration request */
      WHEN "crearSolicitudMigracionNumeracionMovil" THEN DO:
      
         ASSIGN
            MNPProcess.UpdateTS = Func.Common:mMakeTS()
            MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
            MNPProcess.PortRequest = lcPortCode
            MNPProcess.StatusCode = {&MNP_ST_MENV}.
      END.
      
      /* 2.4 - Finalize Mobile numbering migration request */
      WHEN "finalizarSolicitudMigracionNumeracionMovil" THEN DO:
         
         ASSIGN
            MNPProcess.UpdateTS = Func.Common:mMakeTS()
            MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
            MNPProcess.StatusCode = {&MNP_ST_MFIN}.
      END.
      
      /* 2.14 - Mobile numbering migration request detail */
      WHEN "obtenerSolicitudMigracionNumeracion" THEN DO:
         /* no actions needed */   
      END.

      /* 2.15 - Numbering query for Mobile numbering migration request detail */
      WHEN "obtenerSolicitudNumeracionMigracionNumeracionMovil" THEN DO:
         /* no actions needed */   
      END.

      /* NUMBER MIGRATION OPERATIONS ENDS */


      /* 1.16 - Cancellation request by donor */
      /* deprecated */
      WHEN "crearSolicitudCancelacionPortabilidadMovilIniciadaPorDonante" THEN DO:

         FIND MNPCancelProposal WHERE
              MNPCancelProposal.MNPSeq = MNPProcess.MNPSeq
              EXCLUSIVE-LOCK NO-ERROR.
         
         IF NOT AVAIL MNPCancelProposal THEN DO:
            lcResponseDesc = "Cancel proposal was not found".
            fErrorHandle(lcResponseDesc).
            fLogError(lcResponseDesc + ":" + MNPProcess.PortRequest).
            LEAVE.
         END.
         
         ASSIGN     
            MNPCancelProposal.ReferenceCode = lcCancelProposalRefCode.
         
         RELEASE MNPCancelProposal.
            
         FOR EACH MNPSub WHERE
            MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK:

            FIND MobSub WHERE
               MobSub.MSSeq = MNPSub.MsSeq NO-LOCK NO-ERROR.
            
            IF AVAIL MobSub THEN 
               FIND Customer WHERE
                    Customer.Custnum = MobSub.Custnum 
                    NO-LOCK NO-ERROR.

            fMNPCallAlarm("MNPCancelPropose",
                      Func.Common:mMakeTS(),
                      MNPProcess.FormRequest,
                      MNPSub.CLI,
                      (IF AVAIL MobSub THEN MobSub.Custnum ELSE 0),
                      (IF AVAIL Customer THEN Customer.Language ELSE 1),
                      "800622600",
                      Order.OrderId).
         END.

      END.
      
      /* 1.10 Confirm portability activation request query */
      WHEN "confirmarSolicitudAltaPortabilidadMovil" THEN DO:

         /* Multiple MNP out */
         FIND MNPSub NO-LOCK WHERE
              MNPSub.MNPSeq = MNPProcess.MNPSeq NO-ERROR.

         /* In case of multiple MNP OUT, additioanal confirmation is required from NC side */
         IF AMBIGUOUS MNPSub THEN DO:
            
            llConfirm = FALSE.
            
            FOR EACH MNPSub NO-LOCK WHERE
                     MNPSub.MNPSeq = MNPProcess.MNPSeq:
               IF MNPSub.StatusReason = "" THEN DO:
                  messagebuf.StatusCode = {&MNP_MSG_WAITING_CONFIRM}.
                  MNPProcess.StateFlag = {&MNP_STATEFLAG_WAITING_CONFIRM}.
                  RETURN.
               END.
               IF MNPSub.StatusReason EQ "CONFIRM" THEN DO:
                  llConfirm = TRUE.
                  LEAVE.
               END.
            END.
         END.
         ELSE llConfirm = TRUE.
   
         IF llConfirm THEN RUN pHandleFromASOL2ACON.
         ELSE RUN pHandleFromASOL2AREC.
      END.
      
      /* 1.11 Reject portability activation request */
      WHEN "rechazarSolicitudAltaPortabilidadMovil" THEN DO:
         RUN pHandleFromASOL2AREC.
      END.
      
      /* 1.12 - Cancel Portability activation request */
      /* Rules
         1) Cancel possible ongoing subscription creation request
         2) Cancel any pending SMS messages
         3) Release POS SIM 
      */
      WHEN "cancelarSolicitudAltaPortabilidadMovil" THEN DO:
         
         /* ACAN cannot come after porting */ 
         IF Order.OrderType = 3 THEN DO:
            FIND FIRST MsRequest WHERE
                       MsRequest.MsSeq      = Order.MsSeq AND
                       MsRequest.ReqType    = ({&REQTYPE_SUBSCRIPTION_REACTIVATION}) AND
                       MsRequest.ReqStatus  = ({&REQUEST_STATUS_NEW}) AND
                       MsRequest.ReqIparam1 = Order.OrderId
                 NO-LOCK NO-ERROR.
            IF AVAIL MsRequest THEN DO:
               fReqStatus(4,"Cancelled MNP Process").
            END. /* IF AVAIL MsRequest THEN DO: */
            /* ongoing reactivation requests should not exist */
            ELSE DO:
               FIND FIRST MsRequest WHERE
                          MsRequest.MsSeq = Order.MsSeq AND
                          MsRequest.ReqType = ({&REQTYPE_SUBSCRIPTION_REACTIVATION}) AND
                          MsRequest.ReqStatus NE ({&REQUEST_STATUS_NEW}) AND
                          MsRequest.ReqIparam1 = Order.OrderId
                    NO-LOCK NO-ERROR.
               IF AVAIL MsRequest THEN DO:
                  lcResponseDesc = "Cancellation failed, ongoing reactivation request".
                  fErrorHandle(lcResponseDesc).
                  fLogError(lcResponseDesc + ":" + MNPProcess.PortRequest).
                  LEAVE.
               END. /* IF AVAIL MsRequest THEN DO: */
            END. /* ELSE DO: */
               
            /* Marked SIM status to LOST (previous state) */
            FIND SIM WHERE
                 SIM.ICC = Order.ICC AND
                 SIM.SimStat = 4 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL SIM THEN SIM.SimStat = 7.
            RELEASE SIM.

         END. /* IF Order.OrderType = 3 THEN DO: */
         ELSE DO:
            FIND FIRST MsRequest WHERE
                       MsRequest.MsSeq     = Order.MsSeq AND
                       MsRequest.ReqType   = ({&REQTYPE_SUBSCRIPTION_CREATE}) AND
                       MsRequest.ReqStatus = ({&REQUEST_STATUS_NEW})
                 NO-LOCK NO-ERROR.
            IF AVAIL MsRequest THEN
               fReqStatus(4,"Cancelled MNP Process").

            /* ongoing subscription requests should not exist */
            ELSE DO:
               FIND FIRST MsRequest WHERE
                          MsRequest.MsSeq = Order.MsSeq AND
                          MsRequest.ReqType = ({&REQTYPE_SUBSCRIPTION_CREATE}) AND
                          MsRequest.ReqStatus NE ({&REQUEST_STATUS_CANCELLED})
                    NO-LOCK NO-ERROR.
               IF AVAIL MsRequest THEN DO:
                  lcResponseDesc = "Cancellation failed, ongoing subscription request".
                  fErrorHandle(lcResponseDesc).
                  fLogError(lcResponseDesc + ":" + MNPProcess.PortRequest).
                  LEAVE.
               END. /* IF AVAIL MsRequest THEN DO: */
            END. /* ELSE DO: */
         END. /* ELSE DO: */
        
         /* Cancel pending SMS messages */
         FOR EACH CallAlarm WHERE
                  CallAlarm.Brand = Syst.Var:gcBrand AND
                  CallAlarm.CLI = Order.CLI AND
                  CallAlarm.DeliStat = 1 AND
                  CallAlarm.CreditType = {&SMSTYPE_MNP} EXCLUSIVE-LOCK:
             CallAlarm.DeliStat = 4. /* CANCELLED */
         END.
                  
         llOrderClosed = fSetOrderStatus(Order.OrderId,"7").
         
         IF llOrderClosed THEN DO:
            fMarkOrderStamp(Order.OrderID,"Close",0.0).

            /* If mainline MNP order is cancelled then release associated 
               additional lines and extra lines if avilable */

            fActionOnAdditionalLines (OrderCustomer.CustIdType,
                                      OrderCustomer.CustID,
                                      Order.CLIType,
                                      FALSE,
                                      "RELEASE"). 

            IF fCLITypeIsMainLine(Order.CLIType) THEN 
               fActionOnExtraLineOrders(Order.OrderId,    /* Main line Order Id  */
                                        "RELEASE").       /* Action              */

         END.

         IF NOT llOrderClosed THEN fLogError("Order closing failed: " + STRING(Order.OrderId)).
         
         /* YDR-16 */
         IF llOrderClosed AND LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 THEN DO:
           
            FIND SIM WHERE
               SIM.ICC = Order.ICC AND
               SIM.SimStat = 4 NO-LOCK NO-ERROR.
            IF AVAIL SIM AND SIM.Stock = "RETAILER" THEN
               fReleaseSIM(Order.OrderID).
         END.
         
         /* YOT-451 */
         lcSMS = "".
         IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 THEN DO:
            /* Release SIM if not send to LO YDR-1825*/
            IF llOrderClosed AND
            Order.OrderType EQ {&ORDER_TYPE_MNP} AND
            Order.Logistics = "" THEN DO:
               FIND SIM WHERE
                    SIM.ICC = Order.ICC AND
                    SIM.SimStat = 20 EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAIL SIM THEN SIM.SIMStat = {&SIM_SIMSTAT_AVAILABLE}.
                  RELEASE SIM.
            END.
            
            IF Order.OrderChannel EQ "self" OR
               Order.OrderChannel EQ "fusion_self" THEN
               lcSMS = (IF MNPProcess.StatusCode = {&MNP_ST_ACON}
                        THEN "MNPCanAfterConf"
                        ELSE "MNPCanBeforeConf").
            ELSE lcSMS = "MNPInCancel".
         END.
         ELSE IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0
            THEN lcSMS = "MNPInCancelPOS".

         IF lcSMS > "" THEN DO:
            
            liLang = INT(OrderCustomer.Language) NO-ERROR.
            
            fMNPCallAlarm(
                lcSMS,
                Func.Common:mMakeTS(),
                MNPProcess.FormRequest,
                Order.CLI,
                Order.CustNum,
                liLang,
                "800622600",
                Order.OrderId).
         END.
      
         FOR EACH MNPCancelProposal EXCLUSIVE-LOCK WHERE
            MNPCancelProposal.MnpSeq = MNPProcess.MNPSeq AND
            MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_NEW}:
            MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_CANCELLED}.
         END.
      
         ASSIGN         
            MNPProcess.UpdateTS = Func.Common:mMakeTS()
            MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
            MNPProcess.StatusCode = {&MNP_ST_ACAN}
            Order.MNPStatus = MNPProcess.StatusCode + 1.

         FIND CURRENT Order NO-LOCK.

         RUN Mc/cancelorder.p(Order.OrderID,TRUE).

         /* YDR-70 */
         IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 THEN DO:
            
            FIND OrderAccessory WHERE
                 OrderAccessory.Brand = Syst.Var:gcBrand AND
                 OrderAccessory.OrderId = Order.OrderId AND
                 OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE})
            EXCLUSIVE-LOCK NO-ERROR.

            IF AVAIL OrderAccessory AND OrderAccessory.IMEI NE "" THEN DO:
               
               IF llDoEvent THEN DO:
                  DEFINE VARIABLE lhOrderAccessory AS HANDLE NO-UNDO.
                  lhOrderAccessory = BUFFER OrderAccessory:HANDLE.
                  RUN StarEventInitialize(lhOrderAccessory).
                  RUN StarEventSetOldBuffer(lhOrderAccessory).
               END.
               
               OrderAccessory.IMEIStatus = ({&IMEI_STATUS_TO_BE_RELEASED}).

               IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderAccessory).

               RELEASE OrderAccessory.
            END.
         END.

      END.

      OTHERWISE RETURN.
   END.
   
   IF MessageBuf.StatusCode < 900 THEN ASSIGN
      MessageBuf.StatusCode = {&MNP_MSG_HANDLED}
      liHandled = liHandled + 1.
   
END.

PROCEDURE pHandleFromASOL2ACON:
   /* For normal errors and for special errors
   Normal: 1.10 Confirm portability activation request query.
   Another case for this procedure: YOT-3421 */
   
   /* ACON cannot come after NEW, AREC or APOR */ 
   /*
   IF LOOKUP(STRING(MNPProcess.StatusCode),"0,4,7") > 0 THEN DO: 
      fLogError("Statuscode is wrong " + STRING(MNPProcess.StatusCode) + ": " + MNPProcess.PortRequest). 
      MessageBuf.StatusCode = {&MNP_MSG_HANDLING}.
      LEAVE.
   END. 
   */

   DEFINE VARIABLE liTermReqId AS INTEGER NO-UNDO. 
   /* create termination request(s) */
   FOR EACH MNPSub WHERE
      MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK:

      IF MNPSub.StatusReason NE "" AND
         MNPSub.StatusReason NE "CONFIRM" THEN NEXT.

      FIND MobSub WHERE
           MobSub.MsSeq = MNPSub.MsSeq AND
           MobSub.CLI = MNPSub.CLI NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN NEXT.

      DEFINE VARIABLE liMsisdnStat AS INTEGER NO-UNDO.
      DEFINE VARIABLE liSimStat AS INTEGER NO-UNDO.
      DEFINE VARIABLE liQuarTime AS INTEGER NO-UNDO.
      DEFINE VARIABLE llPenalty AS LOGICAL NO-UNDO. 
      DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE lcTermType AS CHARACTER NO-UNDO.
      DEF VAR ldaMNPDate AS DATE NO-UNDO.
            
      fInitialiseValues(2, 
                        fIsYoigoCLI(MNPSub.CLI),
                        fIsMasmovilCLI(MNPSub.CLI),
                        output liMsisdnStat,
                        output liSimStat,
                        output liQuarTime).

      llPenalty = fIsPenalty(2, MNPSub.MsSeq).
      
      /* YOT-3421: Cancel pending Termination Request and create new one */
      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.MsSeq = MNPSub.MsSeq AND
                 MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                 MsRequest.ReqStatus = {&REQUEST_STATUS_NEW} AND
                 MsRequest.ActStamp > MNPSub.PortingTime NO-ERROR.
      IF AVAIL MsRequest THEN
         fReqStatus(4,"Cancelled MNP Process due to MNP OUT").

      IF fIsConvergenceTariff(MobSub.CLIType) 
         THEN lcTermType = {&TERMINATION_TYPE_PARTIAL}.
      ELSE lcTermType = {&TERMINATION_TYPE_FULL}.

      liTermReqId = fTerminationRequest(MnpSub.MSSeq,
                          MNPSub.PortingTime,
                          liMsisdnStat,
                          liSimStat,
                          liQuarTime,
                          INT(llPenalty),
                          MNPSub.NRN,
                          STRING(2),
                          "5", /* automatic script*/
                          Syst.Var:katun,
                          0, /* orig. request */
                          lcTermType,
                          OUTPUT ocResult).

      IF liTermReqId = 0 THEN
         fErrorHandle(ocResult). 
      ELSE DO:

         Func.Common:mTS2Date(MNPSub.PortingTime, OUTPUT ldaMNPDate).

         fAdditionalLineSTC(liTermReqId,
                            Func.Common:mMake2DT(ldaMNPDate, 0),
                            "MNP").
      END.
   END.
   
   ASSIGN
      MNPProcess.UpdateTS = Func.Common:mMakeTS()
      MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
      MNPProcess.StatusCode = {&MNP_ST_ACON}.
   
END PROCEDURE. /* PROCEDURE pHandleFromASOL2ACON: */

PROCEDURE pHandleFromASOL2AREC:
   /* Release possible number return process that is on hold.
   Another case for this procedure: YOT-3421 */

   DEFINE VARIABLE lcMNPSMSText AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLang     AS INTEGER NO-UNDO INIT 1.

   DEF BUFFER bMNPSub FOR MNPSub.
   DEF BUFFER bMNPProcess FOR MNPProcess.
   
   /* Do not process duplicate internal MNP IN rejections */
   IF MNPProcess.MNPType EQ 1 THEN RETURN.

   FOR EACH MNPSub WHERE
            MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK:
      FOR EACH bMNPSub WHERE
               bMNPSub.CLI = MNPSub.CLI NO-LOCK,
         FIRST bMNPProcess WHERE
            bMNPProcess.MNPSeq = bMNPSub.MNPSeq AND
            bMNPProcess.MNPType = {&MNP_TYPE_TERMINATION} AND
            bMNPProcess.StatusCode = {&MNP_ST_BDET} EXCLUSIVE-LOCK:

         ASSIGN
            bMNPProcess.UpdateTS = Func.Common:mMakeTS()
            bMNPProcess.StatusCode = {&MNP_ST_BNOT}.
         RELEASE bMNPProcess.
      END.

      /* YOT-2088 - Now move Retention order into correct queue */
      FIND MobSub WHERE
           MobSub.MSSeq = MNPSub.MsSeq NO-LOCK NO-ERROR.
           
      IF AVAIL MobSub THEN DO:
         fRetention(MobSub.MsSeq,
                    liLang,
                    MobSub.Custnum,
                    MNPProcess.FormRequest,
                    MNPSub.CLI).
      END. /* IF AVAIL MobSub THEN DO: */
   END.
   
   ASSIGN
      MNPProcess.UpdateTS = Func.Common:mMakeTS()
      MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
      MNPProcess.StatusCode = {&MNP_ST_AREC}.

END PROCEDURE. /* PROCEDURE pHandleFromASOL2AREC: */
