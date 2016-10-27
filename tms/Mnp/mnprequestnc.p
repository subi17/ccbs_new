/* ----------------------------------------------------------------------
  MODULE .......: mnprequestnc.p
  TASK .........: Create new mnp in process and portability request
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 08/2009
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{mnp.i}
{mnpmessages.i}
{tmsconst.i}
{date.i}
{fbundle.i}

DEFINE INPUT PARAMETER iiOrderId AS INTEGER NO-UNDO.

DEFINE VARIABLE lcXML AS CHAR NO-UNDO.
DEFINE VARIABLE liSeq         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcFormRequest AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldChgDate AS DATE NO-UNDO. 
DEFINE VARIABLE ldeToday AS DEC NO-UNDO. 
DEFINE VARIABLE ldeChgStamp AS DECIMAL NO-UNDO.
DEFINE VARIABLE lcProduct AS CHAR NO-UNDO. 
DEFINE VARIABLE lcTariffType AS CHAR NO-UNDO. 

&SCOPED-DEFINE COMPANY_NAME_LIMIT 64   /* Name length limitation send to Nodo Central */

FIND Order NO-LOCK WHERE
     Order.Brand   = gcBrand AND
     Order.OrderId = iiOrderId NO-ERROR.

IF NOT AVAIL Order THEN RETURN ("ERROR: Order not found " + STRING(iiOrderId)).

FIND OrderCustomer OF Order WHERE
   OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

IF NOT AVAIL OrderCustomer THEN RETURN ("ERROR: OrderCustomer not found " + STRING(iiOrderId)).

FIND FIRST MNPOperator WHERE 
           MNPOperator.Brand = gcBrand AND
           MNPOperator.OperName = STRING(order.curroper) AND
           MNPOperator.Active = True
NO-LOCK NO-ERROR.

IF NOT AVAIL MNPOperator THEN
   FIND FIRST MNPOperator WHERE 
              MNPOperator.Brand = gcBrand AND
              MNPOperator.OperName = STRING(order.curroper) AND
              MNPOperator.Active = False
   NO-LOCK NO-ERROR.

ldeToday = fDate2TS(TODAY).

IF AVAIL MNPOperator THEN DO:

   FOR EACH mnpoperation NO-LOCK where
            mnpoperation.CreatedTS > ldeToday and
            mnpoperation.errorcode = "AREC CUPO4" USE-INDEX CreatedTS,
      FIRST MNPProcess NO-LOCK WHERE
            MNPProcess.MNPSeq = mnpoperation.mnpseq AND
            MNPProcess.MNPType = {&MNP_TYPE_IN} AND
            MNPProcess.OperCode = MNPOperator.OperCode:
       RETURN "ERROR:AREC CUPO4".
   END.

END.

ASSIGN
   liSeq         = NEXT-VALUE(M2MSeq)
   lcFormRequest = "005" + STRING(liSeq,"99999999").

/* mark old rejected processes as closed */
FOR EACH MNPProcess WHERE
   MNPProcess.OrderId = Order.OrderId AND
   MNPProcess.MNPType = {&MNP_TYPE_IN} AND
   MNPProcess.StatusCode = {&MNP_ST_AREC} EXCLUSIVE-LOCK:
   ASSIGN
      MNPProcess.UpdateTS = fMakeTS()
      MNPProcess.StatusCode = {&MNP_ST_AREC_CLOSED}. /* closed */
END.

/* Close open processes YOT-1423 */
FOR EACH MNPProcess where
         MNPProcess.OrderId = Order.OrderID and
         MNPProcess.MNPType = {&MNP_TYPE_IN} AND
         MNPProcess.StatusCode = {&MNP_ST_NEW} EXCLUSIVE-LOCK:
   FIND FIRST MNPOperation where
              MNPOperation.mnpseq = MNPProcess.mnpseq and
              MNPOperation.messagetype =
               "CrearSolicitudIndividualAltaPortabilidadMovil" and
              MNPOperation.statuscode = {&MNP_MSG_NC}
   NO-LOCK NO-ERROR.
   IF AVAIL MNPOperation THEN ASSIGN
      MNPProcess.UpdateTS = fMakeTS()
      MNPProcess.StatusCode = {&MNP_ST_AREC_CLOSED}
      MNPProcess.StatusReason = MNPOperation.ErrorCode.
END.
   
FIND FIRST OrderAccessory NO-LOCK WHERE
           OrderAccessory.Brand = gcBrand AND
           OrderAccessory.OrderId = Order.OrderID AND
           OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-ERROR.
IF AVAIL OrderAccessory THEN
   lcProduct = "T".
ELSE
   lcProduct = "S".

lcTariffType = fGetDataBundleInOrderAction(Order.OrderId,
                                           Order.CLIType).
IF lcTariffType = "" THEN
   lcTariffType = Order.CLIType.

ldChgDate = fMNPChangeWindowDate(   /* Count min porting date */
            fMakeTS(),
            /* todo: can be removed after deplo */
            (IF Order.OrderType = 3 AND
                Order.OrderChannel NE "inversa"
             THEN "POS"
             ELSE Order.OrderChannel),
            OrderCustomer.Region,
            lcProduct,
            lcTariffType).

IF Order.PortingDate <> ? THEN
   IF ldChgDate < Order.PortingDate THEN /* Porting date in the future, use that */
      ldChgDate = Order.PortingDate.
ldeChgStamp = fMake2Dt(ldChgDate,7200).

CREATE MNPProcess.
ASSIGN 
   MNPProcess.CreatedTS   = fMakeTS()
   MNPProcess.MNPSeq      = next-value(m2mrequest)
   MNPProcess.OrderId     = Order.OrderId
   MNPProcess.FormRequest = lcFormRequest
   MNPProcess.StatusCode  = {&MNP_ST_NEW}
   MNPProcess.Brand       = gcBrand
   MNPProcess.MNPType     = {&MNP_TYPE_IN}
   MNPProcess.UserCode    = katun
   MNPProcess.UpdateTS    = MNPProcess.CreatedTS
   MNPProcess.OperCode    = MNPOperator.OperCode WHEN AVAIL MNPOperator
   MNPProcess.PortingTime = ldeChgStamp.

CREATE MNPDetails.
ASSIGN
   MNPDetails.MNPSeq = MNPProcess.MNPSeq
   MNPDetails.CustId = OrderCustomer.CustId
   MNPDetails.CustIdType = OrderCustomer.CustIdType
   MNPDetails.FirstName = OrderCustomer.FirstName
   MNPDetails.Surname1 = OrderCustomer.SurName1
   MNPDetails.Surname2 = OrderCustomer.SurName2
   MNPDetails.CompanyName = OrderCustomer.Company 
   MNPDetails.RequestedTS = Order.CrStamp 
   MNPDetails.ReceptorCode = "005"
   MNPDetails.ReceptorNRN = "741111" 
   MNPDetails.DonorCode = MNPOperator.OperCode WHEN AVAIL MNPOperator
   MNPDetails.Nationality = OrderCustomer.Nationality.

CREATE MNPSub.
ASSIGN
   MNPSub.MNPSeq = MNPProcess.MNPSeq
   MNPSub.CLI    = Order.CLI
   MNPSub.ICC    = Order.OldIcc WHEN LENGTH(Order.OldICC) > 6
   MNPSub.MsSeq  = Order.MsSeq
   MNPSub.PortingTime = MNPProcess.PortingTime.

RUN pCreatePortabilityMessageXML(OUTPUT lcXML).

IF lcXml NE "" THEN DO:
   fMNPOperation(MNPProcess.MNPSeq, lcXML, 
   "CrearSolicitudIndividualAltaPortabilidadMovil").
END.

xmlrpc_finalize().
RETURN "".

PROCEDURE pCreatePortabilityMessageXML:
   
   DEF OUTPUT PARAM ocRequest AS LONGCHAR NO-UNDO. 
   
   DEFINE VARIABLE lcReqStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcAbonado AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcDocumentoIdentification AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDatosPersonales AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcOper AS CHARACTER NO-UNDO. 
   
   IF AVAIL MNPOperator THEN lcOper = MNPOperator.OperCode.
   ELSE lcOper = Order.CurrOper.

   ASSIGN
      MNPSub.PortingTime = ldeChgStamp
      MNPProcess.PortingTime = ldeChgStamp.

   lcReqStruct = add_struct(param_toplevel_id, "").
   add_timestamp(lcReqStruct, "fechaSolicitudPorAbonado", Order.CrStamp).
   add_string(lcReqStruct, "codigoOperadorDonante", lcOper).
   add_string(lcReqStruct, "codigoOperadorReceptor", "005").

   lcAbonado = add_struct(lcReqStruct,"abonado").

   lcDocumentoIdentification = add_struct(lcAbonado, "documentoIdentificacion").
   add_string(lcDocumentoIdentification, "tipo", 
      (IF OrderCustomer.CustIdType = "PassPort" THEN "PAS"
       ELSE OrderCustomer.CustIdType)).
   add_string(lcDocumentoIdentification, "documento", CAPS(OrderCustomer.CustId)).

   lcDatosPersonales = add_struct(lcAbonado, "datosPersonales").

   IF OrderCustomer.CustIdType = "CIF" THEN DO:
      IF LENGTH(OrderCustomer.Company) > {&COMPANY_NAME_LIMIT} THEN  /* YOT-4107 Max chars 64 */
      add_string(lcDatosPersonales, "razonSocial", 
                 SUBSTRING(OrderCustomer.Company,1,{&COMPANY_NAME_LIMIT})).
      ELSE add_string(lcDatosPersonales, "razonSocial", OrderCustomer.Company).
   END.
   ELSE DO:
      IF OrderCustomer.CustIdType NE "NIF" THEN
         add_string(lcDatosPersonales, "nacionalidad", OrderCustomer.Nationality).
      add_string(lcDatosPersonales, "nombre", OrderCustomer.FirstName).
      add_string(lcDatosPersonales, "primerApellido", OrderCustomer.Surname1).
      IF OrderCustomer.SurName2 NE "" THEN 
         add_string(lcDatosPersonales, "segundoApellido", OrderCustomer.Surname2).
   END.

   add_string(lcReqStruct, "codigoContrato", MNPProcess.FormRequest).
   add_string(lcReqStruct, "NRNReceptor", "741111").
   add_timestamp(lcReqStruct, "fechaVentanaCambio", MNPProcess.PortingTime).
  
   IF LENGTH(Order.OldICC) > 6 THEN
      add_string(lcReqStruct, "ICCID", substr(Order.OldICC,1,19)).
   
   add_string(lcReqStruct, "MSISDN", Order.CLI).

   xmlrpc_initialize(FALSE).
   ocRequest = serialize_rpc_call("mnp.crearSolicitudIndividualAltaPortabilidadMovil").
   xmlrpc_cleanup().

END PROCEDURE.
