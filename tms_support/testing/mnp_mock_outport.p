/* ----------------------------------------------------------------------
MODULE .......: mnp_mock_outport.p
TASK .........: Send MNP requests into Donor 
APPLICATION ..: TMS
AUTHOR .......: Janne Tourunen 
CREATED ......: 03.10.2012
CHANGED ......:
Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN
   katun = "Qvantel"
      gcBrand = "1".
{Func/cparam2.i}
{xmlrpc/xmlrpc_client.i}
{Syst/tmsconst.i}
{Func/timestamp.i}


DEFINE VARIABLE ocResponse AS CHAR NO-UNDO.

DEFINE VARIABLE lcCLI AS CHAR NO-UNDO.
DEFINE VARIABLE liCustNum AS INT NO-UNDO FORMAT ">>>>>>>>9".
DEFINE VARIABLE lcCliType AS CHAR NO-UNDO.
DEFINE VARIABLE ldePortTime AS DEC NO-UNDO. 
DEFINE VARIABLE ldaNowDay AS DATE NO-UNDO.
DEFINE VARIABLE ldeCreationTS AS DEC NO-UNDO.
DEFINE VARIABLE liCurHour AS INT NO-UNDO.
DEFINE VARIABLE liMorning AS INT NO-UNDO.
DEFINE VARIABLE liMidday AS INT NO-UNDO.
DEFINE VARIABLE liEvening AS INT NO-UNDO.
DEFINE VARIABLE lcFormRequest AS CHAR NO-UNDO.
DEFINE VARIABLE liSeq AS INT NO-UNDO.

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
DEFINE VARIABLE lcvalue AS CHAR NO-UNDO.
DEFINE VARIABLE lcList AS CHAR NO-UNDO.


lcConURL = fCParam("URL","UrlMnpMock").
IF lcConURL = ? OR lcConURL = "" THEN DO:
   ocResponse = "ERROR:Connection URL not defined".
      RETURN.
END.

FORM
    "MSISDN.........:" lcCLI FORMAT "x(10)" 
    HELP "Enter MSISDN which to be outported" SKIP
    "Customer Number:" liCustNum FORMAT ">>>>>>>>>>"
    HELP "Enter customer which to be outported with all MSISDN" SKIP
    WITH OVERLAY ROW 4 centered
    TITLE " MNP MOCK FOR OUTPORTING "
    NO-LABELS
    FRAME lis.

RUN pUserInput.
/* IF  User Wanted TO Cancel this Change TRANSACTION */
IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
KEYLABEL(lastkey) = "F4" THEN UNDO, RETURN.

RUN pAddRequestStructElement(lcCLI, liCustNum, lcList).

MESSAGE "Result" ocResponse VIEW-AS ALERT-BOX.
DEF VAR ufkey  AS LOG NO-UNDO INIT TRUE.

PROCEDURE pUserInput:

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      DISP lcCLI liCustNum WITH FRAME lis.

      UPDATE lcCLI liCustNum WITH FRAME lis EDITING:

         IF ufkey THEN DO:
            ASSIGN ehto = 9. RUN Syst/ufkey.p.
            ufkey = false.
         END.

         READKEY.

         nap = keylabel(lastkey).

         IF LOOKUP(nap,poisnap) > 0 THEN DO:

            IF INPUT lcCLI > "" AND FRAME-FIELD = "lcCLI" THEN DO:
               FIND FIRST MobSub WHERE
                          MobSub.CLI = INPUT lcCLI
               NO-LOCK NO-ERROR.

               IF NOT AVAIL MobSub THEN DO:
                  MESSAGE "Subscriber " lcCLI " not found, enter one which exists" VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

               ASSIGN lcCli = MobSub.CLI.

               lcList = lcList + (IF lcList > "" THEN "," ELSE "") +
                        MobSub.CLI + "=" + MobSub.ICC.
            END.
            ELSE IF INPUT liCustNum > "" AND FRAME-FIELD = "liCustNum" THEN DO:
               FIND FIRST Customer WHERE
                          Customer.CustNum = INPUT liCustNum
               NO-LOCK NO-ERROR.

               IF NOT AVAIL Customer THEN DO:
                  MESSAGE "Customer not found, enter one which exists" VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

               FIND FIRST MobSub WHERE
                          MobSub.CustNum = Customer.CustNum
               NO-LOCK NO-ERROR.

               IF NOT AVAIL MobSub THEN DO:
                  MESSAGE "No actibe subscription found, enter one which exists" VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

               ASSIGN liCustNum = MobSub.CustNum.

               FOR EACH MobSub WHERE
                        MobSub.CustNum = Customer.CustNum NO-LOCK:
                  lcList = lcList + (IF lcList > "" THEN "," ELSE "") +
                           MobSub.CLI + "=" + MobSub.ICC.
               END.
            END.
         END.
      APPLY LASTKEY.
      END.
      LEAVE.
   END.
END PROCEDURE.


PROCEDURE pAddRequestStructElement:
   DEF INPUT PARAMETER lcCLI      AS CHAR NO-UNDO.
   DEF INPUT PARAMETER liCustNum  AS INT  NO-UNDO.
   DEF INPUT PARAMETER lcList     AS CHAR NO-UNDO.

   DEF VAR lcMSISDNICCArray    AS CHAR NO-UNDO.
   DEF VAR lcMSISDNRangeArray  AS CHAR NO-UNDO.
   DEF VAR lcMSISDNICCStruct   AS CHAR NO-UNDO.
   DEF VAR lcMSISDNRangeStruct AS CHAR NO-UNDO.
   DEF VAR lcMSISDNICC         AS CHAR NO-UNDO.
   DEF VAR i                   AS INT  NO-UNDO.

   ASSIGN 
      liSeq     = NEXT-VALUE(M2MSeq)
      lcFormRequest = "004" + STRING(liSeq,"99999999")
      lcvalue = lcFormRequest + "00000000000D".

   /* Adjust time limits */
   ASSIGN
      ldeCreationTS = fMakeTS()
      liCurHour = TIME
      liMorning =  8 * 3600
      liMidday  = 14 * 3600
      liEvening = 20 * 3600.

   /* Adjust porting day  */
   IF liCurHour < liMidday THEN
      ldaNowDay = TODAY + 1.
   ELSE IF liCurHour < liEvening THEN
      ldaNowDay = TODAY + 2.

   ldePortTime = fHMS2TS(ldaNowDay,"02:00:00").

   IF lcCLI > "" THEN DO:
      FIND FIRST MobSub WHERE
                 MobSub.CLI = lcCLI NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN DO:
         MESSAGE "No actibe subscription found" VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.
      liCustNum = MobSub.CustNum.
   END.

   FIND FIRST Customer WHERE
              Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN DO:
      MESSAGE "No actibe customer found" VIEW-AS ALERT-BOX ERROR.
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
   add_timestamp(lcNotifyStruct, "fechaCreacion", fmakets()).
   add_boolean(lcNotifyStruct, "sincronizada", FALSE).
   add_string(lcNotifyStruct, "codigoNotificacion", lcInitInfo).

   /* Request Struct */
   lcRequestStruct = add_struct(lcNotifyStruct, "solicitud").

   add_timestamp(lcRequestStruct, "fechaCreacion",ldeCreationTS).
   add_timestamp(lcRequestStruct, "fechaEstado",ldeCreationTS).

   add_string(lcRequestStruct, "codigoReferencia", lcvalue).
   add_string(lcRequestStruct, "estado", "ASOL").
   add_timestamp(lcRequestStruct, "fechaVentanaCambio", ldePortTime).

   IF NUM-ENTRIES(lcList) = 1 THEN DO:
      add_string(lcRequestStruct, "MSISDN", lcCLI).
      add_string(lcRequestStruct, "ICCID", MobSub.ICC).
   END.
   ELSE DO:
      lcMSISDNRangeArray = add_array(lcRequestStruct, "rangoMSISDN").
      lcMSISDNICCArray = add_array(lcRequestStruct, "ICCIDRelativoMSISDN").
      DO i = 1 to NUM-ENTRIES(lcList):
         lcMSISDNICC = ENTRY(i,lcList).
         lcMSISDNICCStruct = add_struct(lcMSISDNICCArray, "").
         lcMSISDNRangeStruct = add_struct(lcMSISDNRangeArray, "").
         add_string(lcMSISDNRangeStruct, "valorInicial", ENTRY(1,lcMSISDNICC,"=")).
         add_string(lcMSISDNRangeStruct, "valorFinal", ENTRY(1,lcMSISDNICC,"=")).
         add_string(lcMSISDNICCStruct, "MSISDN", ENTRY(1,lcMSISDNICC,"=")).
         add_string(lcMSISDNICCStruct, "ICCID", ENTRY(2,lcMSISDNICC,"=")).
      END.
   END.

   add_string(lcRequestStruct, "codigoContrato", lcFormRequest).

   add_boolean(lcRequestStruct, "fechaVentanaCambioPorAbonado",FALSE ).
   add_string(lcRequestStruct, "NRNReceptor", "730000").
   add_string(lcRequestStruct, "codigoOperadorReceptor", "004").
   add_string(lcRequestStruct, "codigoOperadorDonante", "005").
   add_timestamp(lcRequestStruct, "fechaSolicitudPorAbonado", fMakeTS()).
   add_timestamp(lcRequestStruct, "fechaLimiteCambioEstado",
                 fHMS2TS(ldaNowDay - 1,"14:00:00")).
   add_boolean(lcRequestStruct, "operadorDonanteAltaExtraordinaria", FALSE).

   /* Subscription Struct */
   lcSubscriptionStruct = add_struct(lcRequestStruct, "abonado").

   /* IdData Struct */
   lcIdDataStruct = add_struct(lcSubscriptionStruct, "documentoIdentificacion").
   add_string(lcIdDataStruct,"tipo", Customer.CustIdType).
   add_string(lcIdDataStruct,"documento", Customer.OrgId).

   
   lcPersondata = add_struct(lcSubscriptionStruct, "datosPersonales").
   add_string(lcPersondata, "nombre", Customer.FirstName).
   add_string(lcPersondata, "primerApellido", Customer.CustName).
   IF Customer.CustIdType EQ "CIF" THEN
      add_string(lcPersondata, "razonSocial", Customer.CompanyName).


   IF gi_xmlrpc_error NE 0 THEN DO:
   ocResponse = SUBST("ERROR: XML creation: &1", gc_xmlrpc_error).
   RETURN.
   END.

   RUN pRPCMethodCall("mnp.obtenerNotificacionesAltaPortabilidadMovilComoDonantePendientesConfirmarRechazar", TRUE).

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
