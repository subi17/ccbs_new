/** 
 * RPC to test sending of SMS.
 * newton_sms_template_test.p
 * @input       clinmbr;string;mandatory;Number to send test SMS to
                smscontent;string;mandatory;Contains sms
                smsfrom;string;mandatory;Contains sms FROM value

 * @output      success;boolean;Result status of sending test SMS
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
{Func/fsendsms.i}
{Syst/tmsconst.i}
DEF VAR pcCLI        AS CHARACTER NO-UNDO.
DEF VAR pcSmsContent AS CHARACTER NO-UNDO.
DEF VAR pcSender     AS CHARACTER NO-UNDO.
DEF VAR pcReqList    AS CHARACTER NO-UNDO. 
DEF VAR liLoop       AS INTEGER   NO-UNDO.
DEF VAR pcStruct     AS CHARACTER NO-UNDO. 
DEF VAR lcReplaceTxt AS CHARACTER NO-UNDO. 

ASSIGN
   gcBrand      = "1"
   lcReplaceTxt = "y@yoigo.es" /* Replace value for #SENDER tag */
   pcSender     = "800622800".

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/*pcReqList = validate_request(pcStruct,
           "clinmbr!,smscontent!,smsfrom!").*/
pcReqList = validate_request(pcStruct,
           "clinmbr!,smscontent!").
IF gi_xmlrpc_error NE 0 THEN RETURN.
      
ASSIGN      
   pcCLI        = get_string(pcStruct, "clinmbr")
                  WHEN LOOKUP("clinmbr", pcReqList) > 0
   pcSmsContent = get_string(pcStruct, "smscontent")
                  WHEN LOOKUP("smscontent", pcReqList) > 0
   /*pcSender     = get_string(pcStruct, "smsfrom")
                  WHEN LOOKUP("smsfrom", pcReqList) > 0*/.

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND MobSub NO-LOCK WHERE
     MobSub.Brand EQ gcBrand AND
     Mobsub.CLI   EQ pcCLI NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN appl_err("Requested subscriber not found ").
IF INDEX(pcSmsContent,"#SENDER") > 0 THEN 
DO:
   pcSmsContent = REPLACE(pcSmsContent,"#SENDER",lcReplaceTxt).
END.   
/* Sending test SMS */
RUN pSendTestSMS IN THIS-PROCEDURE (MobSub.MsSeq,
                 pcSmsContent,    /* SMSText */
                 {&SMSTYPE_INFO}, /* SMSType */
                 pcSender,
                 "") NO-ERROR.
IF ERROR-STATUS:ERROR THEN
   add_boolean(response_toplevel_id,?,TRUE).
ELSE add_boolean(response_toplevel_id,?,FALSE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.

/* INTERNAL PROCEDURES */
PROCEDURE pSendTestSMS:
DEF INPUT PARAMETER iiMsSeq       AS INTEGER   NO-UNDO.
DEF INPUT PARAMETER icSMSText     AS CHARACTER NO-UNDO.
DEF INPUT PARAMETER iiSMSType     AS INTEGER   NO-UNDO.
DEF INPUT PARAMETER icSender      AS CHARACTER NO-UNDO.
DEF INPUT PARAMETER icExtraParams AS CHARACTER NO-UNDO.

DEF VAR ldReqStamp AS DECIMAL NO-UNDO.
DEF VAR liSMSType  AS INTEGER NO-UNDO.

DEF BUFFER bMobSub FOR MobSub.

   FIND FIRST bMobSub NO-LOCK WHERE
              bMobSub.MsSeq EQ iiMsSeq NO-ERROR.
   IF NOT AVAILABLE bMobSub THEN RETURN.

   FIND FIRST Customer NO-LOCK OF bMobSub NO-ERROR.
   IF NOT AVAILABLE Customer THEN RETURN.

   IF NOT icSMSText > "" THEN RETURN.

   /* send SMS */
   icSMSText = fGetSMSTxt(icSMSText,
                          TODAY,
                          Customer.Language,
                          OUTPUT ldReqStamp).

   IF iiSMSType EQ ? THEN iiSMSType = liSMSType.

   IF icSMSText > "" THEN 
   DO:
      fMakeSchedSMS2(bMobSub.CustNum,
                     bMobSub.CLI,
                     iiSMSType,
                     icSMSText,
                     ldReqStamp,
                     icSender,
                     "").
   END. /* IF icSMSText > "" THEN DO: */
END PROCEDURE. /* PROCEDURE pSendTestSMS: */