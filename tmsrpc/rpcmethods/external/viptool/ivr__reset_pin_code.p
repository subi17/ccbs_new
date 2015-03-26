/**
 * Change subscription PIN code and notify customer by SMS
 *
 * @input       string;mandatory;MSISDN
                string;mandatory;Subscription PIN code e.g. "0312"
 * @output      boolean;true
 * @Exceptions  1;Subscription not found
                2;Incorrect PIN length
                3;Only numeric PIN values are allowed
                4;Customer not found
                5;Subscription record is locked
                6;SMS not defined
                7;SMS creation failed
 */
{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.

/* Input parameters */
DEF VAR pcCLI AS CHAR NO-UNDO.
DEF VAR pcPIN AS CHAR NO-UNDO. 

{commpaa.i}
ASSIGN
   katun = "IVR_" + gbAuthLog.EndUserId.
   gcBrand = "1".

{eventval.i}
{fsmsreq.i}
{tmsconst.i}
{fgettxt.i}

DEF VAR liRequest AS INT NO-UNDO. 
DEF VAR lcSMSText AS CHAR NO-UNDO. 
DEF VAR lcResult AS CHAR NO-UNDO. 
DEF VAR lhMobSub AS HANDLE NO-UNDO.
DEF VAR ldeSMSStamp AS DEC NO-UNDO. 

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
   {lib/eventlog.i}
END.

IF validate_request(param_toplevel_id, "string,string") EQ ? THEN RETURN.
pcCLI = get_string(param_toplevel_id, "0").
pcPIN = get_string(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND mobsub NO-LOCK WHERE 
     mobsub.brand = gcBrand AND
     mobsub.cli = pcCli NO-ERROR.

IF NOT AVAILABLE mobsub THEN RETURN appl_err("Subscription not found").

IF LENGTH(pcPIN) NE 4 THEN RETURN appl_err("Incorrect PIN length").

INT(pcPIN) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
   RETURN appl_err("Only numeric PIN values are allowed").

FIND FIRST Customer WHERE
           Customer.Custnum = MobSub.Custnum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN appl_err("Customer not found").

lcSMSText = fGetSMSTxt(
                    "PINRESET",
                    TODAY,
                    Customer.Language,
                    OUTPUT ldeSMSStamp).

IF INDEX(lcSMSText,"#PIN") = 0 THEN RETURN appl_err("SMS not defined").
lcSMSText = REPLACE(lcSMSText, "#PIN", pcPIN).

DO TRANSACTION:
   
   FIND CURRENT MobSub EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF LOCKED MobSub THEN RETURN appl_err("Subscription record is locked").
    
   IF llDoEvent THEN DO:
      lhMobSub = BUFFER MobSub:HANDLE.
      RUN StarEventInitialize(lhMobSub).
      RUN StarEventSetOldBuffer(lhMobSub).
   END.
   
   Mobsub.IDCode = pcPIN.
   
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobSub). 

   liRequest = fSMSRequest(
               MobSub.MsSeq,
               9,       /* type=info */
               "free",  /* source of message */
               lcSMSText,
               ldeSMSStamp, 
               {&REQUEST_SOURCE_EXTERNAL_API},
               "", /* usercode */
               "",
               OUTPUT lcResult). 
   
   IF liRequest = 0 THEN UNDO, RETURN appl_err("SMS creation failed").
   
   RELEASE MobSub.

END.

add_boolean(response_toplevel_id,"",True).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
